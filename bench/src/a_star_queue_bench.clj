(ns a-star-queue-bench
  (:require [clojure.data.csv :as csv]
            [clojure.java.io :as io]
            [criterium.core :as criterium]
            [editscript.core :as editscript]
            [editscript.util.pairing :as pairing])
  (:import [clojure.lang IPersistentCollection IPersistentMap IPersistentStack]
           [editscript.util.pairing HeapNode]))

;; Exact pre-optimization queue behavior, retained only as a benchmark baseline.
(deftype LegacyPriorityMap [^:unsynchronized-mutable ^HeapNode heap
                            ^:unsynchronized-mutable entries]
  IPersistentCollection
  (count [_] (count entries))
  (cons [this entry]
    (let [[item priority] entry]
      (set! entries (assoc entries item priority))
      (set! heap (pairing/insert heap item priority))
      this))
  (empty [this]
    (set! heap nil)
    (set! entries {})
    this)
  (equiv [this other] (identical? this other))

  IPersistentMap
  (assoc [this item priority]
    (set! entries (assoc entries item priority))
    (set! heap (pairing/insert heap item priority))
    this)
  (hashCode [_] (hash entries))
  (equals [this other] (identical? this other))
  (containsKey [_ item] (contains? entries item))
  (entryAt [_ item] (find entries item))
  (seq [_] (seq entries))
  (without [this item]
    (set! entries (dissoc entries item))
    this)

  IPersistentStack
  (peek [_] [(.-item heap) (.-priority heap)])
  (pop [this]
    (let [next-heap (pairing/two-pass (pairing/get-left heap))]
      (set! entries (dissoc entries (.-item heap)))
      (set! heap next-heap)
      this)))

(defn legacy-priority-map
  ([] (->LegacyPriorityMap nil {}))
  ([& keyvals]
   {:pre [(even? (count keyvals))]}
   (reduce conj (legacy-priority-map) (partition 2 keyvals))))

(def optimized-priority-map pairing/priority-map)

(defn- read-drawing
  [number]
  (-> (str "../resources/drawing" number ".edn")
      slurp
      read-string))

(def drawing1 (delay (read-drawing 1)))
(def drawing4 (delay (read-drawing 4)))

(defn- with-queue
  [factory f]
  (with-redefs-fn {#'pairing/priority-map factory} f))

(defn- run-diff
  [factory origin target]
  (with-queue factory #(editscript/diff origin target)))

(defn- expansion-count
  [factory origin target]
  (let [frontier-var (ns-resolve 'editscript.diff.a-star 'frontier)
        frontier     @frontier-var
        expansions   (volatile! 0)]
    (with-redefs-fn
      {#'pairing/priority-map factory
       frontier-var (fn [type init end cur]
                      (vswap! expansions inc)
                      (frontier type init end cur))}
      #(do (editscript/diff origin target)
           @expansions))))

(defn- reprioritized-drain-count
  [factory item-count]
  (let [queue (reduce (fn [queue item]
                        (assoc queue item (inc (* 2 item))))
                      (factory)
                      (range item-count))
        queue (reduce (fn [queue item]
                        (assoc queue item (* 2 item)))
                      queue
                      (range item-count))]
    (loop [queue queue
           pops  0]
      (if (empty? queue)
        pops
        (recur (pop queue) (inc pops))))))

(defn- quick-mean-us
  [f]
  (-> (criterium/quick-benchmark (f) {})
      :mean
      first
      (* 1000000.0)))

(defn- verify-diff!
  [name factory origin target]
  (let [script (run-diff factory origin target)]
    (when-not (= target (editscript/patch origin script))
      (throw (ex-info "A* benchmark failed its round trip"
                      {:workload name})))
    script))

(defn- benchmark-diff
  [name origin target]
  (let [legacy-script  (verify-diff! name legacy-priority-map origin target)
        optimized-script (verify-diff! name optimized-priority-map
                                       origin target)]
    (when-not (= (editscript/get-size legacy-script)
                 (editscript/get-size optimized-script))
      (throw (ex-info "A* queue variants produced different script costs"
                      {:workload name}))))
  (let [legacy-work    (expansion-count legacy-priority-map origin target)
        optimized-work (expansion-count optimized-priority-map origin target)
        legacy-us      (with-queue legacy-priority-map
                         #(quick-mean-us
                            (fn [] (editscript/diff origin target))))
        optimized-us   (with-queue optimized-priority-map
                         #(quick-mean-us
                            (fn [] (editscript/diff origin target))))]
    {:workload       name
     :legacy-us      legacy-us
     :optimized-us   optimized-us
     :speedup        (/ legacy-us optimized-us)
     :legacy-work    legacy-work
     :optimized-work optimized-work
     :work-unit      "state expansions"}))

(defn- benchmark-reprioritization
  [name item-count]
  (let [legacy-work    (reprioritized-drain-count legacy-priority-map item-count)
        optimized-work (reprioritized-drain-count optimized-priority-map
                                                   item-count)]
    (when-not (= item-count optimized-work)
      (throw (ex-info "Stale entries escaped from the optimized queue"
                      {:expected item-count :actual optimized-work})))
    (when-not (> legacy-work optimized-work)
      (throw (ex-info "Reprioritization workload did not create stale entries"
                      {:legacy legacy-work :optimized optimized-work})))
    (let [legacy-us    (quick-mean-us
                         #(reprioritized-drain-count legacy-priority-map
                                                    item-count))
          optimized-us (quick-mean-us
                         #(reprioritized-drain-count optimized-priority-map
                                                    item-count))]
      {:workload       name
       :legacy-us      legacy-us
       :optimized-us   optimized-us
       :speedup        (/ legacy-us optimized-us)
       :legacy-work    legacy-work
       :optimized-work optimized-work
       :work-unit      "visible pops"})))

(def workloads
  {"drawing-1-to-4" #(benchmark-diff "drawing-1-to-4"
                                      @drawing1 @drawing4)
   "drawing-4-to-1" #(benchmark-diff "drawing-4-to-1"
                                      @drawing4 @drawing1)
   "reprioritize-1000" #(benchmark-reprioritization
                           "reprioritize-1000" 1000)})

(defn- format-row
  [{:keys [workload legacy-us optimized-us speedup
           legacy-work optimized-work work-unit]}]
  [workload
   (format "%.3f" legacy-us)
   (format "%.3f" optimized-us)
   (format "%.2f" speedup)
   legacy-work
   optimized-work
   work-unit])

(defn- write-results!
  [results]
  (with-open [writer (io/writer "a-star-queue-time.csv")]
    (csv/write-csv
      writer
      (cons ["Workload" "Legacy (us)" "Stale-pruned (us)" "Speedup"
             "Legacy work" "Stale-pruned work" "Work unit"]
            (map format-row results)))))

(defn -main
  [& selected]
  (let [names (if (seq selected) selected (keys workloads))
        unknown (remove workloads names)]
    (when (seq unknown)
      (throw (ex-info "Unknown A* queue workload"
                      {:unknown unknown :available (keys workloads)})))
    (let [results (mapv (fn [name]
                          (println "Benchmarking" name)
                          ((workloads name)))
                        names)]
      (doseq [result results]
        (println (zipmap [:workload :legacy-us :stale-pruned-us :speedup
                          :legacy-work :stale-pruned-work :work-unit]
                         (format-row result))))
      (write-results! results))))

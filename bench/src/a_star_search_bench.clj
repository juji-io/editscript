(ns a-star-search-bench
  (:require [clojure.data.csv :as csv]
            [clojure.java.io :as io]
            [criterium.core :as criterium]
            [editscript.core :as editscript]
            [editscript.util.index :as index]))

(defn- read-drawing
  [number]
  (-> (str "../resources/drawing" number ".edn")
      slurp
      read-string))

(def drawing1 (delay (read-drawing 1)))
(def drawing4 (delay (read-drawing 4)))

(def nested-a
  (vec (for [n (range 12)]
         {:id n
          :payload [[n :left] {:value n :enabled true}]})))

(def nested-b
  (vec (for [n (range 12)]
         [[(+ 100 n) :right]
          {:value (* n n) :enabled false}
          [(- n) (* 2 n) (* 3 n)]])))

(def overlapping-a
  (vec (for [n (range 12)]
         {:id n
          :payload [[n :left] {:value n :enabled true}]})))

(def overlapping-b
  (vec (for [n (range 12)]
         {:key (+ 100 n)
          :payload [[(- n) :right] {:value (* n n) :enabled false}]})))

(def workloads
  {"drawing-1-to-4" [drawing1 drawing4]
   "nested-replacement" [(delay nested-a) (delay nested-b)]
   "nested-overlap" [(delay overlapping-a) (delay overlapping-b)]})

(defn- quick-mean-us
  [f]
  (-> (criterium/quick-benchmark (f) {})
      :mean
      first
      (* 1000000.0)))

(defn- search-counts
  [origin target]
  (let [diff-var       (ns-resolve 'editscript.diff.a-star 'diff*)
        compute-var    (ns-resolve 'editscript.diff.a-star 'compute-diff)
        frontier-var   (ns-resolve 'editscript.diff.a-star 'frontier)
        original-diff  @diff-var
        original-compute (when compute-var @compute-var)
        original-frontier @frontier-var
        calls          (volatile! 0)
        pairs          (volatile! #{})
        computations   (volatile! 0)
        expansions     (volatile! 0)
        diff-wrapper   (fn ^long [a b came opts]
                         (vswap! calls inc)
                         (vswap! pairs conj [(index/get-order a)
                                             (index/get-order b)])
                         (original-diff a b came opts))
        compute-wrapper (when compute-var
                          (fn ^long [a b came opts]
                            (vswap! computations inc)
                            (original-compute a b came opts)))
        frontier-wrapper (fn [type init end cur]
                           (vswap! expansions inc)
                           (original-frontier type init end cur))
        bindings       (cond-> {diff-var diff-wrapper
                                frontier-var frontier-wrapper}
                         compute-var (assoc compute-var compute-wrapper))
        script         (with-redefs-fn bindings
                         #(editscript/diff origin target))]
    (when-not (= target (editscript/patch origin script))
      (throw (ex-info "A* search benchmark failed its round trip" {})))
    {:calls        @calls
     :unique-pairs (count @pairs)
     :computations (if compute-var @computations @calls)
     :expansions   @expansions
     :script-cost  (editscript/get-size script)}))

(defn- benchmark-workload
  [name [origin-delay target-delay]]
  (let [origin @origin-delay
        target @target-delay
        counts (search-counts origin target)
        mean-us (quick-mean-us #(editscript/diff origin target))]
    (assoc counts :workload name :mean-us mean-us)))

(defn- format-row
  [{:keys [workload mean-us expansions calls unique-pairs computations
           script-cost]}]
  [workload
   (format "%.3f" mean-us)
   expansions
   calls
   unique-pairs
   computations
   (- calls computations)
   script-cost])

(defn- write-results!
  [output results]
  (with-open [writer (io/writer output)]
    (csv/write-csv
      writer
      (cons ["Workload" "Mean (us)" "State expansions" "Cost calls"
             "Unique pairs" "Actual computations" "Memo hits" "Script cost"]
            (map format-row results)))))

(defn -main
  [& [output]]
  (let [output  (or output "a-star-search-time.csv")
        results (mapv (fn [[name workload]]
                        (println "Benchmarking" name)
                        (benchmark-workload name workload))
                      workloads)]
    (doseq [result results]
      (println (zipmap [:workload :mean-us :expansions :cost-calls
                        :unique-pairs :computations :memo-hits :script-cost]
                       (format-row result))))
    (write-results! output results)))

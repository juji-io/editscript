(ns editscript.bench.pairing-two-pass
  (:require [clojure.string :as string]
            [editscript.util.pairing :as pairing]))

;; Exact pre-optimization implementation retained as the benchmark control.
(defn- recursive-two-pass
  [node]
  (if (or (nil? node) (nil? (pairing/get-right node)))
    node
    (let [a node
          b (pairing/get-right node)
          n (pairing/get-right b)]
      (pairing/set-right a nil)
      (pairing/set-right b nil)
      (pairing/merge-nodes
        (pairing/merge-nodes a b)
        (recursive-two-pass n)))))

(def implementations
  [["recursive" recursive-two-pass]
   ["iterative" pairing/two-pass]])

(def workloads
  [{:workload "paired-2048" :node-count 2049 :repetitions 128}
   {:workload "stack-pressure-100000" :node-count 100001 :repetitions 1}])

(def sample-count 9)

(def node-performance
  (aget (js/require "perf_hooks") "performance"))

(defn- now-ms
  []
  (.call (aget node-performance "now") node-performance))

(defn- wide-heap
  [node-count]
  (reduce (fn [heap item]
            (pairing/insert heap item item))
          nil
          (range node-count)))

(defn- child-roots
  [node-count repetitions]
  (mapv (fn [_]
          (pairing/get-left (wide-heap node-count)))
        (range repetitions)))

(defn- merge-roots
  [two-pass roots]
  (loop [index    0
         checksum 0]
    (if (< index (count roots))
      (let [root (two-pass (nth roots index))]
        (recur (inc index) (+ checksum (.-item root))))
      checksum)))

(defn- timed-sample
  [two-pass node-count repetitions]
  (let [roots (child-roots node-count repetitions)
        start (now-ms)]
    (try
      (let [checksum (merge-roots two-pass roots)]
        {:elapsed  (- (now-ms) start)
         :checksum checksum})
      (catch :default error
        {:error (or (.-name error) "error")}))))

(defn- median
  [values]
  (nth (vec (sort values)) (quot (count values) 2)))

(defn- benchmark-implementation
  [{:keys [workload node-count repetitions]} [implementation two-pass]]
  ;; Warm with a bounded tree so the recursive control can also reach its
  ;; optimized JavaScript tier before the stack-pressure sample.
  (merge-roots two-pass (child-roots (min node-count 2049) 2))
  (loop [sample    0
         elapsed  []
         checksum nil]
    (if (= sample sample-count)
      (let [median-ms (median elapsed)]
        {:workload       workload
         :implementation implementation
         :node-count     node-count
         :repetitions    repetitions
         :median-ms      median-ms
         :ns-per-node    (/ (* median-ms 1000000)
                            (* (dec node-count) repetitions))
         :checksum       checksum
         :status         "ok"})
      (let [{:keys [error] :as result}
            (timed-sample two-pass node-count repetitions)]
        (if error
          {:workload       workload
           :implementation implementation
           :node-count     node-count
           :repetitions    repetitions
           :status         error}
          (let [expected repetitions]
            (when-not (= expected (:checksum result))
              (throw (js/Error. (str "Pairing heap checksum changed for "
                                     workload))))
            (recur (inc sample)
                   (conj elapsed (:elapsed result))
                   (:checksum result))))))))

(defn- drain-in-order?
  [two-pass node-count]
  (loop [node     (wide-heap node-count)
         expected 0]
    (if node
      (and (= expected (.-item node))
           (recur (two-pass (pairing/get-left node)) (inc expected)))
      (= expected node-count))))

(defn- validate-implementations!
  []
  (doseq [[implementation two-pass] implementations]
    (when-not (drain-in-order? two-pass 2048)
      (throw (js/Error. (str implementation
                             " two-pass merge violated heap order"))))))

(defn- add-speedups
  [results]
  (let [baselines (into {}
                        (keep (fn [{:keys [workload implementation median-ms]}]
                                (when (and (= implementation "recursive")
                                           median-ms)
                                  [workload median-ms])))
                        results)]
    (mapv (fn [{:keys [workload median-ms] :as result}]
            (if-let [baseline (and median-ms (get baselines workload))]
              (assoc result :speedup (/ baseline median-ms))
              result))
          results)))

(defn- fixed
  [value digits]
  (when (some? value)
    (.toFixed value digits)))

(defn- result-csv
  [results]
  (str
    "Workload,Implementation,Heap nodes,Repetitions,Median (ms),Nanoseconds per node,Speedup,Status,Checksum\n"
    (string/join
      "\n"
      (map (fn [{:keys [workload implementation node-count repetitions
                         median-ms ns-per-node speedup status checksum]}]
             (string/join ","
                          [workload implementation node-count repetitions
                           (or (fixed median-ms 3) "")
                           (or (fixed ns-per-node 3) "")
                           (or (fixed speedup 3) "")
                           status (or checksum "")]))
           results))
    "\n"))

(defn -main
  [& [output]]
  (let [output  (or output "pairing-two-pass-time.csv")
        fs      (js/require "fs")
        _       (validate-implementations!)
        results (->> workloads
                     (mapcat (fn [workload]
                               (map #(benchmark-implementation workload %)
                                    implementations)))
                     vec
                     add-speedups)]
    (doseq [{:keys [workload implementation median-ms ns-per-node speedup
                    status checksum]} results]
      (println workload implementation
               (if median-ms (str (fixed median-ms 3) " ms,") "")
               (if ns-per-node
                 (str (fixed ns-per-node 3) " ns/node,") "")
               (if speedup (str (fixed speedup 2) "x,") "")
               status
               (if checksum (str "checksum " checksum) "")))
    (.call (aget fs "writeFileSync") fs
           output (result-csv results) "utf8")))

(set! *main-cli-fn* -main)

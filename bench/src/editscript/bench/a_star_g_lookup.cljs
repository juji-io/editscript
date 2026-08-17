(ns editscript.bench.a-star-g-lookup
  (:require [clojure.string :as string]
            [goog.math.Long :refer [getMaxValue]]))

(def scores
  (into {}
        (map (fn [key]
               [key (mod key 97)]))
        (range 1024)))

(def iterations 5000000)
(def sample-count 7)

(defn- legacy-access-g
  [g cur]
  (get g cur (getMaxValue)))

(defn- native-access-g
  [g cur]
  (get g cur js/Number.POSITIVE_INFINITY))

(def implementations
  [["legacy-goog-long" legacy-access-g]
   ["native-infinity" native-access-g]])

(def workloads
  [{:workload "present" :key-mask 1023}
   {:workload "half-missing" :key-mask 2047}])

(defn- lookup-loop
  [access-g key-mask iteration-count]
  (loop [iteration 0
         rejected  0]
    (if (< iteration iteration-count)
      (let [key        (bit-and iteration key-mask)
            tentative (bit-and iteration 127)]
        (recur (inc iteration)
               (if (>= tentative (access-g scores key))
                 (inc rejected)
                 rejected)))
      rejected)))

(defn- elapsed-ms
  [f]
  (let [start  (js/Date.now)
        result (f)]
    {:elapsed (- (js/Date.now) start)
     :result  result}))

(defn- median
  [values]
  (nth (vec (sort values)) (quot (count values) 2)))

(defn- benchmark-pair
  [{:keys [workload key-mask]}]
  (let [expected (lookup-loop native-access-g key-mask iterations)]
    (doseq [[_ access-g] implementations]
      (when-not (= expected (lookup-loop access-g key-mask iterations))
        (throw (js/Error. (str "A* g lookup implementations disagree for "
                               workload)))))
    ;; Warm both controls before alternating measured samples.
    (doseq [[_ access-g] implementations]
      (lookup-loop access-g key-mask iterations))
    (let [samples
          (reduce
            (fn [samples _]
              (reduce
                (fn [samples [implementation access-g]]
                  (let [{:keys [elapsed result]}
                        (elapsed-ms
                          #(lookup-loop access-g key-mask iterations))]
                    (when-not (= expected result)
                      (throw (js/Error. "A* g lookup checksum changed")))
                    (update samples implementation conj elapsed)))
                samples
                implementations))
            (zipmap (map first implementations) (repeat []))
            (range sample-count))
          rows
          (mapv
            (fn [[implementation _]]
              (let [elapsed (median (get samples implementation))]
                {:workload      workload
                 :implementation implementation
                 :elapsed-ms    elapsed
                 :ns-per-lookup (/ (* elapsed 1000000) iterations)
                 :checksum      expected}))
            implementations)
          baseline (:ns-per-lookup (first rows))]
      (mapv #(assoc % :speedup (/ baseline (:ns-per-lookup %))) rows))))

(defn- fixed
  [value digits]
  (.toFixed value digits))

(defn- result-csv
  [results]
  (str
    "Workload,Sentinel,Lookups,Median (ms),Nanoseconds per lookup,Speedup,Checksum\n"
    (string/join
      "\n"
      (map (fn [{:keys [workload implementation elapsed-ms ns-per-lookup
                         speedup checksum]}]
             (string/join ","
                          [workload implementation iterations elapsed-ms
                           (fixed ns-per-lookup 3) (fixed speedup 3) checksum]))
           results))
    "\n"))

(defn -main
  [& [output]]
  (let [output  (or output "a-star-g-lookup-time.csv")
        results (into [] cat (map benchmark-pair workloads))
        fs      (js/require "fs")]
    (doseq [{:keys [workload implementation elapsed-ms ns-per-lookup
                    speedup checksum]} results]
      (println workload implementation
               (str elapsed-ms " ms,")
               (str (fixed ns-per-lookup 3) " ns/lookup,")
               (str (fixed speedup 2) "x,")
               "checksum" checksum))
    (.call (aget fs "writeFileSync") fs
           output (result-csv results) "utf8")))

(set! *main-cli-fn* -main)

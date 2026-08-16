(ns node-count-bench
  (:require [clojure.data.csv :as csv]
            [clojure.java.io :as io]
            [criterium.core :as criterium]
            [editscript.core :as editscript]
            [editscript.patch :as patch]
            [editscript.util.index :as index]))

(def shared-subtree
  {:payload  (vec (range 32))
   :metadata {:enabled true :labels [:a :b :c :d]}})

(def shared-origin
  (vec (repeat 2000 shared-subtree)))

(defn- unique-subtree
  [n]
  {:payload  (mapv #(+ n %) (range 32))
   :metadata {:enabled (even? n) :labels [n (inc n) (+ n 2) (+ n 3)]}})

(def unique-origin
  (mapv unique-subtree (range 2000)))

(def deletion-indexes
  (range 0 2000 4))

(defn- deletion-script
  []
  (editscript/edits->script
    (mapv (fn [index] [[index] :-]) deletion-indexes)))

(def shared-script (deletion-script))
(def unique-script (deletion-script))

(defn- index-data-nodes
  [data]
  (index/get-size (index/index data)))

(defn- get-data
  [data path]
  (reduce patch/vget data path))

(defn- index-change-ratio
  [origin editscript]
  (double
    (/ (reduce
         (fn [sum [path op value]]
           (+ sum (case op
                    (:r :+) (index-data-nodes value)
                    :s      1
                    :-      (index-data-nodes (get-data origin path)))))
         0
         (editscript/get-edits editscript))
       (index-data-nodes origin))))

(def workloads
  {"count-shared-direct"
   {:implementation "direct" :f #(editscript/data-nodes shared-origin)}

   "count-shared-index"
   {:implementation "index" :f #(index-data-nodes shared-origin)}

   "count-unique-direct"
   {:implementation "direct" :f #(editscript/data-nodes unique-origin)}

   "count-unique-index"
   {:implementation "index" :f #(index-data-nodes unique-origin)}

   "ratio-shared-direct"
   {:implementation "direct" :f #(editscript/change-ratio
                                    shared-origin shared-script)}

   "ratio-shared-index"
   {:implementation "index" :f #(index-change-ratio
                                   shared-origin shared-script)}

   "ratio-unique-direct"
   {:implementation "direct" :f #(editscript/change-ratio
                                    unique-origin unique-script)}

   "ratio-unique-index"
   {:implementation "index" :f #(index-change-ratio
                                   unique-origin unique-script)}})

(defn- quick-mean-us
  [f]
  (-> (criterium/quick-benchmark (f) {})
      :mean
      first
      (* 1000000.0)))

(defn- benchmark-workload
  [[name {:keys [implementation f]}]]
  (println "Benchmarking" name)
  {:workload       name
   :implementation implementation
   :mean-us        (quick-mean-us f)
   :result         (f)})

(defn- write-results!
  [output results]
  (with-open [writer (io/writer output)]
    (csv/write-csv
      writer
      (cons ["Workload" "Implementation" "Mean (us)" "Result"]
            (map (fn [{:keys [workload implementation mean-us result]}]
                   [workload implementation (format "%.3f" mean-us) result])
                 results)))))

(defn- validate-workloads!
  []
  (doseq [data [shared-origin unique-origin]]
    (when-not (= (index-data-nodes data)
                 (editscript/data-nodes data))
      (throw (ex-info "Node-count benchmark implementations disagree" {}))))
  (doseq [[origin script] [[shared-origin shared-script]
                           [unique-origin unique-script]]]
    (when-not (= (index-change-ratio origin script)
                 (editscript/change-ratio origin script))
      (throw (ex-info "Change-ratio benchmark implementations disagree" {})))))

(defn -main
  [& [output & selected]]
  (let [output   (or output "node-count-time.csv")
        selected (if (seq selected) (set selected) (set (keys workloads)))
        unknown  (seq (remove workloads selected))]
    (when unknown
      (throw (ex-info "Unknown node-count benchmark workload"
                      {:unknown unknown
                       :available (sort (keys workloads))})))
    (validate-workloads!)
    (let [results (->> workloads
                       (filter (comp selected key))
                       (sort-by key)
                       (mapv benchmark-workload))]
      (doseq [{:keys [workload implementation mean-us result]} results]
        (println workload implementation (format "%.3f us," mean-us) result))
      (write-results! output results))))

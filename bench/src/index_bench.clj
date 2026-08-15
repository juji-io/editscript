(ns index-bench
  (:require [clojure.data.csv :as csv]
            [clojure.java.io :as io]
            [criterium.core :as criterium]
            [editscript.core :as editscript]
            [editscript.util.index :as index]))

(def shared-subtree
  {:payload [0 1 2 3]
   :metadata {:enabled true :label "shared"}})

(def shared-origin
  (vec (repeat 2000 shared-subtree)))

(def shared-target
  (assoc shared-origin
         1000
         (assoc shared-subtree :payload [0 1 :changed 3])))

(def workloads
  {"index-one-shared-2000"
   #(index/index shared-origin)

   "index-both-shared-2000"
   #(vector (index/index shared-origin)
            (index/index shared-target))

   "a-star-shared-2000"
   #(editscript/diff shared-origin shared-target {:algo :a-star})})

(defn- quick-mean-us
  [f]
  (-> (criterium/quick-benchmark (f) {})
      :mean
      first
      (* 1000000.0)))

(defn- benchmark-workload
  [[name f]]
  (println "Benchmarking" name)
  {:workload name
   :mean-us (quick-mean-us f)})

(defn- write-results!
  [output results]
  (with-open [writer (io/writer output)]
    (csv/write-csv
      writer
      (cons ["Workload" "Mean (us)"]
            (map (fn [{:keys [workload mean-us]}]
                   [workload (format "%.3f" mean-us)])
                 results)))))

(defn- validate-workload!
  []
  (let [script (editscript/diff shared-origin shared-target {:algo :a-star})]
    (when-not (= shared-target (editscript/patch shared-origin script))
      (throw (ex-info "A* indexing benchmark failed its round trip" {})))))

(defn -main
  [& [output & selected]]
  (let [output    (or output "index-time.csv")
        selected  (if (seq selected) (set selected) (set (keys workloads)))
        unknown   (seq (remove workloads selected))]
    (when unknown
      (throw (ex-info "Unknown index benchmark workload"
                      {:unknown unknown
                       :available (sort (keys workloads))})))
    (validate-workload!)
    (let [results (->> workloads
                       (filter (comp selected key))
                       (sort-by key)
                       (mapv benchmark-workload))]
      (doseq [{:keys [workload mean-us]} results]
        (println workload (format "%.3f us" mean-us)))
      (write-results! output results))))

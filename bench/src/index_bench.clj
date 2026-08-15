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

   "index-both-shared-context-2000"
   #(let [context (index/index-context)]
      (vector (index/index shared-origin context)
              (index/index shared-target context)))

   "a-star-shared-2000"
   #(editscript/diff shared-origin shared-target {:algo :a-star})})

(def workload-node-totals
  {"index-one-shared-2000"
   (delay (editscript/data-nodes shared-origin))

   "index-both-shared-2000"
   (delay (+ (editscript/data-nodes shared-origin)
             (editscript/data-nodes shared-target)))

   "index-both-shared-context-2000"
   (delay (+ (editscript/data-nodes shared-origin)
             (editscript/data-nodes shared-target)))

   "a-star-shared-2000"
   (delay (+ (editscript/data-nodes shared-origin)
             (editscript/data-nodes shared-target)))})

(defn- quick-mean-us
  [f]
  (-> (criterium/quick-benchmark (f) {})
      :mean
      first
      (* 1000000.0)))

(defn- created-node-count
  [f]
  (let [make-var (ns-resolve 'editscript.util.index 'make-node)
        original @make-var
        created  (volatile! 0)
        wrapper  (fn [context key value parent start metadata]
                   (vswap! created inc)
                   (original context key value parent start metadata))]
    (with-redefs-fn {make-var wrapper} f)
    @created))

(defn- benchmark-workload
  [[name f]]
  (println "Benchmarking" name)
  (let [total-nodes   @(get workload-node-totals name)
        created-nodes (created-node-count f)]
    {:workload       name
     :mean-us        (quick-mean-us f)
     :created-nodes  created-nodes
     :total-nodes    total-nodes
     :created-ratio  (double (/ created-nodes total-nodes))}))

(defn- write-results!
  [output results]
  (with-open [writer (io/writer output)]
    (csv/write-csv
      writer
      (cons ["Workload" "Mean (us)" "Created nodes" "Total nodes"
             "Created (%)"]
            (map (fn [{:keys [workload mean-us created-nodes total-nodes
                              created-ratio]}]
                   [workload (format "%.3f" mean-us) created-nodes total-nodes
                    (format "%.3f" (* 100.0 created-ratio))])
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
      (doseq [{:keys [workload mean-us created-nodes total-nodes]} results]
        (println workload (format "%.3f us," mean-us)
                 created-nodes "of" total-nodes "nodes created"))
      (write-results! output results))))

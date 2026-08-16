(ns nada-bench
  (:require [clojure.data.csv :as csv]
            [clojure.java.io :as io]
            [criterium.core :as criterium]
            [editscript.core :as editscript]
            [editscript.edit :as edit])
  (:import [com.sun.management ThreadMXBean]
           [java.lang.management ManagementFactory]))

(def entry-counts [1000 10000 50000])

(defn- item
  [index]
  {:id index :payload [index (str "item-" index)]})

(def datasets
  (into {}
        (map (fn [entry-count]
               [entry-count
                {:origin (into {}
                               (map (fn [index] [index (item index)]))
                               (range entry-count))
                 :target (into {}
                               (map (fn [index] [index (item index)]))
                               (range entry-count (* 2 entry-count)))}]))
        entry-counts))

(defn- fresh-nada
  []
  (reify edit/IType
    (get-type [_] :nil)))

;; Capture the optimized function before benchmark-local with-redefs changes
;; the root of edit/nada.
(def singleton-nada edit/nada)

(def implementations
  {"legacy-fresh" fresh-nada
   "singleton"    singleton-nada})

(def workloads
  (into {}
        (mapcat
          (fn [[entry-count {:keys [origin target]}]]
            (map (fn [[implementation nada-fn]]
                   [(str "disjoint-" entry-count "-" implementation)
                    {:batch          (str "disjoint-" entry-count)
                     :implementation implementation
                     :entry-count    entry-count
                     :nada-fn        nada-fn
                     :f              #(editscript/diff
                                        origin target
                                        {:algo :quick :vec-timeout nil})}])
                 implementations))
          datasets)))

(defn- quick-mean-us
  [f]
  (-> (criterium/quick-benchmark (f) {})
      :mean
      first
      (* 1000000.0)))

(defn- allocated-bytes
  [f]
  (let [bean (ManagementFactory/getThreadMXBean)]
    (when (and (instance? ThreadMXBean bean)
               (.isThreadAllocatedMemorySupported ^ThreadMXBean bean))
      (let [^ThreadMXBean bean bean]
        (when-not (.isThreadAllocatedMemoryEnabled bean)
          (.setThreadAllocatedMemoryEnabled bean true))
        (let [thread-id (.getId (Thread/currentThread))
              before    (.getThreadAllocatedBytes bean thread-id)]
          (f)
          (- (.getThreadAllocatedBytes bean thread-id) before))))))

(defn- benchmark-workload
  [[name {:keys [batch implementation entry-count nada-fn f]}]]
  (println "Benchmarking" name)
  (with-redefs [edit/nada nada-fn]
    (let [script      (f)
          mean-us     (quick-mean-us f)
          allocations (keep (fn [_] (allocated-bytes f)) (range 5))]
      {:workload       name
       :batch          batch
       :implementation implementation
       :entry-count    entry-count
       :mean-us        mean-us
       :allocated      (when (seq allocations) (apply min allocations))
       :edit-distance  (edit/edit-distance script)})))

(defn- add-comparisons
  [results]
  (let [baselines (into {}
                        (keep (fn [{:keys [batch implementation mean-us
                                           allocated]}]
                                (when (= implementation "legacy-fresh")
                                  [batch {:mean-us mean-us
                                          :allocated allocated}])))
                        results)]
    (mapv (fn [{:keys [batch mean-us allocated] :as result}]
            (let [{baseline-us    :mean-us
                   baseline-bytes :allocated} (get baselines batch)]
              (assoc result
                     :speedup (when baseline-us (/ baseline-us mean-us))
                     :bytes-saved (when (and baseline-bytes allocated)
                                    (- baseline-bytes allocated)))))
          results)))

(defn- write-results!
  [output results]
  (with-open [writer (io/writer output)]
    (csv/write-csv
      writer
      (cons ["Workload" "Nada" "Entries per map" "Edits" "Mean (us)"
             "Speedup vs fresh" "Allocated bytes" "Bytes saved"]
            (map (fn [{:keys [workload implementation entry-count
                              edit-distance mean-us speedup allocated
                              bytes-saved]}]
                   [workload implementation entry-count edit-distance
                    (format "%.3f" mean-us)
                    (if speedup (format "%.3f" speedup) "")
                    (or allocated "") (or bytes-saved "")])
                 results)))))

(defn- diff-with-nada
  [nada-fn origin target]
  (with-redefs [edit/nada nada-fn]
    (editscript/diff origin target {:algo :quick :vec-timeout nil})))

(defn- validate-workloads!
  []
  (doseq [[entry-count {:keys [origin target]}] datasets]
    (let [legacy    (diff-with-nada fresh-nada origin target)
          singleton (diff-with-nada singleton-nada origin target)]
      (when-not (and (= (edit/get-edits legacy)
                        (edit/get-edits singleton))
                     (= target (editscript/patch origin legacy))
                     (= target (editscript/patch origin singleton)))
        (throw (ex-info "Nada benchmark implementations disagree"
                        {:entry-count entry-count}))))))

(defn -main
  [& [output & selected]]
  (let [output   (or output "nada-time.csv")
        selected (if (seq selected) (set selected) (set (keys workloads)))
        unknown  (seq (remove workloads selected))]
    (when unknown
      (throw (ex-info "Unknown nada benchmark workload"
                      {:unknown unknown
                       :available (sort (keys workloads))})))
    (validate-workloads!)
    (let [results (->> workloads
                       (filter (comp selected key))
                       (sort-by key)
                       (mapv benchmark-workload)
                       add-comparisons)]
      (doseq [{:keys [workload implementation mean-us speedup allocated
                      bytes-saved]} results]
        (println workload implementation (format "%.3f us" mean-us)
                 (if speedup (format "%.2fx" speedup) "")
                 (or allocated "n/a") "allocated bytes,"
                 (or bytes-saved "n/a") "saved"))
      (write-results! output results))))

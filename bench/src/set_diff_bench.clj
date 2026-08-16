(ns set-diff-bench
  (:require [clojure.data.csv :as csv]
            [clojure.java.io :as io]
            [clojure.set :as set]
            [criterium.core :as criterium]
            [editscript.core :as editscript]
            [editscript.diff.quick :as quick]
            [editscript.edit :as edit])
  (:import [com.sun.management ThreadMXBean]
           [java.lang.management ManagementFactory]))

(def workload-specs
  {"overlap-99pct-50000" {:set-size 50000 :change-count 500}
   "overlap-50pct-50000" {:set-size 50000 :change-count 25000}
   "disjoint-50000"      {:set-size 50000 :change-count 50000}})

(def datasets
  (into {}
        (map (fn [[name {:keys [set-size change-count] :as spec}]]
               [name
                (assoc spec
                       :origin (set (range set-size))
                       :target (into (set (range change-count set-size))
                                     (range set-size
                                            (+ set-size change-count))))]))
        workload-specs))

(defn- legacy-diff-set
  [script path a b opts]
  (doseq [value (set/difference a b)]
    (quick/diff* script (conj path value) value (edit/nada) opts))
  (doseq [value (set/difference b a)]
    (quick/diff* script (conj path value) (edit/nada) value opts)))

(def diff-set-var
  (or (ns-resolve 'editscript.diff.quick 'diff-set)
      (throw (ex-info "Cannot resolve quick diff-set" {}))))

(def contains-diff-set
  (var-get diff-set-var))

(def implementations
  {"legacy-difference" legacy-diff-set
   "contains"          contains-diff-set})

(def workloads
  (into {}
        (mapcat
          (fn [[batch {:keys [origin target set-size change-count]}]]
            (map (fn [[implementation diff-set-fn]]
                   [(str batch "-" implementation)
                    {:batch          batch
                     :implementation implementation
                     :set-size       set-size
                     :change-count   change-count
                     :diff-set-fn    diff-set-fn
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
  [[name {:keys [batch implementation set-size change-count diff-set-fn f]}]]
  (println "Benchmarking" name)
  (with-redefs-fn
    {diff-set-var diff-set-fn}
    (fn []
      (let [script      (f)
            mean-us     (quick-mean-us f)
            allocations (keep (fn [_] (allocated-bytes f)) (range 5))]
        {:workload       name
         :batch          batch
         :implementation implementation
         :set-size       set-size
         :change-count   change-count
         :mean-us        mean-us
         :allocated      (when (seq allocations) (apply min allocations))
         :edit-distance  (edit/edit-distance script)}))))

(defn- add-comparisons
  [results]
  (let [baselines (into {}
                        (keep (fn [{:keys [batch implementation mean-us
                                           allocated]}]
                                (when (= implementation "legacy-difference")
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
      (cons ["Workload" "Set traversal" "Set size" "Changed per side"
             "Edits" "Mean (us)" "Speedup vs difference" "Allocated bytes"
             "Bytes saved"]
            (map (fn [{:keys [workload implementation set-size change-count
                              edit-distance mean-us speedup allocated
                              bytes-saved]}]
                   [workload implementation set-size change-count edit-distance
                    (format "%.3f" mean-us)
                    (if speedup (format "%.3f" speedup) "")
                    (or allocated "") (or bytes-saved "")])
                 results)))))

(defn- diff-with
  [diff-set-fn origin target]
  (with-redefs-fn
    {diff-set-var diff-set-fn}
    #(editscript/diff origin target {:algo :quick :vec-timeout nil})))

(defn- validate-workloads!
  []
  (doseq [[batch {:keys [origin target]}] datasets]
    (let [legacy   (diff-with legacy-diff-set origin target)
          contains (diff-with contains-diff-set origin target)]
      (when-not (and (= (set (edit/get-edits legacy))
                        (set (edit/get-edits contains)))
                     (= (edit/get-size legacy) (edit/get-size contains))
                     (= target (editscript/patch origin legacy))
                     (= target (editscript/patch origin contains)))
        (throw (ex-info "Set benchmark implementations disagree"
                        {:workload batch}))))))

(defn -main
  [& [output & selected]]
  (let [output   (or output "set-diff-time.csv")
        selected (if (seq selected) (set selected) (set (keys workloads)))
        unknown  (seq (remove workloads selected))]
    (when unknown
      (throw (ex-info "Unknown set benchmark workload"
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

(ns patch-bench
  (:require [clojure.data.csv :as csv]
            [clojure.java.io :as io]
            [clojure.test.check.generators :as gen]
            [criterium.core :as criterium]
            [editscript.core :as core]
            [editscript.edit :as edit]
            [editscript.patch :as patch]))

(defn sequential-patch
  "The pre-batching implementation, retained as the benchmark baseline."
  [origin script]
  {:pre [(instance? editscript.edit.EditScript script)]}
  (reduce patch/patch* origin (edit/get-edits script)))

(def large-row-gen
  (gen/let [id      gen/int
            scores  (gen/vector gen/int 8 24)
            notes   (gen/vector gen/string 2 8)
            enabled gen/boolean]
    {:id id
     :scores scores
     :notes notes
     :enabled enabled}))

(def large-state-gen
  (gen/let [rows  (gen/vector large-row-gen 24 72)
            queue (gen/vector gen/int 24 72)
            flags (gen/vector gen/int 8 24)
            title gen/string]
    {:rows rows
     :queue (apply list queue)
     :flags (set flags)
     :title title}))

(def large-state-pair-gen
  (gen/tuple large-state-gen large-state-gen))

(defn generated-cases
  "Create reproducible property-generated patch workloads. Diffing happens once,
  outside the timed expressions."
  [case-count]
  (mapv
    (fn [seed]
      (let [[origin target] (gen/generate large-state-pair-gen 100 seed)]
        {:origin origin
         :target target
         :script (core/diff origin target {:algo :quick})}))
    (range 424242 (+ 424242 case-count))))

(defn prefix-delete-case
  [type size edit-count]
  (let [values (vec (range size))
        origin (case type
                 :vector values
                 :list   (apply list values))
        target (case type
                 :vector (into [] (subvec values edit-count))
                 :list   (apply list (subvec values edit-count)))
        script (edit/edits->script
                 (vec (repeat edit-count [[0] :-])))]
    {:origin origin :target target :script script}))

(defn single-nested-replace-case
  []
  (let [origin {:rows (vec (map (fn [i]
                                  {:id i :scores (vec (range 16))})
                                (range 100)))}
        target (assoc-in origin [:rows 50 :scores 8] :changed)]
    {:origin origin
     :target target
     :script (core/diff origin target {:algo :quick})}))

(defn workloads
  []
  {"single-nested-replace" [(single-nested-replace-case)]
   "generated-large"      (generated-cases 8)
   "vector-prefix-delete" [(prefix-delete-case :vector 10000 250)]
   "list-prefix-delete"   [(prefix-delete-case :list 10000 250)]})

(defn run-patches
  [patch-fn cases]
  (mapv (fn [{:keys [origin script]}]
          (patch-fn origin script))
        cases))

(defn verify-workload!
  [cases]
  (doseq [{:keys [origin target script]} cases]
    (assert (= target (sequential-patch origin script)))
    (assert (= target (core/patch origin script)))))

(defn mean-micros
  [result]
  (* 1000000.0 (-> result :mean first)))

(defn round-to
  [value places]
  (let [factor (Math/pow 10.0 places)]
    (/ (double (Math/round (* value factor))) factor)))

(defn benchmark-workload
  [name cases]
  (verify-workload! cases)
  (println "Benchmarking" name "with" (count cases) "case(s)")
  (let [sequential (criterium/quick-benchmark
                     (run-patches sequential-patch cases) {})
        batched    (criterium/quick-benchmark
                     (run-patches core/patch cases) {})
        old-us     (mean-micros sequential)
        new-us     (mean-micros batched)]
    {:workload name
     :sequential-us old-us
     :batched-us new-us
     :speedup (/ old-us new-us)}))

(defn write-results!
  [results]
  (with-open [writer (io/writer "patch-time.csv")]
    (csv/write-csv
      writer
      (cons ["Workload" "Sequential (us)" "Batched (us)" "Speedup"]
            (map (fn [{:keys [workload sequential-us batched-us speedup]}]
                   [workload
                    (round-to sequential-us 3)
                    (round-to batched-us 3)
                    (round-to speedup 2)])
                 results)))))

(defn -main
  [& selected]
  (let [all       (workloads)
        selected  (if (seq selected) (select-keys all selected) all)
        _         (assert (seq selected) "No matching patch workloads")
        results   (mapv (fn [[name cases]]
                          (benchmark-workload name cases))
                        selected)]
    (write-results! results)
    (doseq [{:keys [workload sequential-us batched-us speedup]} results]
      (printf "%-22s sequential %10.1f us, batched %10.1f us, %6.2fx\n"
              workload sequential-us batched-us speedup))))

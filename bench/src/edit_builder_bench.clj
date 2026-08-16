(ns edit-builder-bench
  (:require [clojure.data.csv :as csv]
            [clojure.java.io :as io]
            [criterium.core :as criterium]
            [editscript.edit :as edit]))

(def max-edit-count 50000)

(def commands
  (mapv (fn [index]
          (let [path [index :payload (mod index 11)]]
            (case (long (mod index 5))
              0 [:+ path false nil]
              1 [:+ path nil nil]
              2 [:- path nil nil]
              3 [:r path [index {:active (even? index)}
                             [nil false (inc index)]] nil]
              4 [:s path [2 [:+ "new"] [:- 1] [:r "value"]]
                 :character])))
        (range max-edit-count)))

(defn- append-command!
  [script [op path value level]]
  (case op
    :+ (edit/add-data script path value)
    :- (edit/delete-data script path)
    :r (edit/replace-data script path value)
    :s (edit/replace-str script path value level)))

(defn- append-commands!
  [script commands]
  (doseq [command commands]
    (append-command! script command))
  script)

(defn- public-script
  [commands]
  (append-commands! (edit/edits->script []) commands))

(defn- transient-script
  [commands]
  (-> (edit/edit-builder)
      (append-commands! commands)
      edit/persistent-script!))

(defn- transient-sized-script
  [commands]
  (let [script (transient-script commands)]
    (edit/get-size script)
    script))

(def batch-sizes [1000 10000 50000])

(def workloads
  (into {}
        (mapcat
          (fn [edit-count]
            (let [batch (subvec commands 0 edit-count)]
              [[(str "mixed-" edit-count "-public")
                {:batch          (str "mixed-" edit-count)
                 :implementation "public-auto-sized"
                 :edit-count     edit-count
                 :f              #(public-script batch)}]
               [(str "mixed-" edit-count "-transient")
                {:batch          (str "mixed-" edit-count)
                 :implementation "internal-transient"
                 :edit-count     edit-count
                 :f              #(transient-script batch)}]
               [(str "mixed-" edit-count "-transient-sized")
                {:batch          (str "mixed-" edit-count)
                 :implementation "internal-transient+size"
                 :edit-count     edit-count
                 :f              #(transient-sized-script batch)}]]))
          batch-sizes)))

(defn- script-metadata
  [script]
  [(edit/get-edits script)
   (edit/get-size script)
   (edit/get-adds-num script)
   (edit/get-dels-num script)
   (edit/get-reps-num script)
   (edit/edit-distance script)])

(defn- quick-mean-us
  [f]
  (-> (criterium/quick-benchmark (f) {})
      :mean
      first
      (* 1000000.0)))

(defn- benchmark-workload
  [[name {:keys [batch implementation edit-count f]}]]
  (println "Benchmarking" name)
  (let [script (f)]
    {:workload       name
     :batch          batch
     :implementation implementation
     :edit-count     edit-count
     :mean-us        (quick-mean-us f)
     :script-size    (edit/get-size script)
     :edit-distance  (edit/edit-distance script)}))

(defn- add-speedups
  [results]
  (let [public-means (into {}
                           (keep (fn [{:keys [batch implementation mean-us]}]
                                   (when (= implementation "public-auto-sized")
                                     [batch mean-us])))
                           results)]
    (mapv (fn [{:keys [batch mean-us] :as result}]
            (assoc result :speedup
                   (when-let [public-mean (get public-means batch)]
                     (/ public-mean mean-us))))
          results)))

(defn- write-results!
  [output results]
  (with-open [writer (io/writer output)]
    (csv/write-csv
      writer
      (cons ["Workload" "Builder" "Edits" "Mean (us)"
             "Speedup vs public" "Script size" "Edit distance"]
            (map (fn [{:keys [workload implementation edit-count mean-us
                              speedup script-size edit-distance]}]
                   [workload implementation edit-count (format "%.3f" mean-us)
                    (if speedup (format "%.3f" speedup) "")
                    script-size edit-distance])
                 results)))))

(defn- validate-workloads!
  []
  (doseq [edit-count batch-sizes]
    (let [batch     (subvec commands 0 edit-count)
          public    (public-script batch)
          transient (transient-script batch)]
      (when-not (= (script-metadata public)
                   (script-metadata transient))
        (throw (ex-info "Edit builder benchmark implementations disagree"
                        {:edit-count edit-count}))))))

(defn -main
  [& [output & selected]]
  (let [output   (or output "edit-builder-time.csv")
        selected (if (seq selected) (set selected) (set (keys workloads)))
        unknown  (seq (remove workloads selected))]
    (when unknown
      (throw (ex-info "Unknown edit builder benchmark workload"
                      {:unknown unknown
                       :available (sort (keys workloads))})))
    (validate-workloads!)
    (let [results (->> workloads
                       (filter (comp selected key))
                       (sort-by key)
                       (mapv benchmark-workload)
                       add-speedups)]
      (doseq [{:keys [workload implementation mean-us speedup]} results]
        (println workload implementation (format "%.3f us" mean-us)
                 (if speedup (format "%.2fx" speedup) "")))
      (write-results! output results))))

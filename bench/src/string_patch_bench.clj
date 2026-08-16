(ns string-patch-bench
  (:require [clojure.data.csv :as csv]
            [clojure.java.io :as io]
            [clojure.string :as string]
            [criterium.core :as criterium]
            [editscript.core :as editscript]
            [editscript.edit :as edit]
            [editscript.patch :as patch]
            [editscript.util.common :as common])
  (:import [com.sun.management ThreadMXBean]
           [java.lang.management ManagementFactory]))

(defn- legacy-segments
  [x edits level]
  (let [x  (common/transform-str x level)
        sf (if (= level :character) subs subvec)
        i  (volatile! 0)]
    (persistent!
      (reduce
        (fn [ss edit]
          (cond
            (integer? edit)
            (let [segment (sf x @i (+ ^long @i ^long edit))]
              (vswap! i (partial + edit))
              (conj! ss segment))

            (= (nth edit 0) :-)
            (do (vswap! i (partial + (nth edit 1))) ss)

            (= (nth edit 0) :r)
            (let [segment (nth edit 1)]
              (vswap! i (partial + (count segment)))
              (conj! ss segment))

            (= (nth edit 0) :+)
            (conj! ss (nth edit 1))))
        (transient [])
        edits))))

(defn- legacy-sreplace
  [x edits level]
  (let [ss (legacy-segments x edits level)]
    (case level
      :character (apply str ss)
      :word      (string/join " " (flatten ss))
      :line      (string/join "\n" (flatten ss)))))

(def sreplace-var
  (or (ns-resolve 'editscript.patch 'sreplace)
      (throw (ex-info "Cannot resolve patch sreplace" {}))))

(def builder-sreplace
  (var-get sreplace-var))

(defn- unit
  [level index]
  (case level
    :character (str (char (+ (int \a) (mod index 26))))
    :word      (str "word-" index)
    :line      (str "line-" index)))

(defn- changed-segment
  [level cycle prefix]
  (let [first-value  (str prefix "-" cycle "-a")
        second-value (str prefix "-" cycle "-b")]
    (if (= level :character)
      (str (char (+ (int \A) (mod cycle 26)))
           (char (+ (int \A) (mod (inc cycle) 26))))
      [first-value second-value])))

(defn- string-operation
  [level]
  (case level
    :character :s
    :word      :sw
    :line      :sl))

(defn- separator
  [level]
  (case level
    :character ""
    :word      " "
    :line      "\n"))

(defn- string-case
  [level cycles]
  (let [unit-count (* cycles 11)
        origin     (string/join (separator level)
                                (map #(unit level %) (range unit-count)))
        operations (into []
                         (mapcat (fn [cycle]
                                   [8
                                    [:r (changed-segment
                                          level cycle "replacement")]
                                    [:- 1]
                                    [:+ (changed-segment
                                          level cycle "addition")]]))
                         (range cycles))
        script     (edit/edits->script
                     [[[] (string-operation level) operations]])]
    {:origin origin :script script :segments (count operations)}))

(def cases
  {"character-20000" (string-case :character 20000)
   "word-5000"       (string-case :word 5000)
   "line-5000"       (string-case :line 5000)})

(def implementations
  {"legacy-flatten-apply" legacy-sreplace
   "segment-builder"      builder-sreplace})

(def workloads
  (into {}
        (mapcat
          (fn [[batch {:keys [origin script segments]}]]
            (map (fn [[implementation sreplace-fn]]
                   [(str batch "-" implementation)
                    {:batch          batch
                     :implementation implementation
                     :segments       segments
                     :sreplace-fn    sreplace-fn
                     :f              #(editscript/patch origin script)}])
                 implementations))
          cases)))

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
  [[name {:keys [batch implementation segments sreplace-fn f]}]]
  (println "Benchmarking" name)
  (with-redefs-fn
    {sreplace-var sreplace-fn}
    (fn []
      (let [result      (f)
            mean-us     (quick-mean-us f)
            allocations (keep (fn [_] (allocated-bytes f)) (range 5))]
        {:workload       name
         :batch          batch
         :implementation implementation
         :segments       segments
         :mean-us        mean-us
         :allocated      (when (seq allocations) (apply min allocations))
         :result-length  (count result)}))))

(defn- add-comparisons
  [results]
  (let [baselines (into {}
                        (keep (fn [{:keys [batch implementation mean-us
                                           allocated]}]
                                (when (= implementation "legacy-flatten-apply")
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
      (cons ["Workload" "String patch" "Edit tokens" "Result characters"
             "Mean (us)" "Speedup vs flatten/apply" "Allocated bytes"
             "Bytes saved"]
            (map (fn [{:keys [workload implementation segments result-length
                              mean-us speedup allocated bytes-saved]}]
                   [workload implementation segments result-length
                    (format "%.3f" mean-us)
                    (if speedup (format "%.3f" speedup) "")
                    (or allocated "") (or bytes-saved "")])
                 results)))))

(defn- patch-with
  [sreplace-fn origin script]
  (with-redefs-fn
    {sreplace-var sreplace-fn}
    #(editscript/patch origin script)))

(defn- validate-workloads!
  []
  (doseq [[batch {:keys [origin script]}] cases]
    (let [legacy  (patch-with legacy-sreplace origin script)
          builder (patch-with builder-sreplace origin script)]
      (when-not (= legacy builder)
        (throw (ex-info "String patch benchmark implementations disagree"
                        {:workload batch}))))))

(defn -main
  [& [output & selected]]
  (let [output   (or output "string-patch-time.csv")
        selected (if (seq selected) (set selected) (set (keys workloads)))
        unknown  (seq (remove workloads selected))]
    (when unknown
      (throw (ex-info "Unknown string patch benchmark workload"
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

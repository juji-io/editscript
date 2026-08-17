(ns wu-frontier-bench
  (:require [clojure.data.csv :as csv]
            [clojure.java.io :as io]
            [criterium.core :as criterium]
            [editscript.util.common :as common]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(defn- legacy-vec-edits*
  ;; Preserve the primitive hints and hot-loop shape from 0.7.0. Omitting the
  ;; hints makes this benchmark-local control substantially slower than the
  ;; published artifact and can hide a real release regression.
  [a b n m]
  (let [^long n n
        ^long m m
        delta (- n m)
        snake (fn [^long k ^long x]
                (loop [x x y (- x k)]
                  (let [ax (get a x)
                        by (get b y)]
                    (if (and (< x n)
                             (< y m)
                             (= (type ax) (type by))
                             (= ax by))
                      (recur (inc x) (inc y))
                      x))))
        update-frontier
        (fn [frontier ^long k]
          (let [[delete-x delete-ops] (get frontier (dec k) [-1 []])
                delete-x             (inc ^long delete-x)
                [add-x add-ops]       (get frontier (inc k) [-1 []])
                x                     (max delete-x ^long add-x)
                ^long snake-x         (snake k x)
                ops                   (if (> delete-x ^long add-x)
                                        (conj delete-ops :-)
                                        (conj add-ops :+))
                ops                   (if (> snake-x x)
                                        (conj ops (- snake-x x))
                                        ops)]
            (assoc! frontier k [snake-x ops])))]
    (loop [p 0 frontier (transient {})]
      (let [frontier (loop [k (- p) frontier frontier]
                       (if (< k delta)
                         (recur (inc k) (update-frontier frontier k))
                         frontier))
            frontier (loop [k (+ delta p) frontier frontier]
                       (if (< delta k)
                         (recur (dec k) (update-frontier frontier k))
                         frontier))
            frontier (update-frontier frontier delta)]
        (if (= n (first (get frontier delta)))
          (-> (persistent! frontier) (get delta) second rest)
          (recur (inc p) frontier))))))

(defn- swap-ops
  [edits]
  (mapv #(case % :+ :- :- :+ %) edits))

(defn- legacy-vec-edits
  [a b]
  (let [a (vec a)
        b (vec b)
        n (count a)
        m (count b)
        edits (if (< n m)
                (legacy-vec-edits* b a m n)
                (legacy-vec-edits* a b n m))]
    (common/min+plus->replace (if (< n m) (swap-ops edits) edits))))

(def sparse-source (vec (range 2000)))

(def sparse-target
  (reduce (fn [result value]
            (cond
              (zero? (mod value 29)) result
              (zero? (mod value 31)) (conj result value [:inserted value])
              (zero? (mod value 37)) (conj result [:changed value])
              :else                  (conj result value)))
          []
          sparse-source))

(def dense-source (vec (range 400)))
(def dense-target (mapv #(+ 1000 %) (range 400)))

(def snake-source (vec (range 50000)))
(def snake-target snake-source)

(def delta-source (vec (range 5000)))
(def delta-target (subvec delta-source 0 500))

(def workload-data
  {"sparse-2000" [sparse-source sparse-target]
   "dense-400"   [dense-source dense-target]
   "snake-50000" [snake-source snake-target]
   "delta-5000"  [delta-source delta-target]})

(def workloads
  (into {}
        (mapcat
          (fn [[name [a b]]]
            [[(str name "-arrays")
              {:implementation "arrays"
               :f #(common/vec-edits a b {:vec-timeout nil})}]
             [(str name "-map")
              {:implementation "map"
               :f #(legacy-vec-edits a b)}]])
          workload-data)))

(defn- quick-mean-us
  [f]
  (-> (criterium/quick-benchmark (f) {})
      :mean
      first
      (* 1000000.0)))

(defn- benchmark-workload
  [[name {:keys [implementation f]}]]
  (println "Benchmarking" name)
  (let [result (f)]
    {:workload       name
     :implementation implementation
     :mean-us        (quick-mean-us f)
     :edit-tokens    (count result)}))

(defn- write-results!
  [output results]
  (with-open [writer (io/writer output)]
    (csv/write-csv
      writer
      (cons ["Workload" "Frontier" "Mean (us)" "Edit tokens"]
            (map (fn [{:keys [workload implementation mean-us edit-tokens]}]
                   [workload implementation (format "%.3f" mean-us) edit-tokens])
                 results)))))

(defn- validate-workloads!
  []
  (doseq [[name [a b]] workload-data]
    (when-not (= (legacy-vec-edits a b)
                 (common/vec-edits a b {:vec-timeout nil}))
      (throw (ex-info "Wu benchmark implementations disagree"
                      {:workload name})))))

(defn -main
  [& [output & selected]]
  (let [output   (or output "wu-frontier-time.csv")
        selected (if (seq selected) (set selected) (set (keys workloads)))
        unknown  (seq (remove workloads selected))]
    (when unknown
      (throw (ex-info "Unknown Wu benchmark workload"
                      {:unknown unknown
                       :available (sort (keys workloads))})))
    (validate-workloads!)
    (let [results (->> workloads
                       (filter (comp selected key))
                       (sort-by key)
                       (mapv benchmark-workload))]
      (doseq [{:keys [workload implementation mean-us edit-tokens]} results]
        (println workload implementation (format "%.3f us," mean-us)
                 edit-tokens "tokens"))
      (write-results! output results))))

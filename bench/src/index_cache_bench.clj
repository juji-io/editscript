(ns index-cache-bench
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

(defn- unique-subtree
  [n changed?]
  {:payload [n (inc n) (if changed? :changed (+ n 2)) (+ n 3)]
   :metadata {:enabled (even? n) :label (str "unique-" n)}})

(def unique-origin
  (mapv #(unique-subtree % false) (range 2000)))

(def unique-target
  (mapv #(unique-subtree % (= % 1000)) (range 2000)))

(def workloads
  {"index-shared-cached"
   {:cache? true :f #(index/index shared-origin)}

   "index-shared-uncached"
   {:cache? false :f #(index/index shared-origin)}

   "index-unique-cached"
   {:cache? true :f #(index/index unique-origin)}

   "index-unique-uncached"
   {:cache? false :f #(index/index unique-origin)}

   "a-star-shared-cached"
   {:cache? true :f #(editscript/diff shared-origin shared-target {:algo :a-star})}

   "a-star-shared-uncached"
   {:cache? false :f #(editscript/diff shared-origin shared-target {:algo :a-star})}

   "a-star-unique-cached"
   {:cache? true :f #(editscript/diff unique-origin unique-target {:algo :a-star})}

   "a-star-unique-uncached"
   {:cache? false :f #(editscript/diff unique-origin unique-target {:algo :a-star})}})

(def cache-lookup-var
  (ns-resolve 'editscript.util.index 'cached-metadata))

(def cache-store-var
  (ns-resolve 'editscript.util.index 'cache-metadata!))

(defn- uncached-bindings
  []
  {cache-lookup-var (fn [_ _] nil)
   cache-store-var  (fn [_ _ metadata] metadata)})

(defn- in-cache-mode
  [cache? f]
  (if cache?
    (f)
    (with-redefs-fn (uncached-bindings) f)))

(defn- quick-mean-us
  [f cache?]
  (-> (in-cache-mode cache?
                     #(criterium/quick-benchmark (f) {}))
      :mean
      first
      (* 1000000.0)))

(defn- cache-counts
  [f cache?]
  (let [original-lookup @cache-lookup-var
        original-store  @cache-store-var
        lookups         (volatile! 0)
        hits            (volatile! 0)
        misses          (volatile! 0)
        lookup          (fn [context data]
                          (vswap! lookups inc)
                          (when cache?
                            (let [metadata (original-lookup context data)]
                              (when metadata (vswap! hits inc))
                              metadata)))
        store           (fn [context data metadata]
                          (vswap! misses inc)
                          (if cache?
                            (original-store context data metadata)
                            metadata))]
    (with-redefs-fn {cache-lookup-var lookup
                     cache-store-var  store}
      f)
    {:lookups @lookups :hits @hits :misses @misses}))

(defn- benchmark-workload
  [[name {:keys [cache? f]}]]
  (println "Benchmarking" name)
  (merge {:workload name
          :cache? cache?
          :mean-us (quick-mean-us f cache?)}
         (cache-counts f cache?)))

(defn- format-row
  [{:keys [workload cache? mean-us lookups hits misses]}]
  [workload
   (if cache? "yes" "no")
   (format "%.3f" mean-us)
   lookups
   hits
   misses
   (format "%.3f" (if (zero? lookups) 0.0 (* 100.0 (/ hits lookups))))])

(defn- write-results!
  [output results]
  (with-open [writer (io/writer output)]
    (csv/write-csv
      writer
      (cons ["Workload" "Cache" "Mean (us)" "Lookups" "Hits" "Misses"
             "Hit rate (%)"]
            (map format-row results)))))

(defn- validate-workloads!
  []
  (doseq [[origin target] [[shared-origin shared-target]
                           [unique-origin unique-target]]]
    (let [script (editscript/diff origin target {:algo :a-star})]
      (when-not (= target (editscript/patch origin script))
        (throw (ex-info "Identity-cache benchmark failed its round trip" {}))))))

(defn -main
  [& [output & selected]]
  (let [output   (or output "index-cache-time.csv")
        selected (if (seq selected) (set selected) (set (keys workloads)))
        unknown  (seq (remove workloads selected))]
    (when unknown
      (throw (ex-info "Unknown identity-cache benchmark workload"
                      {:unknown unknown
                       :available (sort (keys workloads))})))
    (validate-workloads!)
    (let [results (->> workloads
                       (filter (comp selected key))
                       (sort-by key)
                       (mapv benchmark-workload))]
      (doseq [result results]
        (println (zipmap [:workload :cache :mean-us :lookups :hits :misses
                          :hit-rate]
                         (format-row result))))
      (write-results! output results))))

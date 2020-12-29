(ns bench
  (:require [clojure.data :as clj]
            [clojure.data.csv :as csv]
            [clojure.java.io :as io]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.clojure-test :as test]
            [clojure.math.combinatorics :as combo]
            [criterium.core :as c]
            [differ.core :as differ]
            [editscript.core :as editscript]
            [editscript.edit]
            [lambdaisland.deep-diff2 :as deep]
            [taoensso.nippy :as nippy]))

(def data1 (-> "../resources/drawing1.edn"
               slurp
               read-string))
(def data2 (-> "../resources/drawing2.edn"
               slurp
               read-string))
(def data3 (-> "../resources/drawing3.edn"
               slurp
               read-string))
(def data4 (-> "../resources/drawing4.edn"
               slurp
               read-string))

;; original data size

(def size1 (count (nippy/freeze data1))) ;1004
(def size2 (count (nippy/freeze data2))) ;1004
(def size3 (count (nippy/freeze data3))) ;1016
(def size4 (count (nippy/freeze data4))) ;555

;; diff function candidates
(def diffs {"Editscript A*"    #'editscript/diff
            "Editscript Quick" #(editscript/diff %1 %2 {:algo :quick})
            "differ"           #'differ/diff
            "clojure.data"     #'clj/diff
            "deep-diff2"       #'deep/diff})

(def ids (combo/permuted-combinations [1 2 3 4] 2))
(def datas (combo/permuted-combinations [data1 data2 data3 data4] 2))

;; write diff sizes in file diff-sizes.csv

(defn diff-size
  [k [d1 d2]]
  (count (nippy/fast-freeze (let [d ((diffs k) d1 d2)]
                              (if (instance? editscript.edit.EditScript d)
                                (editscript.edit/get-edits d)
                                d)))))

(let [res (conj (map (fn [[i j] ds]
                       (conj (for [k (keys diffs)] (diff-size k ds))
                             (str "diff" i "-" j)))
                     ids
                     datas)
                (conj (keys diffs) "Data Set"))]
  (with-open [writer (io/writer "diff-sizes.csv")]
    (csv/write-csv writer res)))

;; write diff times in file diff-time.csv

(defn diff-time
  [k [d1 d2]]
  (-> (c/quick-benchmark ((diffs k) d1 d2) {})
      :mean
      first
      ((partial * 1000000))
      (double)
      (Math/round)))

(let [res (conj (map (fn [[i j] ds]
                       (conj (for [k (keys diffs)] (diff-time k ds))
                             (str "diff" i "-" j)))
                     ids
                     datas)
                (conj (keys diffs) "Data Set"))]
  (with-open [writer (io/writer "diff-time.csv")]
    (csv/write-csv writer res)))


;; round trip

(def patches {:editscript-a*    #'editscript/patch
              :editscript-quick #'editscript/patch
              :differ           #'differ/patch})

(defn roundtrip-time
  [k [i1 i2]]
  (println [(inc i1) (inc i2)])
  (println k)
  (let [d1       (datas i1)
        d2       (datas i2)
        diff     (diffs k)
        patch    (patches k)
        correct? (= d2 (patch d1 (diff d1 d2)))]
    (if correct?
      (c/quick-bench (patch d1 (diff d1 d2)))
      "Round trip failed!"))
  (println "---"))

(doseq [k  (keys patches)
        ds (combo/permuted-combinations [0 1 2 3] 2)]
  (roundtrip-time k ds))

;; property based testing

(def compound (fn [inner-gen]
                (gen/one-of [(gen/list inner-gen)
                             (gen/vector inner-gen)
                             (gen/set inner-gen)
                             (gen/map inner-gen inner-gen)])))

(def compound-wo-set (fn [inner-gen]
                       (gen/one-of [(gen/list inner-gen)
                                    (gen/vector inner-gen)
                                    (gen/map inner-gen inner-gen)])))

(def compound-vec-map-only (fn [inner-gen]
                             (gen/one-of [(gen/vector inner-gen)
                                          (gen/map inner-gen inner-gen)])))

(def scalars (gen/frequency [[19 (gen/one-of [gen/int
                                              gen/string])]
                             [1 (gen/return nil)]]))

(test/defspec differ-roundtrip-generative-test
  2000
  (prop/for-all [a (gen/recursive-gen compound scalars)
                 b (gen/recursive-gen compound scalars)]
                (let [d (differ/diff a b)]
                  (= b (differ/patch a d)))))
;;==> fail

(test/defspec differ-roundtrip-wo-set-generative-test
  2000
  (prop/for-all [a (gen/recursive-gen compound-wo-set scalars)
                 b (gen/recursive-gen compound-wo-set scalars)]
                (let [d (differ/diff a b)]
                  (= b (differ/patch a d)))))
;;==> fail

(test/defspec differ-roundtrip-vec-map-only-generative-test
  2000
  (prop/for-all [a (gen/recursive-gen compound-vec-map-only scalars)
                 b (gen/recursive-gen compound-vec-map-only scalars)]
                (let [d (differ/diff a b)]
                  (= b (differ/patch a d)))))
;;==> fail

(test/defspec editscript-quick-roundtrip-generative-test
  2000
  (prop/for-all [a (gen/recursive-gen compound scalars)
                 b (gen/recursive-gen compound scalars)]
                (let [d (editscript/diff a b {:algo :quick})]
                  (= b (editscript/patch a d)))))
;;==> success

(test/defspec editscript-a*-roundtrip-generative-test
  2000
  (prop/for-all [a (gen/recursive-gen compound scalars)
                 b (gen/recursive-gen compound scalars)]
                (let [d (editscript/diff a b)]
                  (= b (editscript/patch a d)))))
;;==> success

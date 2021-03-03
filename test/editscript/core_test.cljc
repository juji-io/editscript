;;
;; Copyright (c) Huahai Yang. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.
;;

(ns editscript.core-test
  (:require [clojure.test :refer [is are testing deftest]]
            [editscript.core :refer [patch diff]]
            [editscript.edit :as e]
            ;; [editscript.diff.quick :as q]
            ;; [editscript.diff.a-star :as a]
            [editscript.util.common :as com
             #?@(:cljs [:include-macros true])]
            [clojure.test.check.generators :as gen]
            #?(:cljs [clojure.test.check :refer [quick-check]])
            #?(:cljs [cljs.reader :as reader])
            [clojure.test.check.clojure-test :as test
             #?@(:cljs [:refer-macros [defspec] :include-macros true])]
            [clojure.test.check.properties :as prop
             #?@(:cljs [:include-macros true])]))

(deftest str-diff-test
  (let [a    {:data ["hello word" 24 22 {:a [1 2 3]} 1 3 #{1 2 3}]}
        b    {:data ["Hello world!" 42 22 {:a [1 3]} 1 3 #{1 2 3}]}
        d-q  (diff a b {:algo :quick :str-diff? true})
        d-a  (diff a b {:algo :a-star :str-diff? true})
        e-q  (e/get-edits d-q)
        e-a  (e/get-edits d-a)
        d-q' (e/edits->script e-q)
        d-a' (e/edits->script e-a)]
    (is (e/valid-edits? e-q))
    (is (e/valid-edits? e-a))
    (is (= e-q
           e-a
           [[[:data 0] :s [[:r "H"] 8 [:+ "l"] 1 [:+ "!"]]]
            [[:data 1] :r 42]
            [[:data 3 :a 1] :-]]))
    (is (= b (patch a d-q)))
    (is (= b (patch a d-a)))
    (is (= b (patch a d-q')))
    (is (= b (patch a d-a')))))

(deftest map-entry-test
  (let [a   (first {:a :c})
        b   (first {:a :b})
        d-a (diff a b {:algo :a-star})
        d-q (diff a b {:algo :quick})]
    (is (= b (patch a d-a)))
    (is (= b (patch a d-q)))))

;; generative tests

(def compound (fn [inner-gen]
                (gen/one-of [(gen/list inner-gen)
                             (gen/vector inner-gen)
                             (gen/set inner-gen)
                             (gen/map inner-gen inner-gen)])))

(def scalars (gen/frequency [[19 (gen/one-of [gen/int
                                              gen/string])]
                             [1 (gen/return nil)]]))

(test/defspec quick-end-2-end-generative-test
  2000
  (prop/for-all [a (gen/recursive-gen compound scalars)
                 b (gen/recursive-gen compound scalars)]
                (let [s  (diff a b {:algo :quick})
                      e  (e/get-edits s)
                      s' (e/edits->script e)]
                  (and (= b (patch a s))
                       (= b (patch a s'))))))


(test/defspec a-star-end-2-end-generative-test
  2000
  (prop/for-all [a (gen/recursive-gen compound scalars)
                 b (gen/recursive-gen compound scalars)]
                (let [s  (diff a b)
                      e  (e/get-edits s)
                      s' (e/edits->script e)]
                  (and (= b (patch a s))
                       (= b (patch a s'))))))

(test/defspec combine-edits-generative-test
  2000
  (prop/for-all [a (gen/recursive-gen compound scalars)
                 b (gen/recursive-gen compound scalars)
                 c (gen/recursive-gen compound scalars)]
                (let [d-ab (diff a b {:algo :quick})
                      d-bc (diff b c {:algo :quick})
                      d-ac (diff a c {:algo :quick})]
                  (and (= c (patch a d-ac))
                       (= c (patch a (e/combine d-ab d-bc)))
                       (= c (patch a (e/edits->script
                                      (into (e/get-edits d-ab) (e/get-edits d-bc)))))))))

;; sample data tests

(def data1 (-> "resources/drawing1.edn"
               #?(:clj slurp :cljs com/vslurp)
               #?(:clj read-string :cljs reader/read-string)))
(def data2 (-> "resources/drawing2.edn"
               #?(:clj slurp :cljs com/vslurp)
               #?(:clj read-string :cljs reader/read-string)))
(def data3 (-> "resources/drawing3.edn"
               #?(:clj slurp :cljs com/vslurp)
               #?(:clj read-string :cljs reader/read-string)))
(def data4 (-> "resources/drawing4.edn"
               #?(:clj slurp :cljs com/vslurp)
               #?(:clj read-string :cljs reader/read-string)))

(deftest drawing-sample-test
  (testing "A sample JSON data of a drawing program from https://github.com/justsml/json-diff-performance, converted to edn using https://github.com/peterschwarz/json-to-edn"
    (let [diff12 (diff data1 data2)
          diff13 (diff data1 data3)
          diff14 (diff data1 data4)]
      (is (= data2 (patch data1 diff12)))
      (is (= 1 (e/edit-distance diff12)))
      (is (= 7 (e/get-size diff12)))
      (is (= (e/get-edits diff12)
             [[[2 :fill] :r "#0000ff"]]))
      (is (= data3 (patch data1 diff13)))
      (is (= 5 (e/edit-distance diff13)))
      (is (= 31 (e/get-size diff13)))
      (is (= (e/get-edits diff13)
             [[[2 :rx] :r 69.5]
              [[2 :fill] :r "#0000ff"]
              [[2 :cx] :r 230.5]
              [[2 :cy] :r 228]
              [[2 :ry] :r 57]]))
      (is (= data4 (patch data1 diff14)))
      (is (= 13 (e/edit-distance diff14)))
      (is (= 73 (e/get-size diff14)))
      (is (= (e/get-edits diff14)
             [[[0 :y] :r 13]
              [[0 :width] :r 262]
              [[0 :x] :r 19]
              [[0 :height] :r 101]
              [[1 :y] :r 122]
              [[1 :x] :r 12]
              [[1 :height] :r 25.19999999999999]
              [[2] :-]
              [[2] :-]
              [[2 :y] :r 208]
              [[2 :x] :r 12]
              [[2 :height] :r 25.19999999999999]
              [[3] :-]])))))

(comment


  (require '[criterium.core :as c])

  ;; benchmarks on Intel i7-6850K CPU @ 3.60GHz

  ;; default A* algorithm

  (c/quick-bench (diff data1 data2))
  ;; ==>
  ;; Evaluation count : 1488 in 6 samples of 248 calls.
  ;; Execution time mean : 410.897795 µs
  ;; Execution time std-deviation : 10.096453 µs
  ;; Execution time lower quantile : 404.292758 µs ( 2.5%)
  ;; Execution time upper quantile : 425.510606 µs (97.5%)
  ;; Overhead used : 21.259063 ns

  (c/quick-bench (diff data1 data3))
  ;; ==>
  ;; Evaluation count : 1332 in 6 samples of 222 calls.
  ;; Execution time mean : 451.414935 µs
  ;; Execution time std-deviation : 16.621996 µs
  ;; Execution time lower quantile : 440.475234 µs ( 2.5%)
  ;; Execution time upper quantile : 477.355909 µs (97.5%)
  ;; Overhead used : 21.259063 ns

  (c/quick-bench (diff data1 data4))
  ;; ==>
  ;; Evaluation count : 210 in 6 samples of 35 calls.
  ;; Execution time mean : 2.978602 ms
  ;; Execution time std-deviation : 109.198185 µs
  ;; Execution time lower quantile : 2.871433 ms ( 2.5%)
  ;; Execution time upper quantile : 3.134244 ms (97.5%)
  ;; Overhead used : 21.259063 ns

  (c/quick-bench (diff data4 data1))
  ;; ==>
  ;; Evaluation count : 216 in 6 samples of 36 calls.
  ;; Execution time mean : 2.810354 ms
  ;; Execution time std-deviation : 102.475210 µs
  ;; Execution time lower quantile : 2.756376 ms ( 2.5%)
  ;; Execution time upper quantile : 2.987457 ms (97.5%)
  ;; Overhead used : 21.259063 ns

  (e/edit-distance (diff data1 data4))
  ;; ==> 13
  (e/get-size (diff data1 data4))
  ;; ==> 73
  (diff data1 data4)
  ;; ==>
  ;; [[[0 :y] :r 13] [[0 :width] :r 262] [[0 :x] :r 19] [[0 :height] :r 101] [[1 :y] :r 122] [[1 :x] :r 12] [[1 :height] :r 25.19999999999999] [[2] :-] [[2] :-] [[2 :y] :r 208] [[2 :x] :r 12] [[2 :height] :r 25.19999999999999] [[3] :-]]

  ;; quick algorithm

  (c/quick-bench (diff data1 data2 {:algo :quick}))
  ;; ==>
  ;; Evaluation count : 14100 in 6 samples of 2350 calls.
  ;; Execution time mean : 41.946587 µs
  ;; Execution time std-deviation : 3.521578 µs
  ;; Execution time lower quantile : 37.960159 µs ( 2.5%)
  ;; Execution time upper quantile : 45.623306 µs (97.5%)
  ;; Overhead used : 9.966537 ns

  (c/quick-bench (diff data1 data3 {:algo :quick}))
  ;; ==>
  ;; Evaluation count : 13794 in 6 samples of 2299 calls.
  ;; Execution time mean : 45.373427 µs
  ;; Execution time std-deviation : 2.745173 µs
  ;; Execution time lower quantile : 42.548519 µs ( 2.5%)
  ;; Execution time upper quantile : 49.367947 µs (97.5%)
  ;; Overhead used : 9.966537 ns

  (c/quick-bench (diff data1 data4 {:algo :quick}))
  ;; ==>
  ;; Evaluation count : 4674 in 6 samples of 779 calls.
  ;; Execution time mean : 135.947273 µs
  ;; Execution time std-deviation : 10.746898 µs
  ;; Execution time lower quantile : 124.835175 µs ( 2.5%)
  ;; Execution time upper quantile : 150.795063 µs (97.5%)
  ;; Overhead used : 9.966537 ns
  (e/edit-distance (diff data1 data4 {:algo :quick}))
  ;; ==> 36
  (e/get-size (diff data1 data4))
  ;; ==> 217
  (diff data1 data4)
  ;; [[[0] :-] [[0] :-] [[0] :-] [[0 :y1] :-] [[0 :type] :r "rect"] [[0 :borderWidth] :r 1] [[0 :label] :-] [[0 :x1] :-] [[0 :y2] :-] [[0 :x2] :-] [[0 :y] :+ 13] [[0 :r] :+ 0] [[0 :width] :+ 262] [[0 :x] :+ 19] [[0 :height] :+ 101] [[1 :y] :r 122] [[1 :color] :r "#0000FF"] [[1 :fill] :r {:r 256, :g 0, :b 0, :a 0.5}] [[1 :width] :r 10] [[1 :type] :r "textBlock"] [[1 :size] :r "24px"] [[1 :weight] :r "bold"] [[1 :x] :r 12] [[1 :height] :r 25.19999999999999] [[1 :text] :r "DojoX Drawing Rocks"] [[2 :points] :-] [[2 :type] :r "text"] [[2 :y] :+ 208] [[2 :family] :+ "sans-serif"] [[2 :width] :+ 200] [[2 :size] :+ "18px"] [[2 :pad] :+ 3] [[2 :weight] :+ "normal"] [[2 :x] :+ 12] [[2 :height] :+ 25.19999999999999] [[2 :text] :+ "This is just text"]]


  )

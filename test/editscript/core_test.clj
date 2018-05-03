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
  (:require [clojure.test :refer :all]
            [editscript.core :refer :all]
            [editscript.diff.quick :as q]
            [editscript.diff.a-star :as a]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.clojure-test :as test]
            [clojure.test.check.properties :as prop]
            [clojure.java.io :as io]
            ;; [criterium.core :as c]
            [editscript.edit :as e]))

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
  10000
  (prop/for-all [a (gen/recursive-gen compound scalars)
                 b (gen/recursive-gen compound scalars)]
                (= b (patch a (q/diff a b)))))

(test/defspec a-star-end-2-end-generative-test
  10000
  (prop/for-all [a (gen/recursive-gen compound scalars)
                 b (gen/recursive-gen compound scalars)]
                (= b (patch a (a/diff a b)))))

;; sample data tests

(defn- read-data
  [f]
  (-> f io/resource slurp read-string))

(def data1 (read-data "drawing1.edn"))
(def data2 (read-data "drawing2.edn"))
(def data3 (read-data "drawing3.edn"))
(def data4 (read-data "drawing4.edn"))

(deftest drawing-sample-test
  (testing "A sample JSON data of a drawing program from https://github.com/justsml/json-diff-performance, converted to edn using https://github.com/peterschwarz/json-to-edn"
    (is (= data2 (patch data1 (a/diff data1 data2))))
    (is (= data3 (patch data1 (a/diff data1 data3))))
    (is (= data4 (patch data1 (a/diff data1 data4))))))

(comment

;; benchmarks

;; default A* algorithm

(c/quick-bench (a/diff data1 data2))
;; ==>
;; Evaluation count : 258 in 6 samples of 43 calls.
;; Execution time mean : 2.387151 ms
;; Execution time std-deviation : 36.977425 µs
;; Execution time lower quantile : 2.333425 ms ( 2.5%)
;; Execution time upper quantile : 2.424183 ms (97.5%)
;; Overhead used : 9.787889 ns
(e/edit-distance (a/diff data1 data2))
;; ==> 1
(e/get-size (a/diff data1 data2))
;; ==> 2
(a/diff data1 data2)
;; ==>
;; [[[2 :fill] :r "#0000ff"]]

(c/quick-bench (a/diff data1 data3))
;; ==>
;; Evaluation count : 186 in 6 samples of 31 calls.
;; Execution time mean : 3.374443 ms
;; Execution time std-deviation : 209.607156 µs
;; Execution time lower quantile : 3.167858 ms ( 2.5%)
;; Execution time upper quantile : 3.587970 ms (97.5%)
;; Overhead used : 9.787889 ns
(e/edit-distance (a/diff data1 data3))
;; ==> 5
(e/get-size (a/diff data1 data3))
;; ==> 10
(a/diff data1 data3)
;; ==>
;; [[[2 :rx] :r 69.5] [[2 :fill] :r "#0000ff"] [[2 :cx] :r 230.5] [[2 :cy] :r 228] [[2 :ry] :r 57]]

(c/quick-bench (a/diff data1 data4))
;; ==>
;; Evaluation count : 192 in 6 samples of 32 calls.
;; Execution time mean : 3.235073 ms
;; Execution time std-deviation : 157.019928 µs
;; Execution time lower quantile : 3.113682 ms ( 2.5%)
;; Execution time upper quantile : 3.478369 ms (97.5%)
;; Overhead used : 9.787889 ns

;; Found 1 outliers in 6 samples (16.6667 %)
;; low-severe	 1 (16.6667 %)
;; Variance from outliers : 13.8889 % Variance is moderately inflated by outliers
(e/edit-distance (a/diff data1 data4))
;; ==> 13
(e/get-size (a/diff data1 data4))
;; ==> 23
(a/diff data1 data4)
;; ==>
;; [[[0 :y] :r 13] [[0 :width] :r 262] [[0 :x] :r 19] [[0 :height] :r 101] [[1 :y] :r 122] [[1 :x] :r 12] [[1 :height] :r 25.19999999999999] [[2] :-] [[2] :-] [[2 :y] :r 208] [[2 :x] :r 12] [[2 :height] :r 25.19999999999999] [[3] :-]]

;; quick algorithm

(c/quick-bench (q/diff data1 data2))
;; ==>
;; Evaluation count : 19254 in 6 samples of 3209 calls.
;; Execution time mean : 31.286514 µs
;; Execution time std-deviation : 610.084801 ns
;; Execution time lower quantile : 30.763616 µs ( 2.5%)
;; Execution time upper quantile : 31.994251 µs (97.5%)
;; Overhead used : 9.787889 ns
(e/edit-distance (q/diff data1 data2))
;; ==> 1
(e/get-size (q/diff data1 data2))
;; ==> 2
(q/diff data1 data2)
;; ==>
;; [[[2 :fill] :r "#0000ff"]]

(c/quick-bench (q/diff data1 data3))
;; ==>
;; Evaluation count : 18156 in 6 samples of 3026 calls.
;; Execution time mean : 34.500821 µs
;; Execution time std-deviation : 1.234259 µs
;; Execution time lower quantile : 33.423791 µs ( 2.5%)
;; Execution time upper quantile : 35.991187 µs (97.5%)
;; Overhead used : 9.787889 ns
(e/edit-distance (q/diff data1 data3))
;; ==> 5
(e/get-size (q/diff data1 data3))
;; ==> 10
(q/diff data1 data3)
;; ==>
;; [[[2 :rx] :r 69.5] [[2 :fill] :r "#0000ff"] [[2 :cx] :r 230.5] [[2 :cy] :r 228] [[2 :ry] :r 57]]

(c/quick-bench (q/diff data1 data4))
;; ==>
;; Evaluation count : 6702 in 6 samples of 1117 calls.
;; Execution time mean : 81.733234 µs
;; Execution time std-deviation : 1.074693 µs
;; Execution time lower quantile : 80.428501 µs ( 2.5%)
;; Execution time upper quantile : 82.611032 µs (97.5%)
;; Overhead used : 9.787889 ns
(e/edit-distance (q/diff data1 data4))
;; ==> 36
(e/get-size (q/diff data1 data4))
;; ==> 75
(q/diff data1 data4)
;; [[[0] :-] [[0] :-] [[0] :-] [[0 :y1] :-] [[0 :type] :r "rect"] [[0 :borderWidth] :r 1] [[0 :label] :-] [[0 :x1] :-] [[0 :y2] :-] [[0 :x2] :-] [[0 :y] :+ 13] [[0 :r] :+ 0] [[0 :width] :+ 262] [[0 :x] :+ 19] [[0 :height] :+ 101] [[1 :y] :r 122] [[1 :color] :r "#0000FF"] [[1 :fill] :r {:r 256, :g 0, :b 0, :a 0.5}] [[1 :width] :r 10] [[1 :type] :r "textBlock"] [[1 :size] :r "24px"] [[1 :weight] :r "bold"] [[1 :x] :r 12] [[1 :height] :r 25.19999999999999] [[1 :text] :r "DojoX Drawing Rocks"] [[2 :points] :-] [[2 :type] :r "text"] [[2 :y] :+ 208] [[2 :family] :+ "sans-serif"] [[2 :width] :+ 200] [[2 :size] :+ "18px"] [[2 :pad] :+ 3] [[2 :weight] :+ "normal"] [[2 :x] :+ 12] [[2 :height] :+ 25.19999999999999] [[2 :text] :+ "This is just text"]]



)

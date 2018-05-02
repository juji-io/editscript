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
            [criterium.core :as c]
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

(defn- read-data [f]
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
;; Evaluation count : 264 in 6 samples of 44 calls.
;; Execution time mean : 2.476637 ms
;; Execution time std-deviation : 325.440989 µs
;; Execution time lower quantile : 2.292459 ms ( 2.5%)
;; Execution time upper quantile : 3.036373 ms (97.5%)
;; Overhead used : 9.788943 ns
(e/edit-distance (a/diff data1 data2))
;; ==> 1
(e/get-size (a/diff data1 data2))
;; ==> 2
(a/diff data1 data2)
;; ==>
;; [[[2 :fill] :r "#0000ff"]]

;; Found 1 outliers in 6 samples (16.6667 %)
;; low-severe	 1 (16.6667 %)
;; Variance from outliers : 31.6576 % Variance is moderately inflated by outliers

(c/quick-bench (a/diff data1 data3))
;; ==>
;; Evaluation count : 192 in 6 samples of 32 calls.
;; Execution time mean : 3.714949 ms
;; Execution time std-deviation : 669.761337 µs
;; Execution time lower quantile : 3.101943 ms ( 2.5%)
;; Execution time upper quantile : 4.731556 ms (97.5%)
;; Overhead used : 9.788943 ns
(e/edit-distance (a/diff data1 data3))
;; ==> 5
(e/get-size (a/diff data1 data3))
;; ==> 10
(a/diff data1 data3)
;; ==>
;; [[[2 :rx] :r 69.5] [[2 :fill] :r "#0000ff"] [[2 :cx] :r 230.5] [[2 :cy] :r 228] [[2 :ry] :r 57]]

(c/quick-bench (a/diff data1 data4))
;; ==>
;; Evaluation count : 168 in 6 samples of 28 calls.
;; Execution time mean : 3.761350 ms
;; Execution time std-deviation : 147.495117 µs
;; Execution time lower quantile : 3.615001 ms ( 2.5%)
;; Execution time upper quantile : 3.918334 ms (97.5%)
;; Overhead used : 9.788943 ns
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
;; Evaluation count : 20760 in 6 samples of 3460 calls.
;; Execution time mean : 30.011144 µs
;; Execution time std-deviation : 1.693011 µs
;; Execution time lower quantile : 28.824434 µs ( 2.5%)
;; Execution time upper quantile : 32.695843 µs (97.5%)
;; Overhead used : 9.788943 ns

;; Found 1 outliers in 6 samples (16.6667 %)
;; low-severe	 1 (16.6667 %)
;; Variance from outliers : 14.3936 % Variance is moderately inflated by outliers
(e/edit-distance (q/diff data1 data2))
;; ==> 1
(e/get-size (q/diff data1 data2))
;; ==> 2
(q/diff data1 data2)
;; ==>
;; [[[2 :fill] :r "#0000ff"]]

(c/quick-bench (q/diff data1 data3))
;; ==>
;; Evaluation count : 19692 in 6 samples of 3282 calls.
;; Execution time mean : 30.754420 µs
;; Execution time std-deviation : 781.033921 ns
;; Execution time lower quantile : 30.103331 µs ( 2.5%)
;; Execution time upper quantile : 32.041437 µs (97.5%)
;; Overhead used : 9.788943 ns

;; Found 1 outliers in 6 samples (16.6667 %)
;; low-severe	 1 (16.6667 %)
;; Variance from outliers : 13.8889 % Variance is moderately inflated by outliers
(e/edit-distance (q/diff data1 data3))
;; ==> 5
(e/get-size (q/diff data1 data3))
;; ==> 10
(q/diff data1 data3)
;; ==>
;; [[[2 :rx] :r 69.5] [[2 :fill] :r "#0000ff"] [[2 :cx] :r 230.5] [[2 :cy] :r 228] [[2 :ry] :r 57]]

(c/quick-bench (q/diff data1 data4))
;; ==>
;; Evaluation count : 13962 in 6 samples of 2327 calls.
;; Execution time mean : 43.886412 µs
;; Execution time std-deviation : 841.086279 ns
;; Execution time lower quantile : 43.142360 µs ( 2.5%)
;; Execution time upper quantile : 44.971814 µs (97.5%)
;; Overhead used : 9.788943 ns
(e/edit-distance (q/diff data1 data4))
;; ==> 9
(e/get-size (q/diff data1 data4))
;; ==> 147
(q/diff data1 data4)
;; [[[0] :-] [[0] :-] [[0] :-] [[0] :-] [[0] :-] [[0] :-] [[0] :+ {:y 13, :r 0, :color "#000000", :fill "#CCCCCC", :width 262, :type "rect", :cap "round", :borderWidth 1, :style "Solid", :x 19, :height 101}] [[1] :+ {:y 122, :family "sans-serif", :color "#0000FF", :fill {:r 256, :g 0, :b 0, :a 0.5}, :width 10, :type "textBlock", :cap "round", :borderWidth 1, :size "24px", :style "Solid", :pad 3, :weight "bold", :x 12, :height 25.19999999999999, :text "DojoX Drawing Rocks"}] [[2] :+ {:y 208, :family "sans-serif", :color "#000000", :fill "#CCCCCC", :width 200, :type "text", :cap "round", :borderWidth 1, :size "18px", :style "Solid", :pad 3, :weight "normal", :x 12, :height 25.19999999999999, :text "This is just text"}]]

;; Here it does some wholesale copying, the script size explodes


)

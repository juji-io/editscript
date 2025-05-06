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
            [editscript.core :refer [patch diff get-edits edits->script
                                     edit-distance get-size change-ratio]]
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
             #?@(:cljs [:include-macros true])]
            [editscript.util.index :as i]))

(deftest readme-test
  (let [a   ["Hello word" 24 22 {:a [1 2 3]} 1 3 #{1 2}]
        b   ["Hello world" 24 23 {:a [2 3]} 1 3 #{1 2 3}]
        d   (diff a b)
        d-q (diff a b {:algo :quick :str-diff :character})
        v   (get-edits d)
        ds  (edits->script v)]
    (is (= (get-edits d) [[[0] :r "Hello world"]
                          [[2] :r 23]
                          [[3 :a 0] :-]
                          [[6 3] :+ 3]]))
    (is (= (get-edits d-q)
           [[[0] :s [9 [:+ "l"] 1]]
            [[2] :r 23]
            [[3 :a 0] :-]
            [[6 3] :+ 3]]))
    (is (= 4 (edit-distance d)))
    (is (= 23 (get-size d)))
    (is (= 4 (edit-distance d-q)))
    (is (= 23 (get-size d-q)))
    (is (= b (patch a d)))
    (is (= b (patch a d-q)))
    (is (= b (patch a ds))))
  (let [a   [2 {:a 42} 3 {:b 4} {:c 29}]
        b   [{:a 5} {:b 5}]
        d   (diff a b)
        d-q (diff a b {:algo :quick})]
    (is (= 1 (edit-distance d)))
    (is (= 9 (get-size d)))
    (is (= (get-edits d) [[[] :r [{:a 5} {:b 5}]]]))
    (is (= 7 (edit-distance d-q)))
    (is (= 35 (get-size d-q)))
    (is (= (get-edits d-q) [[[0] :-]
                            [[0] :-]
                            [[0] :-]
                            [[0 :b] :-]
                            [[0 :a] :+ 5]
                            [[1 :c] :-]
                            [[1 :b] :+ 5]]))))

(deftest char-str-diff-test
  (let [a    {:data ["hello word" 24 22 {:a [1 2 3]} 1 3 #{1 2 3}]}
        b    {:data ["Hello world!" 42 22 {:a [1 3]} 1 3 #{1 2 3}]}
        d-q  (diff a b {:algo :quick :str-diff :character})
        d-a  (diff a b {:algo :a-star :str-diff :character})
        e-q  (e/get-edits d-q)
        e-a  (e/get-edits d-a)
        d-q1 (e/edits->script e-q)
        d-a1 (e/edits->script e-a)]
    (is (e/valid-edits? e-q))
    (is (e/valid-edits? e-a))
    (is (= e-q
           e-a
           [[[:data 0] :s [[:r "H"] 8 [:+ "l"] 1 [:+ "!"]]]
            [[:data 1] :r 42]
            [[:data 3 :a 1] :-]]))
    (is (= b (patch a d-q)))
    (is (= b (patch a d-a)))
    (is (= b (patch a d-q1)))
    (is (= b (patch a d-a1)))))

(deftest word-str-diff-test
  (let [a  "You know, it does not matter how slowly you go as long as you do not stop."
        b  "Hey, do you know, it does not matter how fast you go as long as you don't stop."
        d  (diff a b {:str-diff :word :str-change-limit 0.5})
        e  (e/get-edits d)
        ds (e/edits->script e)]
    (is (e/valid-edits? e))
    (is (= e [[[]
               :sw
               [[:+ ["Hey," "do"]]
                [:r ["you"]]
                6
                [:r ["fast"]]
                6
                [:r ["don't"]]
                [:- 1]
                1]]]))
    (is (= b (patch a d)))
    (is (= b (patch a ds)))))

(deftest line-str-diff-test
  (let [a
        "侠客行
        唐 李白
        赵客缦胡缨，吴钩霜雪明。
        银鞍照白马，飒沓如流星。
        十步杀一人，千里不留行。
        事了拂衣去，深藏身与名。
        闲过信陵饮，脱剑膝前横。
        将炙啖朱亥，持觞劝侯嬴。
        三杯吐然诺，五岳倒为轻。
        眼花耳热后，意气素霓生。
        救赵挥金槌，邯郸先震惊。
        千秋二壮士，烜赫大梁城。
        纵死侠骨香，不惭世上英。
        谁能书阁下，白首太玄经。"
        b
        "侠客行
        赵客缦胡缨，吴钩霜雪明。
        银鞍照白马，飒沓如流星。
        十步杀百人，千里不留行。
        事了拂衣去，深藏身与名。
        闲过信陵饮，脱剑膝前横。
        将炙啖朱亥，持觞劝侯嬴。
        三杯吐然诺，五岳倒为轻。
        眼花耳热后，意气素霓生。
        救赵挥金槌，邯郸先震惊。
        千秋二壮士，烜赫大梁城。
        纵死侠骨香，不惭世上英。
        谁能书阁下，白首太玄经。"
        d  (diff a b {:str-diff :line :str-change-limit 0.5})
        e  (e/get-edits d)
        ds (e/edits->script e)]
    (is (e/valid-edits? e))
    (is (= e [[[] :sl [1 [:- 1]
                       2 [:r ["        十步杀百人，千里不留行。"]] 9]]]))
    (is (= b (patch a d)))
    (is (= b (patch a ds)))))

(deftest map-entry-test
  (let [a   (first {:a :c})
        b   (first {:a :b})
        d-a (diff a b {:algo :a-star})
        d-q (diff a b {:algo :quick})]
    (is (= b (patch a d-a)))
    (is (= b (patch a d-q)))))

(deftest vec-timeout-test
  (let [a   (vec (range 30000))
        b   (vec (concat (range 100) [213 222 223 224 123] (range 300 800)
                         [100 950 221 897 1232] (range 990 2810) (range 2810 30000)))
        d   (diff a b)
        d-o (diff a b {:vec-timeout 1})
        d-q (diff a b {:vec-timeout 1 :algo :quick})
        ]
    (is (= b (patch a d)))
    (is (< 1 (e/edit-distance d)))
    (is (= b (patch a d-o)))
    (is (= (e/edit-distance d-o) 1))
    (is (= b (patch a d-q)))
    (is (= (e/edit-distance d-q) 1))
    ))

(deftest change-ratio-test
  (are [a b r] (is (= (change-ratio a (diff a b)) r))
    {:a {:b 2 :c 3}} {:a {:b 3 :c 2}} 0.5
    1                2                1.0
    [:a :b]          [:a :c]          (double (/ 1 3))
    [:a]             [:b]             0.5
    [:a]             [:a :b]          0.5
    []               [:a]             2.0
    [:a]             []               0.5
    [:a]             nil              0.5))

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
               #?(:default slurp :cljs com/vslurp)
               #?(:default read-string :cljs reader/read-string)))
(def data2 (-> "resources/drawing2.edn"
               #?(:default slurp :cljs com/vslurp)
               #?(:default read-string :cljs reader/read-string)))
(def data3 (-> "resources/drawing3.edn"
               #?(:default slurp :cljs com/vslurp)
               #?(:default read-string :cljs reader/read-string)))
(def data4 (-> "resources/drawing4.edn"
               #?(:default slurp :cljs com/vslurp)
               #?(:default read-string :cljs reader/read-string)))

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
  ;; Evaluation count : 1752 in 6 samples of 292 calls.
  ;; Execution time mean : 358.745200 µs
  ;; Execution time std-deviation : 20.783677 µs
  ;; Execution time lower quantile : 344.041945 µs ( 2.5%)
  ;; Execution time upper quantile : 383.072120 µs (97.5%)
  ;; Overhead used : 14.553786 ns

  (c/quick-bench (diff data1 data3))
  ;; ==>
  ;; Evaluation count : 1638 in 6 samples of 273 calls.
  ;; Execution time mean : 391.184907 µs
  ;; Execution time std-deviation : 23.452044 µs
  ;; Execution time lower quantile : 371.116374 µs ( 2.5%)
  ;; Execution time upper quantile : 423.216158 µs (97.5%)
  ;; Overhead used : 14.553786 ns

  (c/quick-bench (diff data1 data4))
  ;; ==>
  ;; Evaluation count : 246 in 6 samples of 41 calls.
  ;; Execution time mean : 2.147089 ms
  ;; Execution time std-deviation : 18.701984 µs
  ;; Execution time lower quantile : 2.132857 ms ( 2.5%)
  ;; Execution time upper quantile : 2.179175 ms (97.5%)
  ;; Overhead used : 14.553786 ns

  (c/quick-bench (diff data4 data1))
  ;; ==>
  ;; Evaluation count : 624 in 6 samples of 104 calls.
  ;; Execution time mean : 1.044390 ms
  ;; Execution time std-deviation : 35.679732 µs
  ;; Execution time lower quantile : 1.006069 ms ( 2.5%)
  ;; Execution time upper quantile : 1.097140 ms (97.5%)
  ;; Overhead used : 14.553786 ns

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


  (def old (-> "resources/old.edn"
               #?(:clj slurp :cljs com/vslurp)
               #?(:clj read-string :cljs reader/read-string)))
  (def new (-> "resources/new.edn"
               #?(:clj slurp :cljs com/vslurp)
               #?(:clj read-string :cljs reader/read-string)))
  (diff old new {:str-diff :word :algo :quick})




  )

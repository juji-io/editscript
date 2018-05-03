;;
;; Copyright (c) Huahai Yang. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.
;;

(ns editscript.diff.quick-test
  (:require [clojure.test :refer :all]
            ;; [criterium.core :as c]
            [editscript.edit :refer :all]
            [editscript.diff.quick :refer :all]
            [editscript.core :refer [patch]]))

(deftest vec-edits-test
  (testing "Wu 1990 vector edit example and more"
    (let [a (vec (seq "acbdeacbed"))
          b (vec (seq "acebdabbabed"))
          c [0 0]
          d [1 -1 -1 nil -1 1 -1 -1 -1]]
      (is (= (vec-edits a b) [2 :+ 2 :- 1 :- 1 :+ :+ :+ 2]))
      (is (= (vec-edits c d) [:+ :+ :+ :+ :+ :+ :+ :r :r])))))

(deftest min+plus->replace-test
  (testing "Replacement of consecutive :- :+ with :r"
    (is (= (min+plus->replace [:- :+ 2 3 4 :- :- :+ 3 4 :- 3 :+ 3])
           [:r 2 3 4 :- :r 3 4 :- 3 :+ 3]))
    (is (= (min+plus->replace [:- :+ 2 3 4 :- :- :+ 3 4 :- :+ 3])
           [:r 2 3 4 :- :r 3 4 :r 3]))
    (is (= (min+plus->replace [:- :+]) [:r]))
    (is (= (min+plus->replace [:- :+ 3]) [:r 3]))
    (is (= (min+plus->replace []) []))
    (is (= (min+plus->replace [:-]) [:-]))))

(deftest diff-patch-test
  (testing "Diffing and patching some nested data structures"
    (let [a   {:a {:o 4} :b 'b}
          b   {:a {:o 3} :b 'c :c 42}
          b-a (diff a b)
          a-b (diff b a)
          c   [nil 3 'c {:a 3} 4]
          d   [3 'c {:b 3} 4]
          d-c (diff c d)
          c-d (diff d c)
          e   ["abc" 24 23 {:a [1 2 3]} 1 3 #{1 2}]
          f   [24 23 {:a [2 3]} 1 3 #{1 2 3}]
          f-e (diff e f)
          e-f (diff f e)
          g   {nil 1}
          h   {nil 2}
          h-g (diff g h)
          g-h (diff h g)
          i   {nil 3}
          j   '()
          j-i (diff i j)
          i-j (diff j i)
          k   {1 3}
          l   {1 nil}
          l-k (diff k l)
          k-l (diff l k)]
      (is (= (get-edits b-a)
             [[[:a :o] :r 3]
              [[:b] :r 'c]
              [[:c] :+ 42]]))
      (is (= (get-edits d-c)
             [[[0] :-]
              [[2 :a] :-]
              [[2 :b] :+ 3]]))
      (is (= (get-edits f-e)
             [[[0] :-]
              [[2 :a 0] :-]
              [[5 3] :+ 3]]))
      (is (= a (patch b a-b)))
      (is (= b (patch a b-a)))
      (is (= c (patch d c-d)))
      (is (= d (patch c d-c)))
      (is (= e (patch f e-f)))
      (is (= f (patch e f-e)))
      (is (= g (patch h g-h)))
      (is (= h (patch g h-g)))
      (is (= i (patch j i-j)))
      (is (= j (patch e j-i)))
      (is (= k (patch l k-l)))
      (is (= l (patch k l-k))))))


(comment

  ;; sequence diff benchmark from https://github.com/friemen/diffit

  (defn rand-alter
    [pass-prob remove-prob add-prob xs]
    (let [ops (vec (concat (repeat pass-prob :=)
                           (repeat remove-prob :-)
                           (repeat add-prob :+)))]
      (reduce (fn [xs x]
                (case (rand-nth ops)
                  :+ (conj xs x "-")
                  :- xs
                  := (conj xs x)))
              []
              xs)))

  (def as (vec (range 2000)))
  (def bs (vec (rand-alter 80 10 10 as)))

  (c/bench (editscript.diff.a-star/diff as bs))

  (c/bench (vec-edits as bs))

  ;; ==>
  ;; Evaluation count : 1920 in 60 samples of 32 calls.
  ;; Execution time mean : 32.714460 ms
  ;; Execution time std-deviation : 997.703094 µs
  ;; Execution time lower quantile : 32.008704 ms ( 2.5%)
  ;; Execution time upper quantile : 35.291895 ms (97.5%)
  ;; Overhead used : 9.788943 ns

  ;; Found 9 outliers in 60 samples (15.0000 %)
	;; low-severe	 9 (15.0000 %)
  ;; Variance from outliers : 17.3922 % Variance is moderately inflated by outliers

  (c/bench (diff as bs))
  ;; ==>
  ;; Evaluation count : 1800 in 60 samples of 30 calls.
  ;; Execution time mean : 34.128722 ms
  ;; Execution time std-deviation : 1.284325 ms
  ;; Execution time lower quantile : 33.047449 ms ( 2.5%)
  ;; Execution time upper quantile : 37.014303 ms (97.5%)
  ;; Overhead used : 9.788943 ns

  ;; Found 3 outliers in 60 samples (5.0000 %)
	;; low-severe	 3 (5.0000 %)
  ;; Variance from outliers : 23.8507 % Variance is moderately inflated by outliers

  (c/bench (diffit.vec/diff as bs))
  ;; ==>
  ;; Evaluation count : 1500 in 60 samples of 25 calls.
  ;; Execution time mean : 42.089736 ms
  ;; Execution time std-deviation : 1.517260 ms
  ;; Execution time lower quantile : 40.642024 ms ( 2.5%)
  ;; Execution time upper quantile : 45.306760 ms (97.5%)
  ;; Overhead used : 9.788943 ns

)

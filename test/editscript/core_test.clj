(ns editscript.core-test
  (:require [clojure.test :refer :all]
            [editscript.core :refer :all]
            [editscript.diff.base :refer :all]
            ;; for benchmark
            [criterium.core :as c]
            [diffit.vec]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.clojure-test :as test]
            [clojure.test.check.properties :as prop]))

(deftest vec-edits-test
  (testing "Wu 1990 vector edit example and more"
    (let [a (vec (seq "acbdeacbed"))
          b (vec (seq "acebdabbabed"))
          c [0 0]
          d [1 -1 -1 nil -1 1 -1 -1 -1]]
      (is (= (vec-edits a b) [2 :+ 2 :- 1 :- 1 :+ :+ :+ 2]))
      (is (= (vec-edits c d) [:+ :+ :+ :+ :+ :+ :+ :+ :+ :- :-])))))

(deftest min+plus->replace-test
  (testing "Replacement of consecutive :- :+ with :r"
    (is (= (min+plus->replace [:- :+ 2 3 4 :- :- :+ 3 4 :- 3 :+ 3])
           [:r 2 3 4 :- :- :+ 3 4 :- 3 :+ 3]))
    (is (= (min+plus->replace [:- :+ 2 3 4 :- :- :+ 3 4 :- :+ 3])
           [:r 2 3 4 :- :- :+ 3 4 :r 3]))
    (is (= (min+plus->replace [:- :+]) [:r]))
    (is (= (min+plus->replace [:- :+ 3]) [:r 3]))
    (is (= (min+plus->replace []) []))
    (is (= (min+plus->replace [:-]) [:-]))))

(deftest diff-patch-test
  (testing "Differing and patching some nested data structures"
    (let [a {:a {:o 4} :b 'b}
          b {:a {:o 3} :b 'c :c 42}
          b-a (diff a b)
          a-b (diff b a)
          c [nil 3 'c {:a 3} 4]
          d [3 'c {:b 3} 4]
          d-c (diff c d)
          c-d (diff d c)
          e ["abc" 24 23 {:a [1 2 3]} 1 3 #{1 2}]
          f [24 23 {:a [2 3]} 1 3 #{1 2 3}]
          f-e (diff e f)
          e-f (diff f e)
          g {nil 1}
          h {nil 2}
          h-g (diff g h)
          g-h (diff h g)
          i {nil 3}
          j '()
          j-i (diff i j)
          i-j (diff j i)
          k {1 3}
          l {1 nil}
          l-k (diff k l)
          k-l (diff l k)]
      (is (= (get-edits b-a)
             [[[:a :o] :editscript.core/r 3]
              [[:b] :editscript.core/r 'c]
              [[:c] :editscript.core/+ 42]] ))
      (is (= (get-edits d-c)
             [[[0] :editscript.core/-]
              [[2 :a] :editscript.core/-]
              [[2 :b] :editscript.core/+ 3]]))
      (is (= (get-edits f-e)
             [[[0] :editscript.core/-]
              [[2 :a 0] :editscript.core/-]
              [[5 3] :editscript.core/+ 3]]))
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

(def compound (fn [inner-gen]
                (gen/one-of [(gen/list inner-gen)
                             (gen/vector inner-gen)
                             (gen/set inner-gen)
                             (gen/map inner-gen inner-gen)])))

(def scalars (gen/frequency [[19 (gen/one-of [gen/int
                                              gen/string])]
                             [1 (gen/return nil)]]))

(test/defspec end-2-end-generative-test
  10000
  (prop/for-all [a (gen/recursive-gen compound scalars)
                 b (gen/recursive-gen compound scalars)]
                (= b (patch a (diff a b)))))

(comment

  ;; benchmark from https://github.com/friemen/diffit

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

  (def as (vec (range 100)))
  (def bs (vec (rand-alter 80 10 10 as)))
  (c/bench (editscript.diff.a-star/A* nil as bs))
  (c/bench (vec-edits as bs))

  (c/bench (diff as bs))
  ;; ==>
  ;; Evaluation count : 1260 in 60 samples of 21 calls.
  ;; Execution time mean : 50.319675 ms
  ;; Execution time std-deviation : 896.197158 µs
  ;; Execution time lower quantile : 49.152154 ms ( 2.5%)
  ;; Execution time upper quantile : 52.479851 ms (97.5%)
  ;; Overhead used : 10.184272 ns

  ;; Found 5 outliers in 60 samples (8.3333 %)
	;; low-severe	 4 (6.6667 %)
	;; low-mild	 1 (1.6667 %)
  ;; Variance from outliers : 7.7632 % Variance is slightly inflated by outliers

  (c/bench (diffit.vec/diff as bs))
  ;; ==>
  ;; Evaluation count : 1200 in 60 samples of 20 calls.
  ;; Execution time mean : 51.102351 ms
  ;; Execution time std-deviation : 759.814820 µs
  ;; Execution time lower quantile : 50.090719 ms ( 2.5%)
  ;; Execution time upper quantile : 52.280115 ms (97.5%)
  ;; Overhead used : 10.184272 ns

  ;; Found 1 outliers in 60 samples (1.6667 %)
	;; low-severe	 1 (1.6667 %)
  ;; Variance from outliers : 1.6389 % Variance is slightly inflated by outliers

  )

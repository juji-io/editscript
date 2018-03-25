(ns editscript.core-test
  (:require [clojure.test :refer :all]
            [editscript.core :refer :all]))

(deftest vec-edits-test
  (testing "Wu 1990 vector edit example"
    (let [a (vec (seq "acbdeacbed"))
          b (vec (seq "acebdabbabed"))]
      (is (= (vec-edits a b) [2 :+ 2 :- 1 :- 1 :+ :+ :+ 2])))))

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

(deftest get-edits-test
  (testing "Differing some nested data structures"
    (let [a {:a {:o 4} :b 'b}
          b {:a {:o 3} :b 'c :c 42}

          c [nil 3 'c {:a 3} 4]
          d [3 'c {:b 3} 4]

          e ["abc" 24 23 {:a [1 2 3]} 1 3 #{1 2}]
          f [24 23 {:a [2 3]} 1 3 #{1 2 3}]]
      (is (= (get-edits (diff a b))
             [[[:a :o] :editscript.core/r]
              [[:b] :editscript.core/r]
              [[:c] :editscript.core/+ 42]] ))
      (is (= (get-edits (diff c d))
             [[[0] :editscript.core/-]
              [[2 :a] :editscript.core/-]
              [[2 :b] :editscript.core/+ 3]]))
      (is (= (get-edits (diff e f))
             [[[0] :editscript.core/-]
              [[2 :a 0] :editscript.core/-]
              [[5 3] :editscript.core/+ 3]])))))

(ns editscript.core-test
  (:require [clojure.test :refer :all]
            [editscript.core :refer :all]
            [clojure.test.check.generators :as gen]))

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
          e-f (diff f e)]
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
      (is (= f (patch e f-e))))))

(def compound (fn [inner-gen]
                (gen/one-of [(gen/list inner-gen)
                             (gen/vector inner-gen)
                             (gen/set inner-gen)
                             (gen/map inner-gen inner-gen)])))

(def scalars (gen/one-of [gen/int gen/string-alpha-numeric]))

(def data (gen/recursive-gen compound scalars))

(last (gen/sample data 200))

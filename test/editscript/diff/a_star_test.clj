(ns editscript.a-star-test
  (:require [clojure.test :refer :all]
            [editscript.core :refer :all]
            [editscript.diff.a-star :refer :all]
            [editscript.core-test :refer :all]
            ;; for benchmark
            [criterium.core :as c]))

(defn test-diff
  [a b result-set]
  (is (result-set (get-edits (diff a b))))
  (is (= b (patch a (diff a b))))
  (is (= a (patch b (diff b a)))))

(deftest vec-diff-test
  (testing "Testing A* with some nested vector data"
    (test-diff (vec (seq "ab"))
               (vec (seq "bc"))
               #{[[[0] :-] [[1] :+ \c]]})
    (test-diff (vec (seq "abd"))
               (vec (seq "bc"))
               #{[[[0] :-] [[1] :r \c]]})
    (test-diff [:a nil [:b :c]]
               [:d :a :b nil]
               #{[[[0] :+ :d] [[2] :+ :b] [[4] :-]]})
    (test-diff [[:d] [:e] :f]
               [[:e] [:f] :d]
               #{[[[0] :-] [[1] :+ [:f]] [[2] :r :d]]
                 [[[0] :-] [[1] :r [:f]] [[2] :+ :d]]})
    (test-diff [[:a] :b [:c [:d] [:e] :f]]
               [[:b] [:c [:e] [:f] :d]]
               #{[[[0 0] :r :b] [[1] :-] [[1 1 0] :r :e] [[1 2 0] :r :f] [[1 3] :r :d]]
                 [[[0 0] :r :b] [[1] :-] [[1 1] :-] [[1 2] :+ [:f]] [[1 3] :r :d]]
                 [[[0 0] :r :b] [[1] :-] [[1 1] :-] [[1 2] :r [:f]] [[1 3] :+ :d]]})
    (test-diff [:a [:b :c :d] :e :f]
               [[:b :c :d :e] [:f]]
               #{[[[0] :-] [[0 3] :+ :e] [[1] :-] [[1] :r [:f]]]})
    (test-diff [:e [:a :b] :c]
               [:a [:b :c] :d]
               #{[[[0] :r :a] [[1 0] :-] [[1 1] :+ :c] [[2] :r :d]]})
    (test-diff [[:u]]
               [:s :t]
               #{[[[0] :+ :s] [[1] :r :t]] [[[0] :r :s] [[1] :+ :t]]})
    (test-diff [[:a [:b :c] :d] :e :f]
               [:b :c [:e] :f :g]
               #{[[[0] :+ :b] [[1] :r :c] [[2] :r [:e]] [[4] :+ :g]]
                 [[[0] :r :b] [[1] :+ :c] [[2] :r [:e]] [[4] :+ :g]]
                 [[[0] :r :b] [[1] :r :c] [[2] :+ [:e]] [[4] :+ :g]]})
    (test-diff [[:a :b] :c [:d]]
               [:c [:d] [:a :b]]
               #{[[[0] :-] [[2] :+ [:a :b]]]})
    (test-diff [[:s :t] [:u]]
               [[:s] :t :s]
               #{[[[0 1] :-] [[1] :+ :t] [[2] :r :s]]
                 [[[0 1] :-] [[1] :r :t] [[2] :+ :s]]})
    (test-diff [[:s :t] [:u]]
               [[:s] :t :s]
               #{[[[0 1] :-] [[1] :+ :t] [[2] :r :s]]
                 [[[0 1] :-] [[1] :r :t] [[2] :+ :s]]})
    (test-diff [:a [:s :t] :u]
               [[:b] [:s :t :u]]
               #{[[[0] :r [:b]] [[1 2] :+ :u] [[2] :-]]})
    (test-diff [:a [:s :t] [:u]]
               [[:s :u] :t :s]
               #{[[[0] :-] [[0 1] :r :u] [[1] :+ :t] [[2] :r :s]]
                 [[[0] :-] [[0 1] :r :u] [[1] :r :t] [[2] :+ :s]]})
    (test-diff [:a [:b [:c [:d :e] :f]]]
               [:a [:b :c :d] :e]
               #{[[[1 1] :+ :c] [[1 2] :r :d] [[2] :+ :e]]
                 [[[1 1] :r :c] [[1 2] :+ :d] [[2] :+ :e]]})))

(deftest mix-diff-test
  (testing "Testing A* with some mixed data structures"
    (test-diff 1
               2
               #{[[[] :r 2]]})
    (test-diff {:a {:o 4} :b 'b}
               {:a {:o 3} :b 'c :c 42}
               #{[[[:a :o] :r 3] [[:b] :r 'c] [[:c] :+ 42]]})
    (test-diff {:a [3 4] :b [1 2]}
               {:a [3] :b {:a 3} :c 42}
               #{[[[:a 1] :-] [[:b] :r {:a 3}] [[:c] :+ 42]]})))

(ns editscript.a-star-test
  (:require [clojure.test :refer :all]
            [editscript.core :refer :all]
            [editscript.diff.a-star :refer :all]
            ;; for benchmark
            [criterium.core :as c]))

(deftest vec-diff-test
  (testing "Testing A* with some nested vector data"
    (is (= (get-edits (diff (vec (seq "ab"))
                            (vec (seq "bc"))))
           [[[0] :-] [[1] :+ \c]]))
    (is (= (get-edits (diff (vec (seq "abd"))
                            (vec (seq "bc"))))
           [[[0] :-] [[1] :r \c]]))
    (is (= (get-edits (diff [[:a] :b [:c [:d] [:e] :f]]
                            [[:b] [:c [:e] [:f] :d]]))
           [[[0 0] :r :b] [[1] :-] [[1 1 0] :r :e] [[1 2 0] :r :f] [[1 3] :r :d]]))
    (is (= (get-edits (diff [:a [:b :c :d] :e :f]
                            [[:b :c :d :e] [:f]]))
           [[[0] :-] [[0 3] :+ :e] [[1] :-] [[1] :r [:f]]]))
    (is (= (get-edits (diff [:e [:a :b] :c]
                            [:a [:b :c] :d]))
           [[[0] :r :a] [[1 0] :-] [[1 1] :+ :c] [[2] :r :d]]))
    (is (= (get-edits (diff [[:u]]
                            [:s :t]))
           [[[0] :+ :s] [[1] :r :t]]))
    (is (= (get-edits (diff [[:a [:b :c] :d] :e :f]
                            [:b :c [:e] :f :g]))
           [[[0] :+ :b] [[1] :r :c] [[2] :r [:e]] [[4] :+ :g]]))
    (is (= (get-edits (diff [[:a :b] :c [:d]]
                            [:c [:d] [:a :b]]))
           [[[0] :-] [[2] :+ [:a :b]]]))
    (is (= (get-edits (diff [[:s :t] [:u]]
                            [[:s] :t :s]))
           [[[0 1] :-] [[1] :+ :t] [[2] :r :s]]))
    (is (= (get-edits (diff [:a [:s :t] :u]
                            [[:b] [:s :t :u]]))
           [[[0] :r [:b]] [[1 2] :+ :u] [[2] :-]]))
    (is (= (get-edits (diff [:a [:s :t] [:u]]
                            [[:s :u] :t :s]))
           [[[0] :-] [[0 1] :r :u] [[1] :+ :t] [[2] :r :s]]))
    (is (= (get-edits (diff [:a [:b [:c [:d :e] :f]]]
                            [:a [:b :c :d] :e]))
           [[[1 1] :+ :c] [[1 2] :r :d] [[2] :+ :e]]))))

(deftest mix-diff-test
  (testing "Testing A* with some mixed data structures"
    (is (= (get-edits (diff 1 2))
           [[[] :r 2]]))
    #_(is (= (get-edits (diff {:a {:o 4} :b 'b}
                            {:a {:o 3} :b 'c :c 42}))
           ))))

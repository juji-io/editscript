;;
;; Copyright (c) Huahai Yang. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.
;;

(ns editscript.diff.a-star-test
  (:require [clojure.test :refer [is testing deftest]]
            [editscript.edit :refer [get-edits]]
            [editscript.diff.a-star :refer [diff]]))

(deftest vec-diff-test
  (testing "Testing A* with some nested vector data"
    (is (= (get-edits (diff (vec (seq "ab"))
                            (vec (seq "bc"))))
           [[[0] :-]
            [[1] :+ \c]]))
    (is (= (get-edits (diff (vec (seq "abd"))
                            (vec (seq "bc"))))
           [[[0] :-]
            [[1] :r \c]]))
    (is (#{[[[0] :r [-1]]
            [[1] :+ 1]]
           [[[0] :+ [-1]]
            [[1] :r 1]]}
         (get-edits (diff [[0 0 0]]
                          [[-1] 1]))))
    (is (= (get-edits (diff [:a nil [:b :c]]
                            [:d :a :b nil]))
           [[[0] :+ :d]
            [[2] :+ :b]
            [[4] :-]]))
    (is (#{[[[0] :-]
            [[1] :+ [:f]]
            [[2] :r :d]]
           [[[0] :-]
            [[1] :r [:f]]
            [[2] :+ :d]]
           [[[0 0] :r :e]
            [[1 0] :r :f]
            [[2] :r :d]] }
         (get-edits (diff [[:d] [:e] :f]
                          [[:e] [:f] :d]))))
    (is (#{[[[0 0] :r :b]
            [[1] :-]
            [[1 1 0] :r :e]
            [[1 2 0] :r :f]
            [[1 3] :r :d]]
           [[[0 0] :r :b]
            [[1] :-]
            [[1 1] :-]
            [[1 2] :+ [:f]]
            [[1 3] :r :d]]
           [[[0 0] :r :b]
            [[1] :-]
            [[1 1] :-]
            [[1 2] :r [:f]]
            [[1 3] :+ :d]]}
         (get-edits (diff [[:a] :b [:c [:d] [:e] :f]]
                          [[:b] [:c [:e] [:f] :d]]))))
    (is (= (get-edits (diff [:a [:b :c :d] :e :f]
                            [[:b :c :d :e] [:f]]))
           [[[0] :-]
            [[0 3] :+ :e]
            [[1] :-]
            [[1] :r [:f]]]))
    (is (= (get-edits (diff [:e [:a :b] :c]
                            [:a [:b :c] :d]))
           [[[0] :r :a]
            [[1 0] :-]
            [[1 1] :+ :c]
            [[2] :r :d]]))
    (is (#{[[[0] :+ :s]
            [[1] :r :t]]
           [[[0] :r :s]
            [[1] :+ :t]]}
         (get-edits (diff [[:u]]
                          [:s :t]))))
    (is (= [[[] :r [:b :c [:e] :f :g]]]
           (get-edits (diff [[:a [:b :c] :d] :e :f]
                            [:b :c [:e] :f :g]))))
    (is (= (get-edits (diff [[:a :b] :c [:d]]
                            [:c [:d] [:a :b]]))
           [[[0] :-]
            [[2] :+ [:a :b]]]))
    (is (#{[[[0 1] :-]
            [[1] :+ :t]
            [[2] :r :s]]
           [[[0 1] :-]
            [[1] :r :t]
            [[2] :+ :s]]}
         (get-edits (diff [[:s :t] [:u]]
                          [[:s] :t :s]))))
    (is (= (get-edits (diff [:a [:s :t] :u]
                            [[:b] [:s :t :u]]))
           [[[0] :r [:b]]
            [[1 2] :+ :u]
            [[2] :-]]))
    (is (#{[[[0] :-]
            [[0 1] :r :u]
            [[1] :+ :t]
            [[2] :r :s]]
           [[[0] :-]
            [[0 1] :r :u]
            [[1] :r :t]
            [[2] :+ :s]]}
         (get-edits (diff [:a [:s :t] [:u]]
                          [[:s :u] :t :s]))))
    (is (#{[[[1 1] :+ :c]
            [[1 2] :r :d]
            [[2] :+ :e]]
           [[[1 1] :r :c]
            [[1 2] :+ :d]
            [[2] :+ :e]]}
         (get-edits (diff [:a [:b [:c [:d :e] :f]]]
                          [:a [:b :c :d] :e]))))))

(deftest mix-diff-test
  (testing "Testing diff with some mixed data structures"
    (is (= (get-edits (diff 1
                            2))
           [[[] :r 2]]))
    (is (= (get-edits (diff [0 -1]
                            [1]))
           [[[0] :-]
            [[0] :r 1]]))
    (is (= (get-edits (diff [{} {0 0}]
                            [{() ()}]))
           [[[0] :-]
            [[0 0] :-]
            [[0 ()] :+ ()]]))
    (is (#{[[[] :r #{1}]]
           [[[0] :-]
            [[-1] :-]
            [[1] :+ 1]]}
          (get-edits (diff #{0 -1}
                           #{1}))))
    (is (= (get-edits (diff []
                            [[{-1 3}]]))
           [[[] :r [[{-1 3}]]]]))
    (is (= (get-edits (diff #{nil -30}
                            #{[()] {}}))
           [[[] :r #{[()] {}}]]))
    (is (= (get-edits (diff #{0 15 ""}
                            #{nil 0 15}))
           [[[""] :-]
            [[nil] :+ nil]]))
    (is (= (get-edits (diff {-37 0}
                            {"" 5 2 nil -37 1}))
           [[[] :r {"" 5 2 nil -37 1}]]))
    (is (= [[[] :r '(0 0 1)]]
           (get-edits (diff '(())
                            '(0 0 1)))))
    (is (#{[[[0] :+ [-1]]
            [[1] :r '()]]
           [[[0] :r [-1]]
            [[1] :+ '()]]}
          (get-edits (diff  '([0 0 0])
                            '([-1] ())))))
    (is (= (get-edits (diff {:a {:o 4} :b 'b}
                            {:a {:o 3} :b 'c :c 42}))
           [[[:a :o] :r 3]
            [[:b] :r 'c]
            [[:c] :+ 42]]))
    (is (= (get-edits (diff {:a [3 4] :b [1 2]}
                            {:a [3] :b {:a 3} :c 42}))
           [[[:a 1] :-]
            [[:b] :r {:a 3}]
            [[:c] :+ 42]]))
    (is (= (get-edits (diff [:zero {:x :y}]
                            [:zero {:a "a" :b "b"}]))
           [[[1] :r {:a "a" :b "b"}]]))
    (is (= (get-edits (diff [:zero :one]
                            [:zero {:a "a" :b "b"}]))
           [[[1] :r {:a "a" :b "b"}]]))
    (is (= (get-edits (diff [:zero {:a "a" :b "b"}]
                            [:zero :one]))
           [[[1] :r :one]]))
    (is (= (get-edits (diff [:zero [:a :b :c :d :e :f]]
                            [:zero [:a]]))
           [[[1] :r [:a]]]))
    (is
      (= (get-edits
           (diff
             "hello world, this is our first visit to your planet. we come in peace."
             "hello worldhis is our first visit to your planet. We come in peace. haha"
             {:str-diff :character}))
         [[[] :s [11 [:- 3] 39 [:r "W"] 16 [:+ " haha"]]]]))
    ))

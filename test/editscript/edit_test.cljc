;;
;; Copyright (c) Huahai Yang. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.
;;

(ns editscript.edit-test
  (:require [editscript.edit :as e]
            [editscript.core :as c]
            #?(:clj [clojure.test :refer [is are deftest ]]
			   :cljr [clojure.test :refer [is are deftest ]]
               :cljs [cljs.test :refer [is are deftest] :include-macros true])))

(deftest edits-equality-test
  (are [d1 d2] (= (e/get-edits d1) (e/get-edits d2))
    (c/diff {} {})
    (c/diff {} {})

    (c/diff 1 2)
    (c/diff 3 2)))

(deftest valid-edits-test
  (are [edits] (e/valid-edits? edits)
    []
    [[[0] :-]]
    [[[1 2] :r 32]]
    [[[:b 2] :+ '()]
     [[:a] :-]]
    [[[] :s [32 [:- 10] 2 [:r "ab"] 11 [:+ "old"]]]]))

(deftest invalid-edits-test
  (are [edits] (not (e/valid-edits? edits))
    nil
    '()
    [1]
    [0 1]
    ['()]
    [[]]
    [[1 2 3]]
    [[[1] :+ 3 4]]
    [[[1] :- 3]]
    [[[1] :r]]
    [[[1] :s [1 [:- "ab"] [:+ "cd"]]]]
    [[[1] :s [[:r 10] 2]]]))

(deftest sizing-test
  (are [diff size] (= size (e/get-size diff))
    (e/edits->script [])  1
    (c/diff [:a :b] [:a]) 5))

(deftest edits->script-test
  (are [a b edits] (= b (c/patch a (e/edits->script edits)))
    ["abc" 24 22 {:a [1 2 3]} 1 3 #{1 2}]
    [24 23 {:a [2 3]} 1 3 #{1 2 3}]
    [[[0] :-]
     [[1] :r 23]
     [[2 :a 0] :-]
     [[5 3] :+ 3]]

    {}
    {:x :hello-world}
    [[[] :r {:x :hello-world}]]))

(deftest combine-test
  (let [a      ["abc" 24 22 {:a [1 2 3]} 1 3 #{1 2}]
        b      [24 23 {:a [2 3]} 1 3 #{1 2 3}]
        c      [24 {:a [1 2 3]} 1 #{1 2}]
        d-ab   (c/diff a b)
        d-bc   (c/diff b c)
        d-ac   (c/diff a c)
        comb-d (e/combine d-ab d-bc)]
    (is (= c (c/patch a comb-d)))
    (is (= c (c/patch a d-ac)))
    (is (= c (c/patch a (e/edits->script
                         (into (e/get-edits d-ab) (e/get-edits d-bc))))))))

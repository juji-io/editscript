;;
;; Copyright (c) Huahai Yang. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.
;;

(ns editscript.util.pairing-test
  (:require
   [editscript.util.pairing :refer [priority-map]]
   [clojure.test :refer [are deftest testing]]))

(deftest test-priority-map
  (let [a (priority-map :a 1 :b 2 :c 3 :d 4 :e 5 :f 6)]
    (testing "Basic priority-map operations that we use in A*"
      (are [x y] (= x y)
        (empty? a)              false
        (peek a)                [:a 1]
        (peek (pop a))          [:b 2]
        (peek (assoc a :z 0))   [:z 0]
        (peek (conj a [:a -1])) [:a -1]))))

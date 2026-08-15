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
   [clojure.test :refer [are deftest is testing]]
   [clojure.test.check.clojure-test :as test
    #?@(:cljs [:refer-macros [defspec] :include-macros true])]
   [clojure.test.check.generators :as gen]
   [clojure.test.check.properties :as prop
    #?@(:cljs [:include-macros true])]
   [editscript.util.pairing :refer [priority-map]]))

(deftest test-priority-map
  (let [a (priority-map :a 1 :b 2 :c 3 :d 4 :e 5 :f 6)]
    (testing "Basic priority-map operations that we use in A*"
      (are [x y] (= x y)
        (empty? a)              false
        (peek a)                [:a 1]
        (peek (pop a))          [:b 2]
        (peek (assoc a :z 0))   [:z 0]
        (peek (conj a [:a -1])) [:a -1]))))

(deftest stale-entry-test
  (testing "a superseded priority cannot escape after the current item is popped"
    (let [queue (assoc (priority-map :a 5 :b 6) :a 1)]
      (is (= [:a 1] (peek queue)))
      (let [queue (pop queue)]
        (is (= [:b 6] (peek queue)))
        (is (= 1 (count queue))))))

  (testing "an older, better priority does not shadow the current priority"
    (let [queue (assoc (priority-map :a 1 :b 2) :a 3)]
      (is (= [:b 2] (peek queue)))
      (let [queue (pop queue)]
        (is (= [:a 3] (peek queue))))))

  (testing "pop also normalizes the root when it is called without peek"
    (let [queue (-> (priority-map :a 5 :b 6)
                    (assoc :a 1)
                    pop)]
      (is (= [:b 6] (peek queue)))
      (is (= 1 (count queue)))))

  (testing "dissociated heap entries are skipped lazily"
    (let [queue (dissoc (priority-map :a 1 :b 2) :a)]
      (is (= [:b 2] (peek queue)))
      (is (= 1 (count queue))))))

(def queue-operation-gen
  (gen/frequency
    [[5 (gen/tuple (gen/return :assoc)
                   (gen/choose 0 31)
                   gen/int)]
     [1 (gen/tuple (gen/return :dissoc)
                   (gen/choose 0 31)
                   (gen/return nil))]]))

(defn- apply-queue-operation
  [[queue model] [op item priority]]
  (case op
    :assoc [(assoc queue item priority) (assoc model item priority)]
    :dissoc [(dissoc queue item) (dissoc model item)]))

(defn- drains-like-priority-map?
  [queue model]
  (loop [queue queue
         model model]
    (if (empty? model)
      (and (empty? queue) (nil? (peek queue)))
      (let [[item priority] (peek queue)]
        (if (and (= priority (get model item ::missing))
                 (= priority (reduce min (vals model))))
          (recur (pop queue) (dissoc model item))
          false)))))

(test/defspec reprioritized-queue-model-test
  #?(:cljs 30 :cljr 30 :default 100)
  (prop/for-all [operations (gen/vector queue-operation-gen 128 512)]
    (let [[queue model] (reduce apply-queue-operation
                                [(priority-map) {}]
                                operations)]
      (and (= model (into {} (seq queue)))
           (= (count model) (count queue))
           (drains-like-priority-map? queue model)))))

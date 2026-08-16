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
  (:require [clojure.test :refer [is testing deftest]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.clojure-test :as test
             #?@(:cljs [:refer-macros [defspec] :include-macros true])]
            [clojure.test.check.properties :as prop
             #?@(:cljs [:include-macros true])]
            [editscript.edit :refer [get-edits]]
            [editscript.util.common :as common
             :refer [vec-edits min+plus->replace]]
            [editscript.diff.quick :refer [diff]]
            [editscript.core :refer [patch]]))

(deftest vec-edits-test
  (testing "Wu 1990 vector edit example and more"
    (let [a (vec (seq "acbdeacbed"))
          b (vec (seq "acebdabbabed"))
          c [0 0]
          d [1 -1 -1 nil -1 1 -1 -1 -1]]
      (is (= (vec-edits a b nil) [2 :+ 2 :- 1 :- 1 :+ :+ :+ 2]))
      (is (= (vec-edits c d nil) [:+ :+ :+ :+ :+ :+ :+ :r :r])))))

(defn- reference-vec-edits*
  [a b n m]
  (let [delta (- n m)
        snake (fn [k x]
                (loop [x x y (- x k)]
                  (let [ax (get a x)
                        by (get b y)]
                    (if (and (< x n)
                             (< y m)
                             (= (type ax) (type by))
                             (= ax by))
                      (recur (inc x) (inc y))
                      x))))
        update-frontier
        (fn [frontier k]
          (let [[delete-x delete-ops] (get frontier (dec k) [-1 []])
                delete-x             (inc delete-x)
                [add-x add-ops]       (get frontier (inc k) [-1 []])
                x                     (max delete-x add-x)
                snake-x               (snake k x)
                ops                   (if (> delete-x add-x)
                                        (conj delete-ops :-)
                                        (conj add-ops :+))
                ops                   (if (> snake-x x)
                                        (conj ops (- snake-x x))
                                        ops)]
            (assoc! frontier k [snake-x ops])))]
    (loop [p 0 frontier (transient {})]
      (let [frontier (loop [k (- p) frontier frontier]
                       (if (< k delta)
                         (recur (inc k) (update-frontier frontier k))
                         frontier))
            frontier (loop [k (+ delta p) frontier frontier]
                       (if (< delta k)
                         (recur (dec k) (update-frontier frontier k))
                         frontier))
            frontier (update-frontier frontier delta)]
        (if (= n (first (get frontier delta)))
          (-> (persistent! frontier) (get delta) second rest)
          (recur (inc p) frontier))))))

(defn- reference-swap-ops
  [edits]
  (mapv #(case % :+ :- :- :+ %) edits))

(defn- reference-vec-edits
  [a b]
  (let [a (vec a)
        b (vec b)
        n (count a)
        m (count b)
        edits (if (< n m)
                (reference-vec-edits* b a m n)
                (reference-vec-edits* a b n m))]
    (min+plus->replace (if (< n m) (reference-swap-ops edits) edits))))

(def ^:private sequence-value
  (gen/elements [nil false true -1 0 1 :a :b "a" "b" [0] '(0)]))

(test/defspec dense-wu-reference-generative-test
  #?(:cljs 250 :cljr 250 :default 1000)
  (prop/for-all [a (gen/vector sequence-value 0 80)
                 b (gen/vector sequence-value 0 80)]
                (= (reference-vec-edits a b)
                   (vec-edits a b {:vec-timeout nil}))))

(defn- edits-align?
  [a b edits]
  (loop [ops (seq edits)
         a-index 0
         b-index 0]
    (if-let [op (first ops)]
      (cond
        (integer? op)
        (and (pos? op)
             (<= (+ a-index op) (count a))
             (<= (+ b-index op) (count b))
             (= (subvec a a-index (+ a-index op))
                (subvec b b-index (+ b-index op)))
             (recur (next ops)
                    (long (+ a-index op))
                    (long (+ b-index op))))

        (= op :-)
        (and (< a-index (count a))
             (recur (next ops) (inc a-index) b-index))

        (= op :+)
        (and (< b-index (count b))
             (recur (next ops) a-index (inc b-index)))

        (= op :r)
        (and (< a-index (count a))
             (< b-index (count b))
             (recur (next ops) (inc a-index) (inc b-index)))

        :else false)
      (and (= a-index (count a))
           (= b-index (count b))))))

(test/defspec large-wu-alignment-generative-test
  #?(:cljs 50 :cljr 50 :default 200)
  (prop/for-all [a      (gen/vector gen/small-integer 128 512)
                 stride (gen/choose 3 17)]
                (let [b (reduce-kv
                          (fn [result index value]
                            (if (zero? (mod index stride))
                              (case (long (mod (quot index stride) 3))
                                0 result
                                1 (conj result [:changed value])
                                2 (conj result value [:inserted value]))
                              (conj result value)))
                          []
                          a)
                      edits (vec-edits a b {:vec-timeout nil})]
                  (edits-align? a b edits))))

(deftest long-snake-timeout-test
  (let [clock-calls (volatile! 0)
        values      (vec (range 10000))]
    (with-redefs [common/current-time
                  (fn []
                    (if (<= (vswap! clock-calls inc) 2) 0 2))]
      (is (= :timeout (vec-edits values values {:vec-timeout 1})))
      (is (<= 3 @clock-calls)))))

(deftest long-frontier-timeout-test
  (let [clock-calls (volatile! 0)
        values      (vec (range 10000))]
    (with-redefs [common/current-time
                  (fn []
                    (if (<= (vswap! clock-calls inc) 2) 0 2))]
      (is (= :timeout (vec-edits values [] {:vec-timeout 1})))
      (is (<= 3 @clock-calls)))))

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
          k-l (diff l k)
          m   "hello world, this is our first visit to your planet. we come in peace."
          n   "hello worldhis is our first visit to your planet. We come in peace. haha"
          n-m (diff m n {:str-diff :character})
          m-n (diff n m {:str-diff :character})]
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
      (is (= (get-edits n-m)
             [[[] :s [11 [:- 3] 39 [:r "W"] 16 [:+ " haha"]]]]))
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
      (is (= l (patch k l-k)))
      (is (= m (patch n m-n)))
      (is (= n (patch m n-m))))))


(comment

  (require '[criterium.core :as c])

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
  (def bs (rand-alter 80 10 10 as))

  (c/bench (editscript.diff.a-star/diff as bs))
  ;; ==>
  ;; Evaluation count : 960 in 60 samples of 16 calls.
  ;; Execution time mean : 65.203017 ms
  ;; Execution time std-deviation : 583.138552 µs
  ;; Execution time lower quantile : 64.500410 ms ( 2.5%)
  ;; Execution time upper quantile : 66.464167 ms (97.5%)
  ;; Overhead used : 9.792106 ns

  ;; Found 4 outliers in 60 samples (6.6667 %)
	;; low-severe	 3 (5.0000 %)
	;; low-mild	 1 (1.6667 %)
  ;; Variance from outliers : 1.6389 % Variance is slightly inflated by outliers

  (c/bench (vec-edits as bs nil))
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

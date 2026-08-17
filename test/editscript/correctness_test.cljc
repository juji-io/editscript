;;
;; Copyright (c) Huahai Yang. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.
;;

(ns editscript.correctness-test
  (:require [clojure.string :as string]
            [clojure.test :refer [deftest is testing]]
            [clojure.test.check.clojure-test :as test
             #?@(:cljs [:refer-macros [defspec] :include-macros true])]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop
             #?@(:cljs [:include-macros true])]
            [editscript.core :as core]
            [editscript.edit :as edit]
            [editscript.patch :as patch]
            [editscript.util.common :as common]))

(def map-key-gen
  (gen/elements [nil false 0 1 :a :b :c "key-a" "key-b"]))

(def scalar-gen
  (gen/frequency [[6 gen/small-integer]
                  [2 gen/boolean]
                  [2 gen/string]
                  [1 (gen/elements [nil :a :b :c])]]))

(defn- compound-gen
  [inner]
  (gen/frequency
    [[4 (gen/vector inner 0 5)]
     [2 (gen/fmap #(apply list %) (gen/vector inner 0 5))]
     [2 (gen/fmap set (gen/vector inner 0 5))]
     [3 (gen/fmap #(into {} %)
                  (gen/vector (gen/tuple map-key-gen inner) 0 5))]]))

(def nested-data-gen
  (gen/recursive-gen compound-gen scalar-gen))

(defn- sequential-patch
  [origin script]
  (reduce patch/patch* origin (edit/get-edits script)))

(defn- reference-data-nodes
  [data]
  (let [type (edit/get-type data)]
    (if (contains? #{:map :set :vec :lst} type)
      (inc
        (reduce +
                (map reference-data-nodes
                     (if (= type :map) (vals data) data))))
      1)))

(defn- reference-data-counts
  [edits]
  (reduce (fn [[adds dels reps] [_ op _]]
            (case op
              :+ [(inc adds) dels reps]
              :- [adds (inc dels) reps]
              :r [adds dels (inc reps)]
              [adds dels reps]))
          [0 0 0]
          edits))

(defn- script-metadata
  [script]
  [(edit/get-size script)
   (edit/get-adds-num script)
   (edit/get-dels-num script)
   (edit/get-reps-num script)
   (edit/edit-distance script)])

(defn- data-script-invariants?
  [origin target script]
  (let [edits             (edit/get-edits script)
        rebuilt           (edit/edits->script edits)
        [adds dels reps]  (reference-data-counts edits)]
    (and (vector? edits)
         (edit/valid-edits? edits)
         (not-any? #(contains? #{:s :sw :sl} (nth % 1)) edits)
         (= target (core/patch origin script))
         (= target (sequential-patch origin script))
         (= target (core/patch origin rebuilt))
         (= edits (edit/get-edits rebuilt))
         (= (script-metadata script) (script-metadata rebuilt))
         (= [adds dels reps (+ adds dels reps)]
            [(edit/get-adds-num script)
             (edit/get-dels-num script)
             (edit/get-reps-num script)
             (edit/edit-distance script)]))))

(defn- search-cost
  [script]
  (reduce (fn [cost [_ op value]]
            (+ cost
               (case op
                 :- 1
                 (:+ :r) (inc (reference-data-nodes value))
                 (:s :sw :sl) 2)))
          0
          (edit/get-edits script)))

(test/defspec algorithm-reference-invariants-generative-test
  #?(:cljs 80 :cljr 80 :default 300)
  (prop/for-all [origin nested-data-gen
                 target nested-data-gen]
    (let [a-options {:algo :a-star :vec-timeout nil}
          q-options {:algo :quick :vec-timeout nil}
          a-script  (core/diff origin target a-options)
          q-script  (core/diff origin target q-options)]
      (and (data-script-invariants? origin target a-script)
           (data-script-invariants? origin target q-script)
           (= (edit/get-edits a-script)
              (edit/get-edits (core/diff origin target a-options)))
           (= (edit/get-edits q-script)
              (edit/get-edits (core/diff origin target q-options)))
           (<= (search-cost a-script)
               (inc (reference-data-nodes target)))
           (= (reference-data-nodes origin) (core/data-nodes origin))
           (= (reference-data-nodes target) (core/data-nodes target))))))

(def shared-state-pair-gen
  (gen/let [values (gen/vector gen/small-integer 32 96)
            stride (gen/choose 4 13)]
    (let [shared  {:numbers (vec (take 12 (cycle values)))
                   :tags    (set (take 8 values))
                   :nested  [[{:enabled true :value (first values)}]]}
          origin  (mapv (fn [index value]
                          {:id index
                           :value value
                           :payload shared})
                        (range)
                        values)
          target  (reduce-kv
                    (fn [result index item]
                      (if (zero? (mod index stride))
                        (case (long (mod (quot index stride) 4))
                          0 result
                          1 (conj result
                                  (assoc-in item [:payload :numbers 0]
                                            [:changed index]))
                          2 (conj result item
                                  {:id [:inserted index]
                                   :value (- index)
                                   :payload shared})
                          3 (conj result
                                  (update-in item [:payload :tags]
                                             conj [:changed index])))
                        (conj result item)))
                    []
                    origin)]
      [origin target])))

(test/defspec shared-structure-algorithm-differential-test
  #?(:cljs 10 :cljr 10 :default 30)
  (prop/for-all [[origin target] shared-state-pair-gen]
    (let [a-script (core/diff origin target
                              {:algo :a-star :vec-timeout nil})
          q-script (core/diff origin target
                              {:algo :quick :vec-timeout nil})]
      (and (data-script-invariants? origin target a-script)
           (data-script-invariants? origin target q-script)
           (<= (search-cost a-script)
               (inc (reference-data-nodes target)))
           (= (reference-data-nodes origin) (core/data-nodes origin))
           (= (reference-data-nodes target) (core/data-nodes target))))))

(def string-level-gen
  (gen/elements [:character :word :line]))

(def string-action-gen
  (gen/elements [:keep :delete :replace]))

(def string-plan-gen
  (gen/bind
    (gen/tuple string-level-gen
               (gen/vector gen/small-integer 1 256))
    (fn [[level values]]
      (let [length (count values)]
        (gen/let [actions   (gen/vector string-action-gen length)
                  insertions (gen/vector
                               (gen/vector gen/small-integer 0 3)
                               (inc length))
                  nested?  (gen/vector gen/boolean (inc length))]
          {:level      level
           :values     values
           :actions    actions
           :insertions insertions
           :nested?    nested?})))))

(defn- generated-unit
  [level index value changed?]
  (case level
    :character
    (str (char (+ (if changed? (int \A) (int \a))
                  (mod (+ index value) 26))))

    :word
    (str (if changed? "changed" "word") "-" index "-" (mod value 97))

    :line
    (str (if changed? "changed-line" "line") "-" index "-" (mod value 97))))

(defn- segment-value
  [level units nested?]
  (if (= level :character)
    (apply str units)
    (if nested? [(vec units)] (vec units))))

(defn- string-plan-edits
  [{:keys [level values actions insertions nested?]}]
  (let [length (count values)]
    (loop [index 0
           edits []]
      (let [insert-values (nth insertions index)
            insert-units  (mapv #(generated-unit level index % true)
                                insert-values)
            edits         (if (seq insert-units)
                            (conj edits
                                  [:+ (segment-value level insert-units
                                                    (nth nested? index))])
                            edits)]
        (if (= index length)
          edits
          (let [edits
                (case (nth actions index)
                  :keep    (conj edits 1)
                  :delete  (conj edits [:- 1])
                  :replace (let [unit (generated-unit
                                        level index (nth values index) true)]
                             (conj edits
                                   [:r (segment-value level [unit]
                                                      (nth nested? index))])))]
            (recur (inc index) edits)))))))

(defn- legacy-string-patch
  [origin edits level]
  (let [source (common/transform-str origin level)
        slice  (if (= level :character) subs subvec)
        index  (volatile! 0)
        segments
        (persistent!
          (reduce
            (fn [segments edit]
              (cond
                (integer? edit)
                (let [segment (slice source @index
                                     (+ (long @index) (long edit)))]
                  (vswap! index (partial + edit))
                  (conj! segments segment))

                (= (nth edit 0) :-)
                (do (vswap! index (partial + (nth edit 1))) segments)

                (= (nth edit 0) :r)
                (let [segment (nth edit 1)]
                  (vswap! index (partial + (count segment)))
                  (conj! segments segment))

                (= (nth edit 0) :+)
                (conj! segments (nth edit 1))))
            (transient [])
            edits))]
    (case level
      :character (apply str segments)
      :word      (string/join " " (flatten segments))
      :line      (string/join "\n" (flatten segments)))))

(test/defspec segment-builder-reference-generative-test
  #?(:cljs 60 :cljr 60 :default 200)
  (prop/for-all [{:keys [level values] :as plan} string-plan-gen]
    (let [source-units (mapv #(generated-unit level %1 %2 false)
                             (range)
                             values)
          origin       (string/join (case level
                                      :character ""
                                      :word      " "
                                      :line      "\n")
                                    source-units)
          operations   (string-plan-edits plan)
          edit-op      (case level
                         :character :s
                         :word      :sw
                         :line      :sl)
          script        (edit/edits->script [[[] edit-op operations]])
          expected      (legacy-string-patch origin operations level)
          diff-script   (core/diff origin expected
                                   {:algo             :quick
                                    :str-diff         level
                                    :str-change-limit 0.99
                                    :vec-timeout      nil})
          rebuilt       (edit/edits->script (edit/get-edits diff-script))]
      (and (edit/valid-edits? (edit/get-edits script))
           (= expected (core/patch origin script))
           (= expected (sequential-patch origin script))
           (= expected (core/patch origin diff-script))
           (= expected (core/patch origin rebuilt))
           (= (script-metadata diff-script) (script-metadata rebuilt))))))

(def algorithm-gen
  (gen/elements [:a-star :quick]))

(test/defspec mixed-algorithm-composition-generative-test
  #?(:cljs 50 :cljr 50 :default 200)
  (prop/for-all [a nested-data-gen
                 b nested-data-gen
                 c nested-data-gen
                 ab-algorithm algorithm-gen
                 bc-algorithm algorithm-gen]
    (let [ab       (core/diff a b {:algo ab-algorithm :vec-timeout nil})
          bc       (core/diff b c {:algo bc-algorithm :vec-timeout nil})
          ab-before (script-metadata ab)
          bc-before (script-metadata bc)
          combined (edit/combine ab bc)]
      (and (= c (core/patch a combined))
           (= c (sequential-patch a combined))
           (edit/valid-edits? (edit/get-edits combined))
           (= (into (edit/get-edits ab) (edit/get-edits bc))
              (edit/get-edits combined))
           (= (+ (edit/get-size ab) (edit/get-size bc))
              (edit/get-size combined))
           (= (+ (edit/edit-distance ab) (edit/edit-distance bc))
              (edit/edit-distance combined))
           (= ab-before (script-metadata ab))
           (= bc-before (script-metadata bc))))))

(deftest exhaustive-small-cross-type-roundtrip-test
  (let [corpus [nil false true -1 0 1 "" "a" "a b" [] '() {} #{}
                [nil false] '(nil false) {:a nil :b [1]} #{nil false}
                [[:nested]] {:nested {:vector [1 2]}}]
        failures
        (for [origin corpus
              target corpus
              algorithm [:a-star :quick]
              :let [script (core/diff origin target
                                      {:algo algorithm :vec-timeout nil})]
              :when (not (data-script-invariants? origin target script))]
          {:origin origin :target target :algorithm algorithm})]
    (is (empty? failures) (pr-str (first failures)))))

(deftest string-level-change-ratio-test
  (testing "all three string edit operations are accepted by change-ratio"
    (doseq [[level expected-op origin target]
            [[:character :s "abcdefghij" "abcdefXhij"]
             [:word :sw "a b c d e" "a b x d e"]
             [:line :sl "a\nb\nc\nd\ne" "a\nb\nx\nd\ne"]]]
      (let [script (core/diff origin target
                              {:algo             :quick
                               :str-diff         level
                               :str-change-limit 0.99
                               :vec-timeout      nil})]
        (is (= expected-op (nth (first (edit/get-edits script)) 1)))
        (is (= 1.0 (core/change-ratio origin script)))))))

(deftest string-diff-fallback-preserves-target-test
  (testing "word and line diffs fall back to the original string, not tokens"
    (doseq [algorithm [:a-star :quick]
            [level origin target]
            [[:word "one two three" ""]
             [:line "one\ntwo\nthree" ""]
             [:word "one two three" "entirely changed"]
             [:line "one\ntwo\nthree" "entirely\nchanged"]]]
      (let [script (core/diff origin target
                              {:algo             algorithm
                               :str-diff         level
                               :str-change-limit 0.99
                               :vec-timeout      nil})]
        (is (= target (core/patch origin script))
            (pr-str {:algorithm algorithm :level level
                     :edits (edit/get-edits script)}))))))

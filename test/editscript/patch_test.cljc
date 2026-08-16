;;
;; Copyright (c) Huahai Yang. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.
;;

(ns editscript.patch-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [clojure.test.check.clojure-test :as test
             #?@(:cljs [:refer-macros [defspec] :include-macros true])]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop
             #?@(:cljs [:include-macros true])]
            [editscript.core :as core]
            [editscript.edit :as edit]
            [editscript.patch :as patch]))

(defn sequential-patch
  "Reference implementation retained by the tests to verify batching."
  [origin script]
  (reduce patch/patch* origin (edit/get-edits script)))

(deftest ordered-batch-test
  (testing "empty and single-edit scripts keep their direct fast paths"
    (let [origin {:items [0 1 2]}
          empty-script (edit/edits->script [])
          single-script (edit/edits->script [[[:items 1] :r :changed]])]
      (is (identical? origin (core/patch origin empty-script)))
      (is (= {:items [0 :changed 2]}
             (core/patch origin single-script)))
      (is (= (sequential-patch origin single-script)
             (core/patch origin single-script)))))

  (testing "streamable vector edits are applied with their sequential indices"
    (let [origin [0 1 2 3 4 5 6 7]
          script (edit/edits->script
                   [[[0] :-]
                    [[0] :-]
                    [[0] :+ :a]
                    [[1] :r :b]
                    [[3] :+ :c]
                    [[5] :-]])
          expected [:a :b 3 :c 4 6 7]]
      (is (= expected (core/patch origin script)))
      (is (= (sequential-patch origin script)
             (core/patch origin script)))))

  (testing "lists retain list semantics while sharing the one-pass path"
    (let [origin '(0 1 2 3 4 5 6 7)
          script (edit/edits->script
                   [[[0] :-]
                    [[0] :-]
                    [[0] :+ :a]
                    [[1] :r :b]
                    [[3] :+ :c]
                    [[5] :-]])]
      (is (list? (core/patch origin script)))
      (is (= '(:a :b 3 :c 4 6 7) (core/patch origin script)))
      (is (= (sequential-patch origin script)
             (core/patch origin script)))))

  (testing "non-streamable indices fall back without changing behavior"
    (let [origin [0 1 2]
          script (edit/edits->script
                   [[[0] :+ :a]
                    [[0] :+ :b]
                    [[2] :-]])]
      (is (= [:b :a 1 2] (core/patch origin script)))
      (is (= (sequential-patch origin script)
             (core/patch origin script)))))

  (testing "root replacements remain barriers between batches"
    (let [origin {:old true}
          script (edit/edits->script
                   [[[] :r {:items [0 1 2]}]
                    [[:items 0] :-]
                    [[:items 0] :+ :new]])]
      (is (= {:items [:new 1 2]} (core/patch origin script)))
      (is (= (sequential-patch origin script)
             (core/patch origin script))))))

(deftest segment-aware-string-patch-test
  (testing "character segments are appended directly"
    (let [script (edit/edits->script
                   [[[] :s [2 [:+ "XY"] [:- 1] [:r "ZZ"] 1]]])]
      (is (= "abXYZZf" (core/patch "abcdef" script)))))

  (testing "word segments retain separators, empty tokens, and nesting"
    (let [script (edit/edits->script
                   [[[] :sw [1 [:+ [["x"] ["y"]]] [:- 1]
                             [:r ["z"]] 2]]])]
      (is (= "a x y z d e" (core/patch "a b c d e" script))))
    (let [script (edit/edits->script [[[] :sw [3]]])]
      (is (= "a  b" (core/patch "a  b" script)))))

  (testing "line segments receive exactly one newline between leaves"
    (let [script (edit/edits->script
                   [[[] :sl [1 [:+ ["x" "y"]] [:- 1]
                             [:r ["z"]] 2]]])]
      (is (= "a\nx\ny\nz\nd\ne"
             (core/patch "a\nb\nc\nd\ne" script))))))

(def large-row-gen
  (gen/let [id      gen/int
            scores  (gen/vector gen/int 8 24)
            notes   (gen/vector gen/string 2 8)
            enabled gen/boolean]
    {:id id
     :scores scores
     :notes notes
     :enabled enabled}))

(def large-state-gen
  (gen/let [rows  (gen/vector large-row-gen 24 72)
            queue (gen/vector gen/int 24 72)
            flags (gen/vector gen/int 8 24)
            title gen/string]
    {:rows rows
     :queue (apply list queue)
     :flags (set flags)
     :title title}))

(defn- mutate-string-units
  [units stride]
  (reduce-kv
    (fn [result index unit]
      (if (zero? (mod index stride))
        (case (long (mod (quot index stride) 3))
          0 result
          1 (conj result (str unit "-changed"))
          2 (conj result unit (str "inserted-" index)))
        (conj result unit)))
    []
    units))

(defn- units->string
  [level units]
  (str/join (case level
              :character ""
              :word      " "
              :line      "\n")
            units))

(test/defspec large-string-segment-patch-equivalence-test
  #?(:cljs 10 :cljr 10 :default 30)
  (prop/for-all [values (gen/vector gen/small-integer 128 384)
                 stride (gen/choose 3 11)]
    (let [origin-units (mapv (fn [index value]
                               (str "unit-" index "-" value))
                             (range)
                             values)
          target-units (mutate-string-units origin-units stride)]
      (every?
        (fn [[level edit-op]]
          (let [origin (units->string level origin-units)
                target (units->string level target-units)
                script (core/diff origin target
                                  {:algo             :quick
                                   :str-diff         level
                                   :str-change-limit 0.99
                                   :vec-timeout      nil})]
            (and (= target (core/patch origin script))
                 (= target (sequential-patch origin script))
                 (some #(= edit-op (nth % 1))
                       (edit/get-edits script)))))
        [[:character :s] [:word :sw] [:line :sl]]))))

(test/defspec large-batched-patch-equivalence-test
  #?(:cljs 30 :cljr 30 :default 100)
  (prop/for-all [origin large-state-gen
                 target large-state-gen]
    (let [script     (core/diff origin target {:algo :quick})
          sequential (sequential-patch origin script)
          batched    (core/patch origin script)]
      (and (= target batched)
           (= sequential batched)))))

(test/defspec large-combined-batch-equivalence-test
  #?(:cljs 10 :cljr 10 :default 30)
  (prop/for-all [a large-state-gen
                 b large-state-gen
                 c large-state-gen]
    (let [script     (edit/combine (core/diff a b {:algo :quick})
                                   (core/diff b c {:algo :quick}))
          sequential (sequential-patch a script)
          batched    (core/patch a script)]
      (and (= c batched)
           (= sequential batched)))))

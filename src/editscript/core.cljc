;;
;; Copyright (c) Huahai Yang. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.
;;

(ns editscript.core
  (:require [editscript.edit :as e]
            [editscript.patch :as p]
            [editscript.util.common :as c]
            [editscript.diff.quick]
            [editscript.diff.a-star])
  #?(:clj (:import [editscript.edit EditScript])))

(defn diff
  "Create an editscript to represent the transformations needed to turn a
  Clojure data structure `a` into another Clojure data structure `b`.

  This function accepts any nested Clojure data structures. In Clojure, those
  implement `IPersistentVector`, `IPersistentMap`, `IPersistentList`,
  and `IPersistentSet` will be treated as collections. The same are true for
  the corresponding deftypes in Clojurescript, such as `PersistentVector`,
  `PersistentMap`, and so on. Anything else are treated as atomic values.

  The editscript is represented as a vector of basic operations: add `:+`,
  delete `:-`, and replace `:r`. Each operation also include a path to the
  location of the operation, which is similar to the path vector in `update-in`.
  However, editscript path works for all above four collection types, not just
  associative ones. For `:+` and `:r`, a new value is also required.


  The following options are supported in the option map:

  * `:algo`  chooses the diff algorithm. The value can be `:a-star` (default) or
  `:quick`; `:a-star` algorithm minimize the size of the resulting editscript,
  `:quick` algorithm is much faster, but does not producing diff with minimal size.

  * `:str-diff?` indicates a desire to perform string diff. It  may reduce the
  result size for small changes in long strings, but will incur a slight computation
  cost. The value is a boolean: `true` or `false` (default). When enabled, the diff
  algorithm will perform string diff if the changes are less than 30 percent of the
  string length; otherwise, whole string replacement will be used."
  ([a b]
   (diff a b {:algo :a-star :str-diff? false}))
  ([a b {:keys [algo str-diff?] :or {algo :a-star str-diff? false}}]
   (c/diff-algo a b {:algo algo :str-diff? str-diff?})))

(defn patch
  "Apply the editscript `script` on `a` to produce `b`, assuming the
  script is the results of running  `(diff a b)`, such that
  `(= b (patch a (diff a b)))` is true"
  [a script]
  {:pre [(instance? editscript.edit.EditScript script)]}
  (reduce
    #(p/patch* %1 %2)
    a
    (e/get-edits script)))

(def ^{:arglists '([edits])
       :doc      "Check if the given vector represents valid edits that can be turned
into an EditScript"}
  valid-edits? e/valid-edits?)

(def ^{:arglists '([this that])
       :doc      "Concate that editscript onto this editscript, return the new
editscript"}
  combine e/combine)

(def ^{:arglists '([es])
       :doc      "Report the size of the editscript"}
  get-size e/get-size)

(def ^{:arglists '([es])
       :doc      "Report the edit distance of the editscript, i.e. number of
operations"}
  edit-distance e/edit-distance)

(def ^{:arglists '([es])
       :doc      "Report the edits of the editscript as a vector"}
  get-edits e/get-edits)

(def ^{:arglists '([es])
       :doc      "Report the number of additions in the editscript"}
  get-adds-num e/get-adds-num)

(def ^{:arglists '([es])
       :doc      "Report the number of deletes in the editscript"}
  get-dels-num e/get-dels-num)

(def ^{:arglists '([es])
       :doc      "Report the edits of replacements in the editscript"}
  get-reps-num e/get-reps-num)

(def ^{:arglists '([edits])
       :doc      "Create an EditScript instance from a vector of edits, like those
obtained through calling `get-edits` on an EditScript"}
  edits->script e/edits->script)

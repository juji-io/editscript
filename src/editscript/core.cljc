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
            [editscript.diff.quick :as q]
            [editscript.diff.a-star :as a])
  #?(:clj (:import [editscript.edit EditScript]
                   [clojure.lang MapEntry]
                   [java.util IdentityHashMap])
     :cljr (:import [editscript.edit EditScript]
                   [clojure.lang MapEntry]
                   [System.Runtime.CompilerServices RuntimeHelpers])))

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

  The following options are supported in the option map of the last argument:

  * `:algo`  chooses the diff algorithm. The value can be `:a-star` (default) or
  `:quick`; `:a-star` algorithm minimize the size of the resulting editscript,
  `:quick` algorithm is much faster, but does not producing diff with minimal size.

  * `:str-diff` specifies the granularity of string diffing. It may have one of
  the following values:
       - `:none`  (default), do not perform string diffing, the fastest.
       - `:line`, diff by line.
       - `:word`, diff by word,
       - `:character`, diff by character, the slowest.

  * `:str-change-limit`, a less than `1.0` and greater than `0.0` double value,
  representing percentage (default `0.2`). Only diff string when less than given
  percentage is changed, otherwise replace the string.

  * `:vec-timeout` specifies a timeout in milliseconds (default `1000`),
  shared by nested vector, list, and string comparisons, as they have O(n^2)
  running time. When timed-out, a replacement operation will be used."
  ([a b]
   (diff a b nil))
  ([a b {:keys [algo]
         :or   {algo :a-star}
         :as   opts}]
   (if (= algo :a-star)
     (a/diff a b opts)
     (q/diff a b opts))))

(defn patch
  "Apply the editscript `script` on `a` to produce `b`, assuming the
  script is the results of running  `(diff a b)`, such that
  `(= b (patch a (diff a b)))` is true"
  [a script]
  {:pre [(instance? editscript.edit.EditScript script)]}
  (p/patch-edits a (e/get-edits script)))

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

(defn- size-context
  []
  #?(:clj  (IdentityHashMap.)
     :cljs (js/WeakMap.)
     :cljr (volatile! {})))

(defn- cached-size
  [context data]
  #?(:clj  (.get ^IdentityHashMap context data)
     :cljs (.get context data)
     :cljr (some (fn [[candidate size]]
                   (when (identical? candidate data) size))
                 (get @context (RuntimeHelpers/GetHashCode data)))))

(defn- cache-size!
  [context data size]
  #?(:clj  (.put ^IdentityHashMap context data size)
     :cljs (.set context data size)
     :cljr (vswap! context update (RuntimeHelpers/GetHashCode data)
                   (fnil conj []) [data size]))
  size)

(declare data-nodes*)

(defn- collection-nodes
  ^long [type data]
  (loop [entries       (seq data)
         size          (long 1)
         previous      nil
         previous-size (long 0)
         previous?     false]
    (if entries
      (let [entry      (first entries)
            child      (if (= type :map)
                         (clojure.core/val entry)
                         entry)
            ^long child-size (if (and previous? (identical? previous child))
                               previous-size
                               (data-nodes* child))]
        (recur (next entries)
               (+ size child-size)
               child
               (long child-size)
               true))
      size)))

(defn- data-nodes*
  ^long [data]
  (let [type (e/get-type data)]
    (if (contains? #{:map :set :vec :lst} type)
      (collection-nodes type data)
      1)))

(declare cached-data-nodes*)

(defn- cached-collection-nodes
  ^long [context type data]
  (loop [entries (seq data)
         size    (long 1)]
    (if entries
      (let [entry            (first entries)
            child            (if (= type :map)
                               (clojure.core/val entry)
                               entry)
            ^long child-size (cached-data-nodes* context child)]
        (recur (next entries)
               (+ size child-size)))
      size)))

(defn- cached-data-nodes*
  ^long [context data]
  (let [type (e/get-type data)]
    (if (contains? #{:map :set :vec :lst} type)
      (if-some [cached (cached-size context data)]
        (long cached)
        (let [size (cached-collection-nodes context type data)]
          (cache-size! context data size)
          size))
      1)))

(defn data-nodes
  "Return the number of nodes of a piece of data."
  [data]
  (data-nodes* data))

(defn- get-data
  [data path]
  (loop [[f & r] path
         v       data]
    (let [c (p/vget v f)]
      (if r
        (recur r c)
        c))))

(defn change-ratio
  "Return an approximation of the ratio of changes of an editscript, a double"
  [origin editscript]
  (let [context     (size-context)
        origin-size (cached-data-nodes* context origin)]
    (double
      (/ (reduce
           (fn [^long sum [path op v]]
             (+ sum (case op
                      (:r :+) (cached-data-nodes* context v)
                      (:s :sw :sl) 1
                      :-      (cached-data-nodes*
                                context (get-data origin path)))))
           0 (get-edits editscript))
         origin-size))))

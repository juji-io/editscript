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
            [editscript.diff.a-star :as a]))

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

  Currently, the default diffing algorithm, `:A*` aims to minimize the size of the
  resulting editscript, a faster alternative is `:quick` algorithm, which does
  not producing optimal diffing results. An `:algo` option can be used to choose
  the algorithm."
  ([a b]
   (diff a b {}))
  ([a b {:keys [algo]}]
   (let [algo (or algo :A*)]
     ((case algo
        :A*    a/diff
        :quick q/diff)
      a b))))

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

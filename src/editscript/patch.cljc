;;
;; Copyright (c) Huahai Yang. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.
;;

(ns ^:no-doc editscript.patch
  (:require [editscript.edit :as e]
            [editscript.util.common :as c]
            [clojure.string :as s]))

#?(:clj (set! *warn-on-reflection* true))
#?(:cljr (set! *warn-on-reflection* true))
#?(:clj (set! *unchecked-math* :warn-on-boxed))

(defn vget
  [x p]
  (case (e/get-type x)
    (:map :vec :set) (get x p)
    :lst             (nth x p)))

(defn- vdelete
  [x p]
  (case (e/get-type x)
    ;;NB, there is a special case where dissoc has no effect:
    ;;if p is ##NaN, then p cannot be found in x, for (= ##NaN ##NaN) is false!
    :map (dissoc x p)
    :vec (into (subvec x 0 p) (subvec x (inc ^long p)))
    :set (disj x p)
    :lst (->> (split-at p x)
              (#(concat (nth % 0) (next (nth % 1))))
              (apply list))))

(defn- vadd
  [x p v]
  (case (e/get-type x)
    :map (assoc x p v)
    :vec (if (= p (count x))
           (conj x v)
           (into (conj (subvec x 0 p) v) (subvec x p)))
    :set (conj x v)
    :lst (->> (split-at p x)
              (#(concat (nth % 0) (conj (nth % 1) v)))
              (apply list))))

(defn- sreplace
  [x edits level]
  (let [x  (c/transform-str x level)
        sf (if (= level :character) subs subvec)
        i  (volatile! 0)
        ss (persistent!
             (reduce
               (fn [ss e]
                 (cond
                   (integer? e)     (let [s (sf x @i (+ ^long @i ^long e))]
                                      (vswap! i (partial + e))
                                      (conj! ss s))
                   (= (nth e 0) :-) (do (vswap! i (partial + (nth e 1))) ss)
                   (= (nth e 0) :r) (let [s (nth e 1)]
                                      (vswap! i (partial + (count s)))
                                      (conj! ss s))
                   (= (nth e 0) :+) (conj! ss (nth e 1))))
               (transient [])
               edits))]
    (case level
      :character (apply str ss)
      :word      (s/join " " (flatten ss))
      :line      (s/join "\n" (flatten ss)))))

(defn- vreplace
  [x p v]
  (case (e/get-type x)
    :map (assoc x p v)
    :vec (assoc x p v)
    :set (-> x (disj p) (conj v))
    :lst (->> (split-at p x)
              (#(concat (nth % 0) (conj (rest (nth % 1)) v)))
              (apply list))))

(defn- valter
  [x p o v]
  (case o
    :-  (vdelete x p)
    :+  (vadd x p v)
    :r  (vreplace x p v)
    :s  (vreplace x p (sreplace (vget x p) v :character))
    :sw (vreplace x p (sreplace (vget x p) v :word))
    :sl (vreplace x p (sreplace (vget x p) v :line))))

(defn patch*
  [old [path op value]]
  (letfn [(up [x p o v]
            (let [[f & r] p]
              (if r
                (valter x f :r (up (vget x f) r o v))
                (if (seq p)
                  (valter x f o v)
                  (case o
                    :s  (sreplace x v :character)
                    :sw (sreplace x v :word)
                    :sl (sreplace x v :line)
                    v)))))]
    (up old path op value)))

(defn- streamable-sequence-edits?
  "Return true when direct vector/list edits can be applied from left to right
  without revisiting an already emitted element. Edit indices are relative to
  the collection produced by all preceding edits."
  [source edits]
  (loop [cursor (long 0)
         length (long (count source))
         edits  (seq edits)]
    (if-let [[path op _] (first edits)]
      (let [p (peek path)]
        (if (nat-int? p)
          (let [p (long p)]
            (if (and (<= cursor p)
                     (case op
                       :+             (<= p length)
                       (:- :r :s :sw :sl) (< p length)))
              (recur (long (case op
                             :- p
                             (inc p)))
                     (long (case op
                             :- (dec length)
                             :+ (inc length)
                             length))
                     (next edits))
              false))
          false))
      true)))

(defn- structural-edit?
  [[_ op _]]
  (or (= op :-) (= op :+)))

(defn- append-range!
  [out source start end]
  (loop [out out
         i   (long start)]
    (if (< i ^long end)
      (recur (conj! out (nth source i)) (inc i))
      out)))

(defn- replace-sequence-value
  [source i op value]
  (case op
    :r  value
    :s  (sreplace (nth source i) value :character)
    :sw (sreplace (nth source i) value :word)
    :sl (sreplace (nth source i) value :line)))

(defn- batch-sequence-edits
  "Apply a streamable run of direct sequence edits while copying the source
  sequence only once."
  [source edits]
  (let [source (if (= :vec (e/get-type source)) source (vec source))]
    (loop [source-i (long 0)
           cursor   (long 0)
           out      (transient [])
           edits    (seq edits)]
      (if-let [[path op value] (first edits)]
        (let [p        (long (peek path))
              gap      (long (- p cursor))
              next-i   (long (+ source-i gap))
              out      (append-range! out source source-i next-i)
              remaining (next edits)]
          (case op
            :- (recur (long (inc next-i)) p out remaining)
            :+ (recur next-i (long (inc p)) (conj! out value) remaining)
            (:r :s :sw :sl)
            (recur (long (inc next-i))
                   (long (inc p))
                   (conj! out (replace-sequence-value source next-i op value))
                   remaining)))
        (persistent! (append-range! out source source-i (count source)))))))

(defn- restore-sequence-type
  [original result]
  (case (e/get-type original)
    :vec (with-meta result (meta original))
    :lst (with-meta (reduce conj '() (rseq result)) (meta original))))

(defn- patch-direct-edits
  "Apply edits whose paths all point to direct children of `parent`."
  [parent edits]
  (let [type        (e/get-type parent)
        multiple?   (boolean (next edits))
        structural? (and (= type :vec) (some structural-edit? edits))]
    (if (and multiple?
             (or (= type :lst) structural?)
             (streamable-sequence-edits? parent edits))
      (restore-sequence-type parent (batch-sequence-edits parent edits))
      (reduce
        (fn [x [path op value]]
          (valter x (peek path) op value))
        parent
        edits))))

(defn- update-at-path
  [old path f]
  (let [depth (long (count path))]
    (letfn [(up [x ^long i]
              (if (= i depth)
                (f x)
                (let [p (nth path i)]
                  (valter x p :r (up (vget x p) (long (inc i)))))))]
      (up old (long 0)))))

(defn- same-parent?
  [parent [path _ _]]
  (and (seq path) (= parent (pop path))))

(defn- parent-run-end
  ^long
  [edits start parent]
  (let [n (long (count edits))]
    (loop [i (long (inc (long start)))]
      (if (and (< i n) (same-parent? parent (nth edits i)))
        (recur (inc i))
        i))))

(defn patch-edits
  "Apply a vector of edits, batching consecutive edits with the same parent.
  Root edits remain ordering barriers and are applied individually."
  [old edits]
  (let [n (long (count edits))]
    (case n
      0 old
      1 (patch* old (nth edits 0))
      (loop [result old
             i      (long 0)]
        (if (< i n)
          (let [edit (nth edits i)
                path (nth edit 0)]
            (if (seq path)
              (let [parent (pop path)
                    end    (parent-run-end edits i parent)
                    run    (subvec edits i end)]
                (recur (update-at-path result parent
                                       #(patch-direct-edits % run))
                       (long end)))
              (recur (patch* result edit) (long (inc i)))))
          result)))))

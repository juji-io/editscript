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
            [editscript.util.common :as c])
  #?(:bb   (:import [java.lang StringBuilder])
     :clj  (:import [java.lang StringBuilder]
                    [java.util StringJoiner])
     :cljr (:import [System.Text StringBuilder])))

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

(defn- string-builder
  [capacity]
  #?(:clj  (StringBuilder. (int capacity))
     :cljs (array)
     :cljr (StringBuilder. (int capacity))))

(defn- builder-string
  [builder]
  #?(:clj  (.toString ^StringBuilder builder)
     :cljs (.join builder "")
     :cljr (.ToString ^StringBuilder builder)))

(declare append-delimited-value!)

(defn- append-delimited-segment!
  [builder separator appended? segment]
  (if (vector? segment)
    (let [length (long (count segment))]
      (loop [index     (long 0)
             appended? appended?]
        (if (< index length)
          (recur (long (inc index))
                 (append-delimited-value! builder separator appended?
                                          (nth segment index)))
          appended?)))
    (loop [values    (seq segment)
           appended? appended?]
      (if values
        (recur (next values)
               (append-delimited-value! builder separator appended?
                                        (first values)))
        appended?))))

(defn- append-delimited-value!
  [builder ^String separator appended? value]
  (if (sequential? value)
    (append-delimited-segment! builder separator appended? value)
    (do
      (when appended?
        #?(:clj  (.append ^StringBuilder builder separator)
           :cljs (.push builder separator)
           :cljr (.Append ^StringBuilder builder separator)))
      (let [^String string-value (if (string? value) value (str value))]
        #?(:clj  (.append ^StringBuilder builder string-value)
           :cljs (.push builder string-value)
           :cljr (.Append ^StringBuilder builder string-value)))
      true)))

(defn- append-delimited-range!
  [builder separator appended? values start end]
  (loop [index     (long start)
         appended? appended?]
    (if (< index ^long end)
      (recur (long (inc index))
             (append-delimited-value! builder separator appended?
                                      (nth values index)))
      appended?)))

#?(:bb
   (defn- sreplace-character
     [^String x edits]
     (let [builder  (StringBuilder. (int (count x)))
           source-i (volatile! (long 0))]
       (reduce
         (fn [^StringBuilder builder edit]
           (cond
             (integer? edit)
             (let [segment (subs x @source-i
                                 (+ ^long @source-i ^long edit))]
               (vswap! source-i (partial + edit))
               (.append builder ^String segment))

             (= (nth edit 0) :-)
             (do (vswap! source-i (partial + (nth edit 1))) builder)

             (= (nth edit 0) :r)
             (let [segment (nth edit 1)]
               (vswap! source-i (partial + (count segment)))
               (.append builder ^String segment))

             (= (nth edit 0) :+)
             (.append builder ^String (nth edit 1))))
         builder
         edits)
       (.toString builder)))

   :clj
   (defn- sreplace-character
     [^String x edits]
     (let [sf       subs
           source-i (volatile! (long 0))
           joiner   (reduce
                      (fn [^StringJoiner joiner edit]
                        (cond
                          (integer? edit)
                          (let [segment (sf x @source-i
                                            (+ ^long @source-i ^long edit))]
                            (vswap! source-i (partial + edit))
                            (.add joiner segment))

                          (= (nth edit 0) :-)
                          (do (vswap! source-i (partial + (nth edit 1)))
                              joiner)

                          (= (nth edit 0) :r)
                          (let [segment (nth edit 1)]
                            (vswap! source-i (partial + (count segment)))
                            (.add joiner segment))

                          (= (nth edit 0) :+)
                          (.add joiner (nth edit 1))))
                      (StringJoiner. "")
                      edits)]
       (.toString ^StringJoiner joiner)))

   :default
   (defn- sreplace-character
     [x edits]
     (let [builder    (string-builder (count x))
           edit-count (long (count edits))]
       (loop [edit-index (long 0)
              source-i   (long 0)]
         (if (< edit-index edit-count)
           (let [edit       (nth edits edit-index)
                 next-index (long (inc edit-index))]
             (if (integer? edit)
               (let [end (long (+ source-i (long edit)))]
                 #?(:cljs (.push builder (subs x source-i end))
                    :cljr (.Append ^StringBuilder builder ^String x
                                   (int source-i) (int (- end source-i))))
                 (recur next-index end))
               (let [op      (nth edit 0)
                     segment (nth edit 1)]
                 (case op
                   :- (recur next-index
                             (long (+ source-i (long segment))))
                   :r (do
                        #?(:cljs (.push builder segment)
                           :cljr (.Append ^StringBuilder builder ^String segment))
                        (recur next-index
                               (long (+ source-i
                                        (long (count segment))))))
                   :+ (do
                        #?(:cljs (.push builder segment)
                           :cljr (.Append ^StringBuilder builder ^String segment))
                        (recur next-index source-i))))))
           (builder-string builder))))))

(defn- sreplace-delimited
  [x edits separator capacity]
  (let [builder    (string-builder capacity)
        edit-count (long (count edits))]
    (loop [edit-index (long 0)
           source-i   (long 0)
           appended?  false]
      (if (< edit-index edit-count)
        (let [edit       (nth edits edit-index)
              next-index (long (inc edit-index))]
          (if (integer? edit)
            (let [end (long (+ source-i (long edit)))]
              (recur next-index end
                     (append-delimited-range! builder separator appended?
                                              x source-i end)))
            (let [op      (nth edit 0)
                  segment (nth edit 1)]
              (case op
                :- (recur next-index
                          (long (+ source-i (long segment)))
                          appended?)
                :r (recur next-index
                          (long (+ source-i (long (count segment))))
                          (append-delimited-value! builder separator appended?
                                                   segment))
                :+ (recur next-index source-i
                          (append-delimited-value! builder separator appended?
                                                   segment))))))
        (builder-string builder)))))

(defn- sreplace
  [x edits level]
  (case level
    :character (sreplace-character x edits)
    :word      (sreplace-delimited (c/transform-str x level) edits " " (count x))
    :line      (sreplace-delimited (c/transform-str x level) edits "\n" (count x))))

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

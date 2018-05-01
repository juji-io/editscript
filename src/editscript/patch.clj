;;
;; Copyright (c) Huahai Yang. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.
;;

(ns editscript.patch
  (:require [clojure.set :as set]
            [editscript.edit :as e]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(defn- vget
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
    :vec (vec (concat (subvec x 0 p) (subvec x (inc ^long p))))
    :set (set/difference x #{p})
    :lst (->> (split-at p x)
              (#(concat (first %) (next (last %))))
              (apply list))))

(defn- vadd
  [x p v]
  (case (e/get-type x)
    :map (assoc x p v)
    :vec (vec (concat (conj (subvec x 0 p) v) (subvec x p)))
    :set (conj x v)
    :lst (->> (split-at p x)
              (#(concat (first %) (conj (last %) v)))
              (apply list))))

(defn- vreplace
  [x p v]
  (case (e/get-type x)
    :map (assoc x p v)
    :vec (vec (concat (conj (subvec x 0 p) v) (subvec x (inc ^long p))))
    :set (-> x (set/difference #{p}) (conj v))
    :lst (->> (split-at p x)
              (#(concat (first %) (conj (rest (last %)) v)))
              (apply list))))

(defn- valter
  [x p o v]
  (case o
    :- (vdelete x p)
    :+ (vadd x p v)
    :r (vreplace x p v)))

(defn patch*
  [old [path op value]]
  (letfn [(up [x p o v]
            (let [[f & r] p]
              (if r
                (valter x f :r (up (vget x f) r o v))
                (if (seq p)
                  (valter x f o v)
                  v))))]
    (up old path op value)))

(ns editscript.core
  (:require [clojure.set :as set])
  (:import [clojure.lang Seqable PersistentVector]))

(set! *warn-on-reflection* true)

(defprotocol IEdit
  (add-data [this path value])
  (delete-data [this path])
  (replace-data [this path value]))

(defprotocol IEditScript
  (edit-distance [this] "Report the edit instance")
  (get-edits [this] "Report the edits as a vector")
  (get-adds-num [this] "Report the number of additions")
  (get-dels-num [this] "Report the number of deletions")
  (get-reps-num [this] "Report the number of replacements"))

(deftype EditScript [^:volatile-mutable ^PersistentVector edits
                     ^:volatile-mutable ^long adds-num
                     ^:volatile-mutable ^long dels-num
                     ^:volatile-mutable ^long reps-num]
  :load-ns true

  IEdit
  (add-data [this path value]
    (locking this
      (set! adds-num (inc adds-num))
      (set! edits (conj edits [path ::+ value]))))
  (delete-data [this path]
    (locking this
      (set! dels-num (inc dels-num))
      (set! edits (conj edits [path ::-]))))
  (replace-data [this path value]
    (locking this
      (set! reps-num (inc reps-num))
      (set! edits (conj edits [path ::r value]))))

  IEditScript
  (get-edits [this] edits)
  (get-adds-num [this] adds-num)
  (get-dels-num [this] dels-num)
  (get-reps-num [this] reps-num)
  (edit-distance [this] (+ adds-num dels-num reps-num))

  Seqable
  (seq [this]
    (.seq edits)))

(defmethod print-method EditScript
  [x ^java.io.Writer writer]
  (print-method (get-edits x) writer))

(defn get-type
  [v]
  (cond
    (map? v)    :map
    (vector? v) :vec
    (set? v)    :set
    (list? v)   :lst
    (= ::nil v) :nil
    :else       :val))

(defn- vget
  [x p]
  (case (get-type x)
    (:map :vec :set) (get x p)
    :lst             (nth x p)))

(defn- vdelete
  [x p]
  (case (get-type x)
    ;;NB, there is a special case where dissoc has no effect:
    ;;if p is ##NaN, then p cannot be found in x, for (= ##NaN ##NaN) is false!
    :map (dissoc x p)
    :vec (vec (concat (subvec x 0 p) (subvec x (inc p))))
    :set (set/difference x #{p})
    :lst (->> (split-at p x)
              (#(concat (first %) (next (last %))))
              (apply list))))

(defn- vadd
  [x p v]
  (case (get-type x)
    :map (assoc x p v)
    :vec (vec (concat (conj (subvec x 0 p) v) (subvec x p)))
    :set (conj x v)
    :lst (->> (split-at p x)
              (#(concat (first %) (conj (last %) v)))
              (apply list))))

(defn- vreplace
  [x p v]
  (case (get-type x)
    :map (assoc x p v)
    :vec (vec (concat (conj (subvec x 0 p) v) (subvec x (inc p))))
    :set (-> x (set/difference #{p}) (conj v))
    :lst (->> (split-at p x)
              (#(concat (first %) (conj (rest (last %)) v)))
              (apply list))))

(defn- valter
  [x p o v]
  (case o
    ::- (vdelete x p)
    ::+ (vadd x p v)
    ::r (vreplace x p v)))

(defn patch*
  [old [path op value]]
  (letfn [(up [x p o v]
            (let [[f & r] p]
              (if r
                (valter x f ::r (up (vget x f) r o v))
                (if (seq p)
                  (valter x f o v)
                  v))))]
    (up old path op value)))

(defn patch
  "Apply the editscript `script` on `a` to produce `b`"
  [a script]
  (reduce
   #(patch* %1 %2)
   a
   (get-edits script)))

(comment

  (def a [1 {:a 1} {:b 2} {:c 3}])
  (def b [{:a 1} {:b 3} {:c 4}])
  (editscript.diff.base/diff a b)

  )

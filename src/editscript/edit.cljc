;;
;; Copyright (c) Huahai Yang. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.
;;

(ns editscript.edit
  #?(:clj (:import [clojure.lang PersistentVector IPersistentList IPersistentMap
                    IPersistentSet IPersistentVector])))

(defprotocol IEdit
  (auto-sizing [this value])
  (add-data [this path value])
  (delete-data [this path])
  (replace-data [this path value]))

(defprotocol IEditScript
  (get-size [this] "Report the size of the editscript")
  (set-size [this size] "Set the size, return the script")
  (edit-distance [this] "Report the edit distance, i.e number of operations")
  (get-edits [this] "Report the edits as a vector")
  (get-adds-num [this] "Report the number of additions")
  (get-dels-num [this] "Report the number of deletions")
  (get-reps-num [this] "Report the number of replacements"))

(defprotocol IType
  (get-type [this] "Return a type keyword, :val, :map, :lst, etc."))

(defn nada
  "A special type means 'not present'"
  []
  (reify IType
    (get-type [_] :nil)))

#?(:clj
   (extend-protocol IType
     IPersistentList
     (get-type [_] :lst)

     IPersistentMap
     (get-type [_] :map)

     IPersistentVector
     (get-type [_] :vec)

     IPersistentSet
     (get-type [_] :set)

     nil
     (get-type [_] :val)

     Object
     (get-type [_] :val))

   :cljs
   (extend-protocol IType
     IList
     (get-type [_] :lst)

     IMap
     (get-type [_] :map)

     IVector
     (get-type [_] :vec)

     ISet
     (get-type [_] :set)

     nil
     (get-type [_] :val)

     default
     (get-type [_] :val)))

(defn- sizing*
  [data size]
  (let [up (fn [s] (inc ^long s))]
    (if (#{:vec :lst :map :set} (get-type data))
      (do (vswap! size up)
          (doseq [child data]
            (sizing* child size)))
      (vswap! size up))))

(defn- sizing
  [data]
  (let [size (volatile! 0) ]
    (sizing* data size)
    @size))

(deftype EditScript [^:volatile-mutable ^PersistentVector edits
                     ^boolean auto-sizing?
                     ^:volatile-mutable ^long size
                     ^:volatile-mutable ^long adds-num
                     ^:volatile-mutable ^long dels-num
                     ^:volatile-mutable ^long reps-num]

  IEdit
  (auto-sizing [this value]
    (when auto-sizing?
      (set! size (long (+ 1 size (sizing value))))))
  (add-data [this path value]
    (locking this
      (auto-sizing this value)
      (set! adds-num (inc adds-num))
      (set! edits (conj edits [path :+ value]))))
  (delete-data [this path]
    (locking this
      (set! size (long (+ 1 size)))
      (set! dels-num (inc dels-num))
      (set! edits (conj edits [path :-]))))
  (replace-data [this path value]
    (locking this
      (auto-sizing this value)
      (set! reps-num (inc reps-num))
      (set! edits (conj edits [path :r value]))))

  IEditScript
  (get-size [this] size)
  (set-size [this s] (set! size (long s)) this)
  (get-edits [this] edits)
  (get-adds-num [this] adds-num)
  (get-dels-num [this] dels-num)
  (get-reps-num [this] reps-num)
  (edit-distance [this] (+ adds-num dels-num reps-num)))

#?(:clj (defmethod print-method EditScript
          [x ^java.io.Writer writer]
          (print-method (get-edits x) writer)))

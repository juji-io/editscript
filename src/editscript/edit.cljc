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
  (combine [this that]
    "Concate that editscript onto this editscript, return the new editscript")
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
     List
     (get-type [_] :lst)

     EmptyList
     (get-type [_] :lst)

     Cons
     (get-type [_] :lst)

     PersistentArrayMap
     (get-type [_] :map)

     PersistentHashMap
     (get-type [_] :map)

     PersistentTreeMap
     (get-type [_] :map)

     PersistentVector
     (get-type [_] :vec)

     Subvec
     (get-type [_] :vec)

     MapEntry
     (get-type [_] :vec)

     PersistentHashSet
     (get-type [_] :set)

     PersistentTreeSet
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

(deftype EditScript [^:unsynchronized-mutable ^PersistentVector edits
                     ^boolean auto-sizing?
                     ^:unsynchronized-mutable ^long size
                     ^:unsynchronized-mutable ^long adds-num
                     ^:unsynchronized-mutable ^long dels-num
                     ^:unsynchronized-mutable ^long reps-num]

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
  (combine [this that]
    (EditScript. (into edits (get-edits that))
                 auto-sizing?
                 (+ size (get-size that))
                 (+ adds-num (get-adds-num that))
                 (+ dels-num (get-dels-num that))
                 (+ reps-num (get-reps-num that))))
  (get-size [this] size)
  (set-size [this s] (set! size (long s)) this)
  (get-edits [this] edits)
  (get-adds-num [this] adds-num)
  (get-dels-num [this] dels-num)
  (get-reps-num [this] reps-num)
  (edit-distance [this] (+ adds-num dels-num reps-num)))

#?(:clj (defmethod print-method EditScript
          [x ^java.io.Writer writer]
          (print-method (get-edits x) writer))
   :cljs (extend-protocol IPrintWithWriter
           EditScript
           (-pr-writer [o writer opts]
             (write-all writer (str (get-edits o))))))

;;
;; Copyright (c) Huahai Yang. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.
;;

(ns ^:no-doc editscript.edit
  #?(:clj (:import [clojure.lang PersistentVector IPersistentList IPersistentMap
                    IPersistentSet IPersistentVector MapEntry]
                   [java.util Map$Entry])
	:cljr (:import [clojure.lang PersistentVector IPersistentList IPersistentMap
                    IPersistentSet IPersistentVector MapEntry]
                  )))

(defprotocol IEdit
  (auto-sizing [this path value])
  (add-data [this path value])
  (delete-data [this path])
  (replace-data [this path value])
  (replace-str [this path ops level]))

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

(defprotocol ^:private IEditBuilder
  (-persistent-script! [this]))

(defprotocol IType
  (get-type [this] "Return a type keyword, :val, :map, :lst, etc."))

(def ^:private nada-instance
  (reify IType
    (get-type [_] :nil)))

;; `nil` and `false` are valid added/replacement values. Keep a distinct marker
;; for the one operation (deletion) whose edit vector omits a value entirely.
(def ^:private omitted-value (volatile! nil))

(defn nada
  "Return the singleton sentinel that means 'not present'."
  []
  nada-instance)

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
 
     Map$Entry
     (get-type [_] :val)

     MapEntry
     (get-type [_] :val)

     nil
     (get-type [_] :val)

     String
     (get-type [_] :str)

     Object
     (get-type [_] :val))
	 
   :cljr
   (extend-protocol IType
     IPersistentList
     (get-type [_] :lst)

     IPersistentMap
     (get-type [_] :map)

     IPersistentVector
     (get-type [_] :vec)

     IPersistentSet
     (get-type [_] :set)

     MapEntry
     (get-type [_] :val)

     nil
     (get-type [_] :val)

     String
     (get-type [_] :str)

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
     (get-type [_] :val)

     PersistentHashSet
     (get-type [_] :set)

     PersistentTreeSet
     (get-type [_] :set)

     nil
     (get-type [_] :val)

     string
     (get-type [_] :str)

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

(declare generated-script-size)

(deftype ^:no-doc EditScript [^:unsynchronized-mutable ^PersistentVector edits
                              ^boolean auto-sizing?
                              ^:unsynchronized-mutable ^long size
                              ^:unsynchronized-mutable ^long adds-num
                              ^:unsynchronized-mutable ^long dels-num
                              ^:unsynchronized-mutable ^long reps-num]

  IEdit
  (auto-sizing [this path value]
    (when auto-sizing?
      (when (= -1 size)
        (set! size (long (+ 0 (long (generated-script-size edits))))))
      (set! size
            (long (+ 2 size (sizing path)
                     (if (identical? value omitted-value)
                       0
                       (sizing value))))))
    this)
  (add-data [this path value]
    (locking this
      (auto-sizing this path value)
      (set! adds-num (inc adds-num))
      (set! edits (conj edits [path :+ value]))
      this))
  (delete-data [this path]
    (locking this
      (auto-sizing this path omitted-value)
      (set! dels-num (inc dels-num))
      (set! edits (conj edits [path :-]))
      this))
  (replace-data [this path value]
    (locking this
      (auto-sizing this path value)
      (set! reps-num (inc reps-num))
      (set! edits (conj edits [path :r value]))
      this))
  (replace-str [this path ops level]
    (locking this
      (auto-sizing this path "")
      (set! reps-num (inc reps-num))
      (set! edits (conj edits [path
                               (case level
                                 :character :s
                                 :word      :sw
                                 :line      :sl)
                               ops]))
      this))

  IEditScript
  (combine [this that]
    ;; Both sizes include an outer edits vector; concatenation shares one.
    (EditScript. (into edits (get-edits that))
                 auto-sizing?
                 (dec (+ (get-size this) (get-size that)))
                 (+ adds-num (get-adds-num that))
                 (+ dels-num (get-dels-num that))
                 (+ reps-num (get-reps-num that))))
  (get-size [this]
    (if (= -1 size)
      (locking this
        (if (= -1 size)
          (do
            (set! size
                  (long (+ 0 (long (generated-script-size edits)))))
            size)
          size))
      size))
  (set-size [this s] (set! size (long s)) this)
  (get-edits [_] edits)
  (get-adds-num [_] adds-num)
  (get-dels-num [_] dels-num)
  (get-reps-num [_] reps-num)
  (edit-distance [_] (+ adds-num dels-num reps-num)))

(defn- generated-script-size
  [edits]
  (let [size (volatile! 1)]
    (doseq [edit edits]
      (let [path  (nth edit 0)
            op    (nth edit 1)
            value (nth edit 2 nil)]
        (vswap! size + 2)
        (sizing* path size)
        (case op
          (:+ :r)      (sizing* value size)
          :-           nil
          (:s :sw :sl) (vswap! size inc))))
    @size))

(deftype ^:no-doc EditBuilder [^:unsynchronized-mutable edits
                               ^:unsynchronized-mutable ^long adds-num
                               ^:unsynchronized-mutable ^long dels-num
                               ^:unsynchronized-mutable ^long reps-num]
  IEdit
  (auto-sizing [this _ _]
    this)
  (add-data [this path value]
    (set! adds-num (inc adds-num))
    (set! edits (conj! edits [path :+ value]))
    this)
  (delete-data [this path]
    (set! dels-num (inc dels-num))
    (set! edits (conj! edits [path :-]))
    this)
  (replace-data [this path value]
    (set! reps-num (inc reps-num))
    (set! edits (conj! edits [path :r value]))
    this)
  (replace-str [this path ops level]
    (set! reps-num (inc reps-num))
    (set! edits (conj! edits [path
                              (case level
                                :character :s
                                :word      :sw
                                :line      :sl)
                              ops]))
    this)

  IEditBuilder
  (-persistent-script! [_]
    ;; -1 defers the recursive size fold until a caller actually requests it.
    (->EditScript (persistent! edits) true -1
                  adds-num dels-num reps-num)))

(defn ^:no-doc edit-builder
  "Create the single-owner transient builder used internally by diffing."
  []
  (->EditBuilder (transient []) 0 0 0))

(defn ^:no-doc persistent-script!
  "Consume an internal edit builder and return its immutable edit vector in an
  EditScript. A builder must be finalized exactly once."
  [builder]
  (-persistent-script! builder))

(defn- valid-str-edits?
  [data level]
  (and (vector? data)
       (every? (fn [x]
                 (or (nat-int? x)
                     (and (vector? x)
                          (= 2 (count x))
                          (let [[op y] x]
                            (and
                              (#{:- :r :+} op)
                              (case op
                                :-      (nat-int? y)
                                (:+ :r) (case level
                                          :s        (string? y)
                                          (:sl :sw) (vector? y))))))))
               data)))

(defn- valid-edit?
  [edit]
  (when (vector? edit)
    (let [c (count edit)]
      (when (< 1 c 4)
        (let [[path op data] edit]
          (and (vector? path)
               (#{:- :r :+ :s :sw :sl} op)
               (if (= :- op) (nil? data) (= c 3))
               (if (#{:s :sw :sl} op)
                 (valid-str-edits? data op)
                 true)))))))

(defn valid-edits?
  [edits]
  (when (vector? edits)
    (if (seq edits)
      (every? valid-edit? edits)
      true)))

(defn- count-ops
  [edits]
  (let [adds (volatile! 0)
        dels (volatile! 0)
        reps (volatile! 0)]
    (doseq [edit edits]
      (case (nth edit 1)
        :+           (vswap! adds inc)
        :-           (vswap! dels inc)
        :r           (vswap! reps inc)
        (:s :sw :sl) (vswap! reps inc)))
    [@adds @dels @reps]))

(defn edits->script
  "Create an EditScript instance from a vector of edits, like those obtained
  through calling `get-edits` on an EditScript"
  [edits]
  (assert (valid-edits? edits) "Not a vector of valid edits")
  (let [[adds dels reps] (count-ops edits)]
    (->EditScript edits true (generated-script-size edits)
                  adds dels reps)))


#?(:clj (defmethod print-method EditScript
          [x ^java.io.Writer writer]
          (print-method (get-edits x) writer))
   :cljs (extend-protocol IPrintWithWriter
           EditScript
           (-pr-writer [o writer opts]
             (write-all writer (str (get-edits o))))))

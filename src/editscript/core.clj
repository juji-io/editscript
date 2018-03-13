(ns editscript.core
  (:require [clojure.set :as set])
  (:import [clojure.lang Seqable PersistentVector IPersistentCollection]))

(set! *warn-on-reflection* true)

(defprotocol IEdit
  (add-data [this path value])
  (delete-data [this path])
  (replace-data [this path old new]))

(defprotocol IEditScript
  (diff-data [this a b])
  (patch-data [this a diff])
  (edit-distance [this] "Report the edit instance")
  (get-edits [this] "Report the edits as a vector")
  (get-adds-num [this] "Report the number of additions")
  (get-dels-num [this] "Report the number of deletions")
  (get-reps-num [this] "Report the number of replacements"))

(deftype EditScript [original
                     ^:volatile-mutable ^PersistentVector edits
                     ^:volatile-mutable ^long adds-num
                     ^:volatile-mutable ^long dels-num
                     ^:volatile-mutable ^long reps-num]
  IEdit
  (add-data [this path value]
    (locking this
      (set! adds-num (inc adds-num))
      (set! edits (conj edits [path ::+ value]))))
  (delete-data [this path]
    (locking this
      (set! dels-num (inc dels-num))
      (set! edits (conj edits [path ::-]))))
  (replace-data [this path old new]
    (locking this
      (set! reps-num (inc reps-num))
      (set! edits (conj edits [path ::- old] [path ::+ new]))))

  IEditScript
  (diff-data [this a b])
  (patch-data [this a diff])
  (get-edits [this] edits)
  (get-adds-num [this] adds-num)
  (get-dels-num [this] dels-num)
  (get-reps-num [this] reps-num)
  (edit-distance [this] (+ adds-num dels-num reps-num))

  Seqable
  (seq [this]
    (.seq edits))

  IPersistentCollection
  (count [this]
    (.count edits))
  (cons [this o]
    (.cons edits o))
  (empty [this]
    (EditScript. original [] 0 0 0))
  (equiv [this o]
    (.equiv edits o)))

(defn get-type [v]
  (cond
    (nil? v)                :nil
    (map? v)                :map
    (vector? v)             :vec
    (set? v)                :set
    (and (sequential? v)
         (not (string? v))) :seq
    :else                   :val))

(defn path? [p] (and (vector? p) (::path (meta p))))

(defn- diff-map [script path a b]
  )

(defn diff* [script path a b]
  (let [ta (get-type a)
        tb (get-type b)]
    (case ta
     :nil (add-data script path b)
     :map (case tb
            :nil (delete-data script path)
            :map (diff-map script path a b))
     :vec (case tb
            :nil (delete-data script path))
     :set (case tb
            :nil (delete-data script path))
     :seq (case tb
            :nil (delete-data script path))
     :val (case tb
            :nil (delete-data script path)))))

(defn diff
  "Create an EditScript that represents the difference between `b` and `a`,
  return nil if `a` and `b` are identical"
  [a b]
  (when-not (identical? a b)
    (let [script (->EditScript a [] 0 0 0)
          path   ^::path []]
      (diff* script path a b))))

(defn patch
  "Apply the editscript `es` on `a` to produce `b`"
  [a es])

(comment
  (def a {:a 1 :b 'b :c {:d 3}})
  (def b {:b 'c :c {:d 2} :e 5 :f 6})
  [[[:e] ::+ 5]
   [[:f] ::+ 6]
   [[:a] ::-]
   [[:c :d] ::+ 2]
   [[:b] ::+ 'c]]

  (def c [3 'c {:a 3} 4])
  (def d [3 'c {:b 3} 4])
  [[[2 :a] ::-]
   [[2 :b] ::+ 3]]

  (def e nil)
  (def f {:a 42})
  [[] ::+ {:a 42}]
  (diff e f)

  (def g "abc")
  (def h {:a 42})
  [[] ::+ {:a 42}]

  (def i ["abc" 24 {:a 42}])
  (def j [{:a 42 :b 24} 1 3])
  [[[0] ::-]
   [[0] ::-]
   [[0 :b] ::+ 24]
   [[1] ::+ 1]
   [[2] ::+ 3]]

  (def k {:a 42 :b ["a" "b"]})
  (def l ["a" "b" "c"])
  [[[] ::+ ["a" "b" "c"]]]

  )

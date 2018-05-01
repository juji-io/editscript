;;
;; Copyright (c) Huahai Yang. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.
;;

(ns editscript.util.priority
  (:import [java.util HashMap PriorityQueue]
           [clojure.lang ILookup IPersistentCollection IPersistentMap
            IPersistentStack MapEntry]))

(set! *warn-on-reflection* true)

(defn map-entry [k v]
  (MapEntry/create k v))

(deftype PriorityMap [^PriorityQueue queue
                      ^HashMap map]
  ILookup
  (valAt [this item] (.get map item))

  IPersistentCollection
  (count [this] (.size map))
  (cons [this e] (let [[item priority] e] (.assoc this item priority)))
  (empty [this]
    (.clear queue)
    (.clear map)
    this)
  (equiv [this o] (= map o))

  IPersistentMap
  (assoc [this item priority]
    (if (.containsKey map item)
      (.replace map item priority)
      (.put map item priority))
    (.offer queue (map-entry priority item))
    this)
  (hashCode [this] (.hashCode map))
  (equals [this o] (or (identical? this o) (.equals map o)))
  (containsKey [this item] (.containsKey map item))
  (entryAt [this k] (map-entry k (get this k)))
  (seq [this] (seq (.entrySet map)))
  (without [this item]
    (let [p (.valAt this item)]
      (.remove queue (map-entry p item)))
    (.remove map item))

  IPersistentStack
  (peek [this] (let [[p item] (.peek queue)] [item p]))
  (pop [this]
    (let [[_ item] (.poll queue)]
      (.remove map item))
    this))

(defn priority-map
  "A priority queue that also functions as a map. Backed by Java's mutable
  version of PriorityQueue and HashMap. I only use and tested these functions:
  `assoc`,`pop`, and `peek`. We also do not pay the price of `decrease-key`,
  instead just insert the item again with a new priority. "
  ([]
   (->PriorityMap (PriorityQueue.) (HashMap.)))
  ([item priority]
   (assoc (priority-map) item priority)))

;;
;; Copyright (c) Huahai Yang. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.
;;

(ns ^:no-doc editscript.util.pairing
  #?(:bb (:require [clojure.data.priority-map])
     :clj
     (:import [clojure.lang IPersistentStack IPersistentMap IPersistentCollection]
              [java.io Writer])
	  :cljr
     (:import [clojure.lang IPersistentStack IPersistentMap IPersistentCollection]
              )))

#?(:clj (set! *warn-on-reflection* true))

(defprotocol IHeapNode
  (get-left [this] "Get the left child node")
  (get-right [this] "Get the right sibling node")
  (set-right [this right] "Set the right sibling")
  (add-child [this node] "Add a child to a node"))

(deftype HeapNode [item
                   priority
                   ^:unsynchronized-mutable left
                   ^:unsynchronized-mutable right]
  IHeapNode
  (get-left [_] left)
  (get-right [_] right)
  (set-right [_ r] (set! right r))
  (add-child [this node]
    (when left (set-right node left))
    (set! left node)
    this))

#?(:clj (defmethod print-method HeapNode
          [x ^Writer writer]
          (print-method {:item     (.-item ^HeapNode x)
                         :priority (.-priority ^HeapNode x)
                         :left     (get-left x)
                         :right    (get-right x)}
                        writer)))

(defn merge-nodes
  [^HeapNode a ^HeapNode b]
  (cond
    (nil? a)                          b
    (nil? b)                          a
    (< (.-priority a) (.-priority b)) (add-child a b)
    :else                             (add-child b a)))

(defn insert
  [^HeapNode node item priority]
  (merge-nodes node (->HeapNode item priority nil nil)))

(defn two-pass
  [^HeapNode node]
  (if (or (nil? node) (nil? (get-right node)))
    node
    (let [a node
          b (get-right node)
          n (get-right b)]
      (set-right a nil)
      (set-right b nil)
      (merge-nodes (merge-nodes a b) (two-pass n)))))

(defn- current-node?
  [m ^HeapNode node]
  (let [entry (find m (.-item node))]
    (and entry (= (val entry) (.-priority node)))))

(defn- prune-stale-roots
  "Discard superseded heap roots until the root agrees with the current map.

  Reprioritizing an item inserts a new heap node, leaving the old node in the
  heap.  Those old nodes are removed lazily so they never escape through
  `peek` or get treated as the live item by `pop`."
  [^HeapNode node m]
  (loop [^HeapNode node node
         removed 0]
    (if (and node (not (current-node? m node)))
      (recur (two-pass (get-left node)) (inc removed))
      [node removed])))

#?(:bb nil
   :clj
   (deftype PriorityMap [^:unsynchronized-mutable ^HeapNode heap
                         ^:unsynchronized-mutable map
                         ^:unsynchronized-mutable ^long heap-count
                         ^:unsynchronized-mutable checked]
     IPersistentCollection
     (count [_] (count map))
     (cons [this e]
       (let [[item priority] e]
         (set! map (assoc map item priority))
         (set! heap (insert heap item priority))
         (set! heap-count (inc heap-count))
         (set! checked false)
         this))
     (empty [this]
       (set! heap nil)
       (set! map {})
       (set! heap-count 0)
       (set! checked false)
       this)
     (equiv [this o] (identical? this o))

     IPersistentMap
     (assoc [this item priority]
       (set! map (assoc map item priority))
       (set! heap (insert heap item priority))
       (set! heap-count (inc heap-count))
       (set! checked false)
       this)
     (hashCode [_] (hash map))
     (equals [this o] (identical? this o))
     (containsKey [_ item] (contains? map item))
     (entryAt [_ k] (find map k))
     (seq [_] (seq map))
     (without [this item]
       (set! map (dissoc map item))
       (set! checked false)
       this)

     IPersistentStack
     (peek [_]
       (when (and (not checked) (> heap-count (count map)))
         (let [[node removed] (prune-stale-roots heap map)]
           (set! heap node)
           (set! heap-count (- heap-count ^long removed))))
       (set! checked true)
       (when heap [(.-item heap) (.-priority heap)]))
     (pop [this]
       (when (and (not checked) (> heap-count (count map)))
         (let [[node removed] (prune-stale-roots heap map)]
           (set! heap node)
           (set! heap-count (- heap-count ^long removed))))
       (let [n (two-pass (get-left heap))]
         (set! map (dissoc map (.-item heap)))
         (set! heap n)
         (set! heap-count (dec heap-count))
         (set! checked false)
         this)))
:cljr
   (deftype PriorityMap [^:unsynchronized-mutable ^HeapNode heap
                         ^:unsynchronized-mutable map
                         ^:unsynchronized-mutable ^long heap-count
                         ^:unsynchronized-mutable checked]
     IPersistentCollection
     (count [_] (count map))
     (^IPersistentCollection cons [this e]
       (let [[item priority] e]
         (set! map (assoc map item priority))
         (set! heap (insert heap item priority))
         (set! heap-count (inc heap-count))
         (set! checked false)
         this))
     (empty [this]
       (set! heap nil)
       (set! map {})
       (set! heap-count 0)
       (set! checked false)
       this)
     (equiv [this o] (identical? this o))

     IPersistentMap
     (^IPersistentMap assoc [this item priority]
       (set! map (assoc map item priority))
       (set! heap (insert heap item priority))
       (set! heap-count (inc heap-count))
       (set! checked false)
       this)
     (^clojure.lang.Associative assoc [this item priority]
       (set! map (assoc map item priority))
       (set! heap (insert heap item priority))
       (set! heap-count (inc heap-count))
       (set! checked false)
       this)
     (GetHashCode [_] (hash map))
     (Equals [this o] (identical? this o))
     (containsKey [_ item] (contains? map item))
     (entryAt [_ k] (find map k))
     (seq [_] (seq map))
     (without [this item]
       (set! map (dissoc map item))
       (set! checked false)
       this)

     IPersistentStack
     (peek [_]
       (when (and (not checked) (> heap-count (count map)))
         (let [[node removed] (prune-stale-roots heap map)]
           (set! heap node)
           (set! heap-count (- heap-count ^long removed))))
       (set! checked true)
       (when heap [(.-item heap) (.-priority heap)]))
     (pop [this]
       (when (and (not checked) (> heap-count (count map)))
         (let [[node removed] (prune-stale-roots heap map)]
           (set! heap node)
           (set! heap-count (- heap-count ^long removed))))
       (let [n (two-pass (get-left heap))]
         (set! map (dissoc map (.-item heap)))
         (set! heap n)
         (set! heap-count (dec heap-count))
         (set! checked false)
         this)))
   :cljs
   (deftype PriorityMap [^:mutable ^HeapNode heap
                         ^:mutable map
                         ^:mutable heap-count
                         ^:mutable checked]

     ISeqable
     (-seq [_] (seq map))

     ICollection
     (-conj [this e]
       (let [[item priority] e]
         (set! map (assoc map item priority))
         (set! heap (insert heap item priority))
         (set! heap-count (inc heap-count))
         (set! checked false)
         this))

     IAssociative
     (-assoc [this item priority]
       (set! map (assoc map item priority))
       (set! heap (insert heap item priority))
       (set! heap-count (inc heap-count))
       (set! checked false)
       this)
     (-contains-key? [_ item] (contains? map item))

     IMap
     (-dissoc [this item]
       (set! map (dissoc map item))
       (set! checked false)
       this)

     IStack
     (-peek [_]
       (when (and (not checked) (> heap-count (count map)))
         (let [[node removed] (prune-stale-roots heap map)]
           (set! heap node)
           (set! heap-count (- heap-count removed))))
       (set! checked true)
       (when heap [(.-item heap) (.-priority heap)]))
     (-pop [this]
       (when (and (not checked) (> heap-count (count map)))
         (let [[node removed] (prune-stale-roots heap map)]
           (set! heap node)
           (set! heap-count (- heap-count removed))))
       (let [n (two-pass (get-left heap))]
         (set! map (dissoc map (.-item heap)))
         (set! heap n)
         (set! heap-count (dec heap-count))
         (set! checked false)
         this))))

#?(:bb (def priority-map clojure.data.priority-map/priority-map)
   :default
   (defn priority-map
     "A priority queue that also functions as a map.
  Backed by a pairing heap implementation, and a regular map.
  NB. We do not implement `decrease-key` for the pairing heap,
  instead we insert the item again and discard superseded nodes lazily."
     ([]
      (->PriorityMap nil {} 0 false))
     ([& keyvals]
      {:pre [(even? (count keyvals))]}
      (reduce conj (priority-map) (partition 2 keyvals)))))

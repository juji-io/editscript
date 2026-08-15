;;
;; Copyright (c) Huahai Yang. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.
;;

(ns editscript.util.index
  (:require [editscript.edit :as e])
  #?(:clj (:import [java.io Writer]
                   [java.util IdentityHashMap])))

;; indexing

(defn- collection-type?
  [type]
  (contains? #{:map :set :vec :lst} type))

(defn index-context
  "Create metadata storage that can be shared by indexes over shared data."
  []
  #?(:clj  (IdentityHashMap.)
     :cljs (js/WeakMap.)
     :cljr (volatile! {})))

(defn- cached-metadata
  [context data]
  #?(:clj  (.get ^IdentityHashMap context data)
     :cljs (.get context data)
     :cljr (get @context data)))

(defn- cache-metadata!
  [context data metadata]
  #?(:clj  (.put ^IdentityHashMap context data metadata)
     :cljs (.set context data metadata)
     :cljr (vswap! context assoc data metadata))
  metadata)

(def ^:private leaf-metadata [1 1])

(declare data-metadata)

(defn- collection-metadata
  "Return [subtree-size traversal-span] without constructing index nodes."
  [context type data]
  (loop [entries       (seq data)
         size          1
         span          0]
    (if entries
      (let [entry          (first entries)
            child-value    (if (= type :map)
                             (clojure.core/val entry)
                             entry)
            child-metadata (data-metadata context child-value)
            child-size     (long (nth child-metadata 0))
            child-span     (long (nth child-metadata 1))]
        (recur (next entries)
               (+ size child-size)
               (+ span child-span)))
      [size (+ span size)])))

(defn- data-metadata
  [context data]
  (let [type (e/get-type data)]
    (if (collection-type? type)
      (or (cached-metadata context data)
          (cache-metadata! context data
                           (collection-metadata context type data)))
      leaf-metadata)))

(defn- empty-children
  [type]
  (transient
    (case type
      (:vec :lst) []
      (:map :set) {})))

(defprotocol INode
  (get-path [this] "Get the path to the node from root")
  (get-value [this] "Get the actual data")
  (get-children [this] "Get child nodes in their keyed lookup collection")
  (get-key [this] "Get the key of this node")
  (get-parent [this] "Get the parent node")
  (get-first [this] "Get the first child node")
  (get-last [this] "Get the last child node")
  (get-next [this] "Get the next sibling node")
  (set-next [this node] "Set the next sibling node")
  (get-order [this] "Get the order of this node in traversal")
  (get-size [this] "Get the size of sub-tree, used to estimate cost")
  (children-realized? [this] "Whether this node has realized its children")
  (realize-children [this] "Realize this node's immediate children"))

(declare make-node)

(deftype Node [key
               value
               parent
               context
               ^:unsynchronized-mutable children
               ^:unsynchronized-mutable first
               ^:unsynchronized-mutable last
               ^:unsynchronized-mutable next
               ^long order
               ^long size]
  INode
  (get-path [this]
    (loop [node this
           path ()]
      (if-let [parent (get-parent node)]
        (recur parent (conj path (get-key node)))
        (vec path))))
  (get-key [_] key)
  (get-value [_] value)
  (get-parent [_] parent)
  (get-children [this]
    (realize-children this)
    children)
  (get-first [this]
    (realize-children this)
    first)
  (get-last [this]
    (realize-children this)
    last)
  (get-next [_] next)
  (set-next [_ n] (set! next n))
  (get-order [_] order)
  (get-size [_] size)
  (children-realized? [_]
    (or (not (collection-type? (e/get-type value)))
        (some? children)))
  (realize-children [this]
    (let [type (e/get-type value)]
      (when (and (nil? children) (collection-type? type))
        (let [metadata (data-metadata context value)
              span     (long (nth metadata 1))
              start    (- order (- span size))]
          (loop [entries    (seq value)
                 lookup     (empty-children type)
                 first-node nil
                 last-node  nil
                 child-start start
                 child-index 0]
            (if entries
              (let [entry          (clojure.core/first entries)
                    child-key      (case type
                                     :map (clojure.core/key entry)
                                     :set entry
                                     (:vec :lst) child-index)
                    child-value    (if (= type :map)
                                     (clojure.core/val entry)
                                     entry)
                    child-metadata (data-metadata context child-value)
                    child-span     (long (nth child-metadata 1))
                    child           (make-node context child-key child-value
                                               this child-start child-metadata)
                    lookup'         (case type
                                      (:vec :lst) (conj! lookup child)
                                      (:map :set) (assoc! lookup child-key child))]
                (when last-node (set-next last-node child))
                (recur (clojure.core/next entries)
                       lookup'
                       (or first-node child)
                       child
                       (+ (long child-start) child-span)
                       (inc (long child-index))))
              (do
                (set! children (persistent! lookup))
                (set! first first-node)
                (set! last last-node)))))))
    this))

#?(:clj
   (defmethod print-method Node
     [x ^Writer writer]
     (print-method {:value    (get-value x)
                    :order    (get-order x)
                    :children (get-children x)}
                   writer)))

(defn- make-node
  [context key value parent start metadata]
  (let [size (long (nth metadata 0))
        span (long (nth metadata 1))]
    (->Node key value parent context nil nil nil nil
            (+ (long start) (- span size)) size)))

(defn index
  "Build a lazy indexing tree. Subtree metadata is computed up front, while
  child Nodes and sibling links are created only when their parent is explored.
  A context may be shared when indexing structurally shared inputs."
  ([data]
   (index data (index-context)))
  ([data context]
   (make-node context nil data nil 0 (data-metadata context data))))

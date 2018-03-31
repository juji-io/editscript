(ns editscript.diff.a-star
  (:require [clojure.set :as set]
            [clojure.data.priority-map :as p]
            [editscript.core :refer :all])
  (:import [clojure.lang PersistentVector]))

(set! *warn-on-reflection* true)

(defprotocol INode
  (get-path [this] "Get the path to the node, path is a vector")
  (get-value [this] "Get the actual data")
  (get-children [this] "Get the children nodes")
  (add-child [this node] "Add a child node")
  (get-size [this] "Get the size of subtree, used to estimate cost")
  (set-size [this s] "Set the size of subtree"))

(deftype Node [path
               value
               ^:volatile-mutable ^PersistentVector children
               ^:volatile-mutable ^long size]
  INode
  (get-path [this] path)
  (get-value [this] value)
  (get-children [this] children)
  (add-child [this node] (set! children (conj children node)))
  (get-size [this] size)
  (set-size [this s] (set! size (long s))))

(defmethod print-method Node [x ^java.io.Writer writer]
  (print-method {:path     (get-path x)
                 :value    (get-value x)
                 :children (count (get-children x))
                 :size     (get-size x)}
                writer))

(declare index*)

(defn- index-associative [nodes path data parent]
  (let [node (->Node path data [] 1)]
    (vswap! nodes conj node)
    (add-child parent node)
    (reduce-kv
     (fn [_ k v]
       (index* nodes (conj path k) v node))
     nil
     data)
    (set-size node
              (+ (get-size node)
                 (apply + (map get-size (get-children node)))))))

(defn- index* [nodes path data parent]
  (case (get-type data)
    (:map :vec) (index-associative nodes path data parent)
    :val        (let [node (->Node path data nil 1)]
                  (add-child parent node)
                  (vswap! nodes conj node))))

(defn- index
  "Traverse data to build an indexing vector of Nodes in pre-order,
  and compute size of their subtrees for cost estimation"
  [data]
  (let [nodes (volatile! [])
        root  (->Node [] :root [] 0)]
    (index* nodes [] data root)
    @nodes))

(defn- diag [[x y]] (- y x))

(defn- heuristic [cur goal]
  (Math/abs (- ^long (diag cur) ^long (diag goal))))

(defn- trace [came cur]
  (loop [c cur ops '()]
    (if-let [[prev op] (came c)]
      (recur prev (conj ops op))
      (vec ops))))

(defn- ops [a b gx gy [x y]]
  (cond-> []
    (and (< x gx) (< y gy)) (conj :r)
    (< x gx)                (conj :-)
    (< y gy)                (conj :+)))

(defn- cost [a b [x y] [x' y']]
  (if (and (= (get a x) (get b y))
           (= 1 (- x' x))
           (= 1 (- y' y)))
    0
    1))

(defn- dest [a b [x y] op]
  (case op
    :+ [x (inc y)]
    :- [(inc x) y]
    :r [(inc x) (inc y)]))

(defn- explore [a b cur goal {:keys [open closed came g] :as m} op]
  (let [neighbor (dest a b cur op)]
    (if (closed neighbor)
      m
      (let [tmp-g (+ (get g cur Long/MAX_VALUE)
                     (cost a b cur neighbor))]
        (if (>= tmp-g (get g neighbor Long/MAX_VALUE))
          (assoc m :open (assoc open neighbor Long/MAX_VALUE))
          (assoc m
                 :came (assoc came neighbor [cur op])
                 :g (assoc g neighbor tmp-g)
                 :open (assoc open neighbor
                              (+ tmp-g (heuristic cur goal) ))))))))

(defn A*
  "The algorithm works on the indices of input data"
  [script a b]
  (let [gx   (count a)
        gy   (count b)
        goal [gx gy]]
    (loop [open   (p/priority-map [0 0] (heuristic [0 0] goal))
           closed #{}
           came   {}
           g      {[0 0] 0}]
      (if (empty? open)
        "failed"
        (let [[cx cy :as cur] (key (peek open))]
          (if (= cur goal)
            (trace came cur)
            (let [{:keys [open closed came g]}
                  (reduce
                   (partial explore a b cur goal)
                   {:open   (pop open)
                    :closed (conj closed cur)
                    :came   came
                    :g      g}
                   (ops a b gx gy cur))]
              (recur open closed came g))))))))

(defn diff
  "Create an EditScript that represents the minimal difference between `b` and `a`"
  [a b]
  (let [script (->EditScript [] 0 0 0)]
    (when-not (identical? a b)
      (A* script (index a) (index b)))
    script))

(def a (vec (seq "a")))
(def b (vec (seq "ac")))
(def a (vec (seq "acbdeacbed")))
(def b (vec (seq "acebdabbabed")))
(A* nil a b)

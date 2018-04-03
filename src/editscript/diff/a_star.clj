(ns editscript.diff.a-star
  (:require [clojure.set :as set]
            [clojure.data.priority-map :as p]
            [editscript.core :refer :all])
  (:import [clojure.lang PersistentHashMap]))

;; (set! *warn-on-reflection* true)

(defprotocol INode
  (get-path [this] "Get the path to the node from root, a vector")
  (get-value [this] "Get the actual data")
  (get-order [this] "Get the order number of the node in indices")
  (set-order [this order] "Set the order number of the node in indices")
  (get-children [this] "Get the children nodes as a map")
  (add-child [this node] "Add a child node to the children map")
  (get-size [this] "Get the size of sub-tree, used to estimate cost")
  (set-size [this s] "Set the size of sub-tree"))

(deftype Node [path
               value
               ^:volatile-mutable ^long order
               ^:volatile-mutable ^PersistentHashMap children
               ^:volatile-mutable ^long size]
  INode
  (get-path [this] path)
  (get-value [this] value)
  (get-order [this] order)
  (set-order [this o] (set! order (long o)))
  (get-children [this] children)
  (add-child [this node]
    (set! children (assoc children (last (get-path node)) node)))
  (get-size [this] size)
  (set-size [this s] (set! size (long s))))

(defmethod print-method Node
  [x ^java.io.Writer writer]
  (print-method {:path     (get-path x)
                 :value    (get-value x)
                 :order    (get-order x)
                 :children (map (fn [[k v]] [k (get-order v)]) (get-children x))
                 :size     (get-size x)}
                writer))

(declare index*)

(defn- index-associative
  [nodes path data parent]
  (let [node (->Node path data 0 {} 1)]
    (add-child parent node)
    (reduce-kv
     (fn [_ k v]
       (index* nodes (conj path k) v node))
     nil
     data)
    (vswap! nodes conj node)
    (doto node
      (set-order (count @nodes))
      (set-size (+ (get-size node)
                   (reduce + (map get-size (vals (get-children node)))))))))

(defn- index-value
  [nodes path data parent]
  (let [node (->Node path data 0 nil 1)]
                  (add-child parent node)
                  (vswap! nodes conj node)
                  (set-order node (count @nodes))))

(defn- index*
  [nodes path data parent]
  (case (get-type data)
    (:map :vec) (index-associative nodes path data parent)
    :val        (index-value nodes path data parent)))

(defn- index
  "Traverse data to build an indexing vector of Nodes in post-order,
  and compute the sizes of their sub-trees for cost estimation"
  [data]
  (let [nodes (volatile! [])]
    (index* nodes [] data (->Node [] ::root 0 {} 0))
    @nodes))

(defprotocol IState
  (global [this] "The location in the global state matrix")
  (local [this] "The location in the local state matrix")
  (local? [this] "Is this state local?"))

(defn- heuristic
  "An optimistic (never over-estimate) estimate of cost to reach goal when at (x y).

  For positive goal differential (delta), the optimal editscript length is deletion
  dependent, equals to 2p+delta, where p is number of deletions. Optimistically
  assuming no new deletion will be needed after (x, y), the cost works out to
  be delta-(y-x). The same logic applies to negative delta.

  However, because addition/replacement requires new value to be present in
  editscript, whereas deletion does not, we assign unit cost 2 to
  addition/replacement, 1 to deletion."
  [[x y] [gx gy]]
  (let [delta (- gy gx)
        cost  (Math/abs (- delta (- y x)))]
    (if (< delta 0)
      cost
      (* 2 cost))))

(defn- trace [came cur]
  (println (str "explored: " (count came)))
  (loop [c cur ops '()]
    (if-let [[prev op] (came c)]
      (recur prev (conj ops op))
      (vec ops))))

(defn- explore [a b cur goal
                {:keys [open closed came g] :as m}
                {:keys [op co nb]}]
  (if (closed nb)
    m
    (let [gc    (get g cur Long/MAX_VALUE)
          tmp-g (if (= gc Long/MAX_VALUE) gc (+ gc co))]
      (if (>= tmp-g (get g nb Long/MAX_VALUE))
        (assoc! m :open (assoc open nb Long/MAX_VALUE))
        (assoc! m
                :came (assoc! came nb [cur op])
                :g (assoc! g nb tmp-g)
                :open (assoc open nb (+ tmp-g (heuristic nb goal))))))))

(defn- frontier [a b gx gy [x y]]
  (let [va (get a x)
        vb (get b y)]
    (if (= va vb)
      [{:op := :co 0 :nb [(inc x) (inc y)]}]
      (cond-> []
        (< x gx)       (conj {:op :- :co 1 :nb [(inc x) y]})
        (and (< x gx)
             (< y gy)) (conj {:op :r :co 2 :nb [(inc x) (inc y)]})
        (< y gy)       (conj {:op :+ :co 2 :nb [x (inc y)]})))))

(defn A*
  "A* algorithm, works on the indices of input data"
  [script a b]
  (let [gx   (count a)
        gy   (count b)
        goal [gx gy]]
    (loop [{:keys [open closed came g] :as m}
           (transient
            {:open   (p/priority-map [0 0] (heuristic [0 0] goal))
             :closed (transient #{})
             :came   (transient {})
             :g      (transient {[0 0] 0})})]
      (if (empty? open)
        "failed"
        (let [[cx cy :as cur] (key (peek open))]
          (if (= cur goal)
            (trace (persistent! came) cur)
            (recur (reduce
                    (partial explore a b cur goal)
                    (assoc! m :open (pop open) :closed (conj! closed cur))
                    (frontier a b gx gy cur)))))))))

(defn diff
  "Create an EditScript that represents the minimal difference between `b` and `a`"
  [a b]
  (let [script (->EditScript [] 0 0 0)]
    (when-not (identical? a b)
      (A* script (index a) (index b)))
    script))

(comment

  (def a [{:a 1 :b 2} {:c 2} [4 5 6]])
  (def b [:b {:a 1} {:c 3} [5 7]])
  (index a)
  (index b)

  (def a (vec (seq "acbdeacbed")))
  (def b (vec (seq "acebdabbabed")))
  (A* nil a b)

  (def a (vec (seq "ab")))
  (def b (vec (seq "bc")))
  (A* nil a b)
  )

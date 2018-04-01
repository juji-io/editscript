(ns editscript.diff.a-star
  (:require [clojure.set :as set]
            [clojure.data.priority-map :as p]
            [editscript.core :refer :all])
  (:import [clojure.lang PersistentHashMap]))

(set! *warn-on-reflection* true)

(defprotocol INode
  (get-path [this] "Get the path to the node from root, a vector")
  (get-value [this] "Get the actual data")
  (get-order [this] "Get the order number of the node in indices")
  (get-children [this] "Get the children nodes")
  (add-child [this node] "Add a child node to children map")
  (get-size [this] "Get the size of subtree, used to estimate cost")
  (set-size [this s] "Set the size of subtree"))

(deftype Node [path
               value
               order
               ^:volatile-mutable ^PersistentHashMap children
               ^:volatile-mutable ^long size]
  INode
  (get-path [this] path)
  (get-value [this] value)
  (get-order [this] order)
  (get-children [this] children)
  (add-child [this node]
    (set! children (assoc children (last (get-path node)) node)))
  (get-size [this] size)
  (set-size [this s] (set! size (long s))))

(defmethod print-method Node [x ^java.io.Writer writer]
  (print-method {:path     (get-path x)
                 :value    (get-value x)
                 :order    (get-order x)
                 :children (keys (get-children x))
                 :size     (get-size x)}
                writer))

(declare index*)

(defn- index-associative [nodes path data parent]
  (let [node (->Node path data (count @nodes) {} 1)]
    (vswap! nodes conj node)
    (add-child parent node)
    (reduce-kv
     (fn [_ k v]
       (index* nodes (conj path k) v node))
     nil
     data)
    (set-size node
              (+ (get-size node)
                 (apply + (map get-size (vals (get-children node))))))))

(defn- index* [nodes path data parent]
  (case (get-type data)
    (:map :vec) (index-associative nodes path data parent)
    :val        (let [node (->Node path data (count @nodes) nil 1)]
                  (add-child parent node)
                  (vswap! nodes conj node))))

(defn- index
  "Traverse data to build an indexing vector of Nodes in pre-order,
  and compute the sizes of their sub-trees for cost estimation"
  [data]
  (let [nodes (volatile! [])
        root  (->Node [] ::root 0 {} 0)]
    (index* nodes [] data root)
    @nodes))

(defn- diag [[x y]] (- y x))

(defn- heuristic [cur goal]
  (Math/abs (- ^long (diag goal) ^long (diag cur))))

(defn- trace [came cur]
  (loop [c cur ops '()]
    (if-let [[prev op] (came c)]
      (recur prev (conj ops op))
      (vec ops))))

(defn- explore [a b cur goal
                {:keys [open closed came g] :as m}
                {:keys [op co nt]}]
  (if (closed nt)
    m
    (let [gc    (get g cur Long/MAX_VALUE)
          tmp-g (if (= gc Long/MAX_VALUE) gc (+ gc co))]
      (if (>= tmp-g (get g nt Long/MAX_VALUE))
        (assoc! m :open (assoc open nt Long/MAX_VALUE))
        (assoc! m
                :came (assoc! came nt [cur op])
                :g (assoc! g nt tmp-g)
                :open (assoc open nt (+ tmp-g (heuristic cur goal))))))))

(defn- frontier [a b gx gy [x y]]
  (let [va (get a x)
        vb (get b y)]
    (if (= va vb)
      [{:op := :co 0 :nt [(inc x) (inc y)]}]
      (cond-> []
        (< x gx)       (conj {:op :- :co 1 :nt [(inc x) y]})
        (and (< x gx)
             (< y gy)) (conj {:op :r :co 2 :nt [(inc x) (inc y)]})
        (< y gy)       (conj {:op :+ :co 2 :nt [x (inc y)]})))))

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
  (index a)

  (def a (vec (seq "acbdeacbed")))
  (def b (vec (seq "acebdabbabed")))
  (A* nil a b)

  (def a (vec (seq "abc")))
  (def b (vec (seq "abc")))
  (A* nil a b)
  )

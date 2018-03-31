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
  (Math/abs (- ^long (diag goal) ^long (diag cur))))

(defn- trace [came cur]
  (loop [c cur ops '()]
    (if-let [[prev op] (came c)]
      (recur prev (conj ops op))
      (vec ops))))

(defn- explore [a b cur goal
                {:keys [open closed came g] :as m}
                {:keys [op cost neighbor]}]
  (if (closed neighbor)
    m
    (let [gc    (get g cur Long/MAX_VALUE)
          tmp-g (if (= gc Long/MAX_VALUE) gc (+ gc cost))]
      (if (>= tmp-g (get g neighbor Long/MAX_VALUE))
        (assoc! m :open (assoc open neighbor Long/MAX_VALUE))
        (assoc! m
                :came (assoc! came neighbor [cur op])
                :g (assoc! g neighbor tmp-g)
                :open (assoc open neighbor (+ tmp-g (heuristic cur goal))))))))

(defn- frontier [a b gx gy [x y]]
  (let [va (get a x)
        vb (get b y)]
    (if (= va vb)
      [{:op := :cost 0 :neighbor [(inc x) (inc y)]}]
      (cond-> []
        (< x gx)       (conj {:op :- :cost 1 :neighbor [(inc x) y]})
        (and (< x gx)
             (< y gy)) (conj {:op :r :cost 2 :neighbor [(inc x) (inc y)]})
        (< y gy)       (conj {:op :+ :cost 2 :neighbor [x (inc y)]})))))

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
  (def b (vec (seq "bc")))
  (A* nil a b)
  )

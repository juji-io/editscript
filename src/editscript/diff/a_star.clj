(ns editscript.diff.a-star
  (:require [clojure.set :as set]
            [clojure.data.priority-map :as p]
            [editscript.core :refer :all])
  (:import [clojure.lang PersistentTreeMap]))

;; (set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(defprotocol INode
  (get-path [this] "Get the path to the node from root, a vector")
  (get-value [this] "Get the actual data")
  (get-order [this] "Get the order number of the node in indices")
  (set-order [this order] "set the order number of the node in indices")
  (get-children [this] "Get the children nodes as a map")
  (add-child [this node] "Add a child node to the children map")
  (get-size [this] "Get the size of sub-tree, used to estimate cost")
  (set-size [this s] "Set the size of sub-tree"))

(deftype Node [path
               value
               ^:volatile-mutable ^long order
               ^:volatile-mutable ^PersistentTreeMap children
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
                 :children (mapv (fn [[k v]] [k (get-order v)]) (get-children x))
                 :size     (get-size x)}
                writer))

(declare index*)

(defn- index-associative
  [nodes path data parent]
  (let [node (->Node path data 0 (sorted-map) 1)]
    (add-child parent node)
    (reduce-kv
     (fn [_ k v]
       (index* nodes (conj path k) v node))
     nil
     data)
    (doto node
      (set-order (count @nodes))
      (set-size (+ (get-size node)
                   (reduce + (map get-size (vals (get-children node)))))))
    (vswap! nodes conj node)))

(defn- index-value
  [nodes path data parent]
  (let [node (->Node path data (count @nodes) nil 1)]
    (add-child parent node)
    (vswap! nodes conj node)))

(defn- index*
  [nodes path data parent]
  (case (get-type data)
    (:map :vec) (index-associative nodes path data parent)
    :val        (index-value nodes path data parent)))

(defn- index
  "Traverse data to build an indexing vector of Nodes in post-order,
  compute path, sizes of sub-trees, etc."
  [data]
  (let [nodes (volatile! [])
        dummy (->Node [] ::dummy 0 (sorted-map) 0)]
    (index* nodes [] data dummy)
    @nodes))

(defprotocol IStep
  (operator [this] "Operator to try")
  (current [this] "Actual start location of the step")
  (neighbor [this] "Destination of the step"))

(deftype Step [op cu nb]
  IStep
  (operator [_] op)
  (current [_] cu)
  (neighbor [_] nb))

(defmethod print-method Step
  [x ^java.io.Writer writer]
  (print-method {:op (operator x)
                 :cu (current x)
                 :nb (neighbor x)}
                writer))

(defn- get-step
  [step]
  ((juxt operator current neighbor) step))

(defprotocol IState
  (get-open [this] "Get the open priority queue")
  (set-open [this open] "Set the open priority queue")
  (get-g [this] "Get the g cost map")
  (set-g [this g] "Set the g cost map"))

(deftype State [^:volatile-mutable open
                ^:volatile-mutable g]
  IState
  (get-open [_] open)
  (set-open [this o] (set! open o) this)
  (get-g [_] g)
  (set-g [this got] (set! g got) this))

(defn- get-state
  [state]
  ((juxt get-open get-g) state))

(defn- access-g
  [g cur]
  (get g cur Long/MAX_VALUE))

(declare A*)

(defn- compute-cost
  [ai bi came g [x y :as current] [x' y'] op]
  (let [na    (ai x)
        nb    (bi y)
        sa    (get-size na)
        sb    (get-size nb)
        sb+1  (inc sb)
        gc    (access-g g current)]
    (if (= gc Long/MAX_VALUE)
      gc
      (case op
        := gc
        :- (inc gc)
        :+ (+ gc sb+1)
        :r (if (or (= sa 1) (= sb 1))
             (+ gc sb+1)
             (min (A* na nb came)
                  sb+1))))))

(defn- heuristic
  "An optimistic estimate of the cost to reach goal when at (x y).
  For sequences with positive goal differential (delta), the optimal number of
  edits is deletion dependent, equals to 2p+delta, where p is number of deletions.
  Optimistically assuming no new deletion will be needed after (x, y), the cost
  estimate is delta-(y-x). The same logic applies to negative delta.
  For nested structure, multiple edits may be merged into one.
  Also, because addition/replacement requires new value to be present in
  editscript, whereas deletion does not, we assign estimate differently."
  [[x y] [gx gy]]
  (let [delta (- gy gx)
        cost  (Math/abs (- ^long delta (- ^long y ^long x)))]
    (if (= cost 0)
      0
      (if (< delta 0)
        1
        (inc cost)))))

(defn- explore
  [ai bi came goal state step]
  (let [[open g]                         (get-state state)
        [op [x y :as cu] [x' y' :as nb]] (get-step step)
        tmp-g                            (compute-cost ai bi came g cu nb op)]
    (if (>= tmp-g (access-g g nb))
      (if (open nb)
        state
        (set-open state (assoc open nb Long/MAX_VALUE)))
      (do (vswap! came assoc [(ai x') (bi y')] [[(ai x) (bi y)] op])
          (doto state
            (set-open (assoc open nb (+ tmp-g (heuristic nb goal))))
            (set-g (assoc g nb tmp-g)))))))

(defn- values=?
  [va vb]
  (or (identical? va vb)
      (and (= (type va) (type vb)) (= va vb))))

(defn- frontier
  [ai bi [x y :as cur] [gx gy]]
  (let [na   (get ai x)
        nb   (get bi y)
        va   (get-value na)
        vb   (get-value nb)
        a=b  (values=? va vb)
        x+1  (inc x)
        y+1  (inc y)
        x<gx (< x gx)
        y<gy (< y gy)]
    (if (and x<gx y<gy a=b)
      [(->Step := cur [x+1 y+1])]
      (cond-> []
       (and x<gx y<gy (not a=b)) (conj (->Step :r cur [x+1 y+1]))
       x<gx                      (conj (->Step :- cur [x+1 y]))
       y<gy                      (conj (->Step :+ cur [x y+1]))))))

(defn- children-nodes
  [node]
  (-> node get-children vals vec))

(defn- A*
  [ra rb came]
  (let [ai   (conj (children-nodes ra) ra)
        bi   (conj (children-nodes rb) rb)
        goal [(dec (count ai)) (dec (count bi))]
        init [0 0]]
    (loop [state (->State (p/priority-map init (heuristic init goal)) {init 0})]
      (let [[open g] (get-state state)]
        (if (empty? open)
          "A* fails to find a solution"
          (let [[cur cost] (peek open)]
            (if (= cur goal)
              cost
              (recur (reduce
                      (partial explore ai bi came goal)
                      (set-open state (pop open))
                      (frontier ai bi cur goal))))))))))

(defn- convert-path [trie path]
  path
  )

(defn- update-path [trie path arg2]
  )

(defn- write-script
  [steps script]
  (reduce
   (fn [trie [op na nb]]
     (let [path (get-path na)
           path' (convert-path trie path)
           value (get-value nb)]
       (case op
         :- (do (delete-data script path')
                (update-path trie path :-))
         :r (do (replace-data script path' value)
                (update-path trie path :r))
         :+ (do (add-data script path value)
                (update-path trie path :+))
         nil)
       trie))
   (volatile! {})
   steps))

(defn- trace
  ([came cur]
   (loop [[na' nb' :as c] cur steps '()]
     (let [[[na nb :as prev] op] (@came c)
           steps                 (conj steps [op na nb])]
       (if prev
         (recur prev steps)
         (if (= (count steps) 1)
           [[:r na' nb']]
           (rest steps))))))
  ([came cur script]
   (-> (trace came cur)
       (write-script script))))

(defn diff
  "Create an EditScript that represents the minimal difference between `b` and `a`"
  [a b]
  (let [script (->EditScript [] 0 0 0)]
    (when-not (identical? a b)
      (let [roota       (last (index a))
            rootb       (last (index b))
            came (volatile! {})
            cost (A* roota rootb came)]
        (println "cost is" cost)
        (println "came is"
                 (apply sorted-map (mapcat (fn [[k [prev op]]]
                                             [(mapv get-order k)
                                              [(mapv get-order prev) op]])
                                           @came)))
        (println "explored" (count @came))
        (trace came [roota rootb] script)))
    script))

(comment

  (def a [:a [:b [:c [:d :e] :f]]])
  (def b [:a [:b :c :d] :e])
  (diff a b)


  (def a [:a [:s :t] [:u]])
  (def b [[:s :u] :t :s])
  (diff a b)
  [[[0] :-] [[1 1] :r :u] [[1] :r [:s :u]] [[2 0] :-] [[2] :r :t] [[] :+ :s]]

  (def a [:a [:s :t] :u])
  (def b [[:b] [:s :t :u]])
  (diff a b)

  (def a [[:s :t] [:u]])
  (def b [[:s] :t :s])
  (diff a b)
  [[[0 1] :-] [[0] :r [:s]] [[1 0] :-] [[1] :r :t] [[] :+ :s]]
  [[[0 1] :-] [[0] :r [:s]] [[1 0] :r :t] [[1] :r :s]]

  (def a (vec (seq "abd")))
  (def b (vec (seq "bc")))
  [[[0] :-] [[2] :r \c]]
  (diff a b)

  (def a [[:a :b] :c [:d]])
  (def b [:c [:d] [:a :b]])
  [[[0] :-] [[] :+ [:a :b]]]
  (diff a b)

  (def a [[:a [:b :c] :d] :e :f])
  (def b [:b :c [:e] :f :g])
  [[[0] :r :b] [[1] :r :c] [[2] :+ [:e]] [[] :+ :g]]
  (diff a b)

  (def a [[:u]])
  (def b [:s :t])
  (diff a b)

  (def a [:e [:a :b] :c])
  (def b [:a [:b :c] :d])
  (diff a b)

  (def a [:a [:b :c :d] :e :f])
  (def b [[:b :c :d :e] [:f]])
  (diff a b)

  (def a [[:a] :b [:c [:d] [:e] :f]])
  (def b [[:b] [:c [:e] [:f] :d]])
  (diff a b)

  (editscript.diff.base/diff a b)

  (def a 3)
  (def b 7)
  (diff a b)

  (def a (vec (seq "ab")))
  (def b (vec (seq "bc")))
  (diff a b)
  [[[0] :-] [[] :+ \c]]

  (children-nodes (last (index a)))

  )

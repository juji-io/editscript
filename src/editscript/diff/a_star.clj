(ns editscript.diff.a-star
  (:require [clojure.set :as set]
            [clojure.data.priority-map :as p]
            [editscript.core :refer :all])
  (:import [clojure.lang PersistentTreeMap]))

(set! *warn-on-reflection* true)

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

(defn- value+size [node] ((juxt get-value get-size) node))

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
        head (->Node [] ::head -1 (sorted-map) 0)]
    (index* nodes [] data head)
    @nodes))

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
  (get-global [this] "The global g map used in local search")
  (get-open [this] "Get the open priority queue")
  (set-open [this open] "Set the open priority queue")
  (get-came [this] "Get the succession map")
  (set-came [this came] "Set the succession map")
  (get-g [this] "Get the g cost map")
  (set-g [this g] "Set the g cost map"))

(deftype State [global
                ^:volatile-mutable open
                ^:volatile-mutable came
                ^:volatile-mutable g]
  IState
  (get-global [_] global)
  (get-open [_] open)
  (set-open [this o] (set! open o) this)
  (get-came [_] came)
  (set-came [this c] (set! came c) this)
  (get-g [_] g)
  (set-g [this got] (set! g got) this))

(defn- get-state
  [state]
  ((juxt get-global get-open get-came get-g) state))

(defn- access-g
  [g cur]
  (get g cur Long/MAX_VALUE))

(declare A*)

(defn- child-replace-cost
  [global na nb]
  (let [sa (get-size na) sb (get-size nb)
        r  (if (= sa sb 1)
             (if (= (get-value na) (get-value nb)) 0 2)
             (let [x  (get-order na) y  (get-order nb)
                   x' (- x (dec sa)) y' (- y (dec sb))]
               (- (access-g global [x y]) (access-g global [x' y']))))]
    (println "child-replacement cost of" na "with" nb "is" r)
    r))

(defn- children-nodes [node] (-> node get-children vals vec))

(defn- restart-replace-cost
  [g na nb]
  (let [ai             (conj (children-nodes na) na)
        bi             (conj (children-nodes nb) nb)
        {:keys [cost]} (A* ai bi g)]
    (println "restart-replace-cost for" na "and" nb "is" cost)
    cost))

(defn- compute-cost [ai bi state step]
  (let [[global _ came g]    (get-state state)
        [op [x y :as cu] nb] (get-step step)
        na                   (ai x)         nb (bi y)
        sa                   (get-size na)  sb (get-size nb)
        x'                   (- x (dec sa)) y' (- y (dec sb))
        sb+1                 (inc sb)       gc (access-g g cu)]
    (if (= gc Long/MAX_VALUE)
      gc
      (case op
        := gc
        :- (if global
             (inc gc)
             (let [r (access-g g [x' y])]
               (if (= r Long/MAX_VALUE)
                 r
                 (inc r))))
        :+ (if global
             (+ gc sb+1)
             (let [r (access-g g [x y'])]
               (if (= r Long/MAX_VALUE)
                 r
                 (+ r sb+1))))
        :r (if global
             (+ gc (child-replace-cost global na nb))
             (let [r (access-g g [x' y'])]
               (if (= r Long/MAX_VALUE)
                 r
                 (+ r (if (or (= sa 1) (= sb 1))
                        sb+1
                        (min (restart-replace-cost g na nb)
                             sb+1))))))))))

(defn- explore
  [ai bi goal state step]
  (let [[_ open came g] (get-state state)
        [op cu nb]      (get-step step)]
    (let [tmp-g (compute-cost ai bi state step)]
      (if (>= tmp-g (access-g g nb))
        (if (open nb)
          state
          (set-open state (assoc open nb Long/MAX_VALUE)))
        (doto state
          (set-open (assoc open nb (+ tmp-g (heuristic nb goal))))
          (set-came (assoc came nb [cu op]))
          (set-g (assoc g nb tmp-g)))))))

(defn- frontier
  [ai bi [x y :as cur] [gx gy]]
  (let [na   (get ai x)     nb   (get bi y)
        va   (get-value na) vb   (get-value nb)
        a=b  (or (identical? va vb) (= va vb))
        x+1  (inc x)        y+1  (inc y)
        x<gx (< x gx)       y<gy (< y gy)]
    (cond-> []
      (and x<gx y<gy a=b)       (conj (->Step := cur [x+1 y+1]))
      (and x<gx y<gy (not a=b)) (conj (->Step :r cur [x+1 y+1]))
      x<gx                      (conj (->Step :- cur [x+1 y]))
      y<gy                      (conj (->Step :+ cur [x y+1])))))

(defn- A*
  "A* algorithm, works on the indices of input data"
  ([ai bi]
   (A* ai bi nil))
  ([ai bi global]
   (println "ai is" ai)
   (println "bi is" bi)
   (let [goal [(dec (count ai)) (dec (count bi))]
         init [0 0]]
     (if global
       (println "local goal is" goal)
       (println "goal is" goal))
     (loop [state (->State global
                           (p/priority-map init (heuristic init goal))
                           {}
                           {init 0})]
       (let [[_ open came g] (get-state state)]
         (if global
           (println "local g is" (apply sorted-map (mapcat identity g)))
           (println "g is" (apply sorted-map (mapcat identity g))))
         (if (empty? open)
           "failed"
           (let [[cur cost] (peek open)]
             (if (= cur goal)
               {:cur cur :cost cost :came came}
               (recur (reduce
                       (partial explore ai bi goal)
                       (set-open state (pop open))
                       (frontier ai bi cur goal)))))))))))

(defn- write-script
  [steps script]
  (doseq [[o a b] steps]
    (let [path  (get-path a)
          value (get-value b)]
      (case o
        :- (delete-data script path)
        :r (replace-data script path value)
        :+ (add-data script path value)
        nil))))

(defn- trace
  ([ai bi came cur]
   (loop [c cur steps '()]
     (if-let [[[x y :as prev] op] (came c)]
       (recur prev (conj steps [op (ai x) (bi y)]))
       steps)))
  ([ai bi came cur script]
   (-> (trace ai bi came cur)
       (write-script script))))

(defn diff
  "Create an EditScript that represents the minimal difference between `b` and `a`"
  [a b]
  (let [script (->EditScript [] 0 0 0)]
    (when-not (identical? a b)
      (let [ai                 (index a)
            bi                 (index b)
            {:keys [cur came]} (A* ai bi)]
        (println "came is" (apply sorted-map (mapcat identity came)))
        (trace ai bi came cur script)))
    script))

(comment

  (def a [:a [:s :t] [:u]])
  (def b [[:s :u] :t :s])
  (diff a b)
  [[[0] :-] [[1 1] :r :u] [[1] :r [:s :u]] [[2 0] :-] [[2] :r :t] [[] :+ :s]]
  (def a [:a [:s :t] :u])
  (def b [[:b] [:s :t :u]])
  (diff a b)
  [[[0] :+ :b] [[0] :r [:b]] [[1] :+ :u] [[1] :r [:s :t :u]] [[2] :-]]
  (def a [[:s :t] [:u]])
  (def b [[:s] :t :s])
  (diff a b)
  [[[0 1] :-] [[0] :r [:s]] [[1 0] :-] [[1] :r :t] [[] :+ :s]]
  [[[0 1] :-] [[0] :r [:s]] [[1 0] :r :t] [[1] :r :s]]

  (def a (vec (seq "ab")))
  (def b (vec (seq "bc")))
  (diff a b)
  [[[0] :-] [[] :+ \c]]

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


  )

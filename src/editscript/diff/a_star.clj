(ns editscript.diff.a-star
  (:require [clojure.set :as set]
            [clojure.data.priority-map :as p]
            [editscript.core :refer :all])
  (:import [clojure.lang PersistentTreeMap]))

;; (set! *warn-on-reflection* true)

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
        head (->Node [] ::head -1 (sorted-map) 0)]
    (index* nodes [] data head)
    @nodes))

(defn- heuristic
  "An optimistic estimate of cost to reach goal when at (x y).
  For sequences, with positive goal differential (delta), the optimal number of
  edits is deletion dependent, equals to 2p+delta, where p is number of deletions.
  Optimistically assuming no new deletion will be needed after (x, y), the cost
  estimate is delta-(y-x). The same logic applies to negative delta.
  For nested structure, multiple edits may be merged into one.
  Also, because addition/replacement requires new value to be present in
  editscript, whereas deletion does not, we assign estimate differently. "
  [[x y] [gx gy]]
  (let [delta (- gy gx)
        cost  (Math/abs (- delta (- y x)))]
    (if (< delta 0)
      (if (= cost 0) 0 1)
      (inc cost))))

(defn- value+size [node] ((juxt get-value get-size) node))

(defprotocol IStep
  (operator [this] "Operator to try")
  (current [this] "Actual start location of the step")
  (neighbor [this] "Destination of the step")
  ;; (trail [this] "Local breakdown of the step")
  )

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
  (local? [this] "Is this local search?")
  (get-open [this] "Get the open priority queue")
  (set-open [this open] "Set the open priority queue")
  (get-closed [this] "Get the closed set")
  (set-closed [this closed] "Set the closed set")
  (get-came [this] "Get the succession map")
  (set-came [this came] "Set the succession map")
  (get-g [this] "Get the g cost map")
  (set-g [this g] "Set the g cost map"))

(deftype State [local
                ^:volatile-mutable open
                ^:volatile-mutable closed
                ^:volatile-mutable came
                ^:volatile-mutable g]
  IState
  (local? [_] local)
  (get-open [_] open)
  (set-open [this o] (set! open o) this)
  (get-closed [_] closed)
  (set-closed [this c] (set! closed c) this)
  (get-came [_] came)
  (set-came [this c] (set! came c) this)
  (get-g [_] g)
  (set-g [this got] (set! g got) this))

(defn- get-state
  [state]
  ((juxt local? get-open get-closed get-came get-g) state))

(defn- access-g
  [g cur]
  (get g cur Long/MAX_VALUE))

(declare A*)

(defn- child-replace-cost
  [global na nb]
  (let [x     (get-order na)
        y     (get-order nb)
        start (fn [node order] (- order (-> node get-size dec)))]
    (- (access-g global [x y])
       (access-g global [(start na x) (start nb y)]))))

(defn- children-nodes [node] (-> node get-children vals vec))

(defn- trace-local [ai' bi' came' cur]
  )

(defn- restart-replace-cost
  [na nb]
  (let [ai                      (conj (children-nodes na) na)
        bi                      (conj (children-nodes nb) nb)
        {:keys [cost cur came]} (A* ai bi {:local? true})]
    cost))

(defn- compute-cost [ai bi [x y :as cur] state step]
  (let [[local? open closed came g] (get-state state)
        [op [x' y' :as cu] nb]      (get-step step)
        na                          (ai x')
        nb                          (bi y')
        sb                          (get-size nb)
        gc                          (access-g g cu)]
    (if (= gc Long/MAX_VALUE)
      gc
      (+ gc (case op
              := 0
              :- 1
              :+ (inc sb)
              :r (cond
                   local?     (child-replace-cost g na nb)
                   (= cur cu) (inc sb)
                   :else      (restart-replace-cost na nb)))))))

(defn- explore
  [ai bi cur goal state step]
  (let [[local? open closed came g] (get-state state)
        [op cu nb]               (get-step step)]
    (if (closed nb)
      state
      (let [tmp-g (compute-cost ai bi cur state step)]
        (if (>= tmp-g (access-g g nb))
          (if (open nb)
            state
            (set-open state (assoc open nb Long/MAX_VALUE)))
          (doto state
            #_(set-came state
                        (transient (merge (persistent! came)
                                          (trace-local ai' bi' came' cur))))
            (set-open (assoc open nb (+ tmp-g (heuristic nb goal))))
            (set-came (assoc! came nb [cur op]))
            (set-g (assoc g nb tmp-g))))))))

(defn- frontier
  [ai bi [x y :as cur] [gx gy] state]
  (let [local   (local? state)
        na      (get ai x)
        nb      (get bi y)
        [va sa] (value+size na)
        [vb sb] (value+size nb)
        x'      (- x (dec sa))
        y'      (- y (dec sb))
        cur-    [x' y]
        cur+    [x y']
        curr    [x' y']
        g       (get-g state)
        gc      (access-g g cur)
        g-      (access-g g cur-)
        g+      (access-g g cur+)
        gr      (access-g g curr)
        x+1     (inc x)
        y+1     (inc y)]
    (cond-> []
      (and (< x gx)
           local)       (conj (->Step :- cur [x+1 y]))
      (and (< x gx)
           (not local)) (conj (->Step :- (if (< g- gc) cur- cur) [x+1 y]))
      (and (< x gx)
           (< y gy)
           (= va vb))   (conj (->Step := cur [x+1 y+1]))
      (and (< x gx)
           (< y gy)
           (not= va vb)
           local)       (conj (->Step :r cur [x+1 y+1]))
      (and (< x gx)
           (< y gy)
           (not= va vb)
           (not local)) (conj (->Step :r (if (< gr gc) curr cur) [x+1 y+1]))
      (and (< y gy)
           local)       (conj (->Step :+ cur [x y+1]))
      (and (< y gy)
           (not local)) (conj (->Step :+ (if (< g+ gc) cur+ cur) [x y+1])))))

(defn- initialize
  [state cur]
  (let [[_ open closed _ _] (get-state state)]
    (doto state
      (set-open (pop open))
      (set-closed (conj! closed cur)))))

(defn- A*
  "A* algorithm, works on the indices of input data"
  ([ai bi]
   (A* ai bi {}))
  ([ai bi {:keys [local?]}]
   (let [goal [(dec (count ai)) (dec (count bi))]
         init [0 0]]
     (loop [state (->State local?
                           (p/priority-map init (heuristic init goal))
                           (transient #{})
                           (transient {})
                           {init 0})]
       (let [[_ open closed came g] (get-state state)]
         (println "g is" (apply sorted-map (mapcat identity g)))
         (if (empty? open)
           "failed"
           (let [[cur cost] (peek open)]
             (if (= cur goal)
               {:cur cur :cost cost :came came}
               (recur (reduce
                       (partial explore ai bi cur goal)
                       (initialize state cur)
                       (frontier ai bi cur goal state)))))))))))

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
        (trace ai bi (persistent! came) cur script)))
    script))

(comment

  (def a [:a [:s :t] [:u]])
  (def b [[:s :u] :t :s])
  (diff a b)
  (A* (index a) (index b))
  (index a)
  (index b)

  (def a (vec (seq "acbdeacbed")))
  (def b (vec (seq "acebdabbabed")))
  (diff a b)

  (def a (vec (seq "ab")))
  (def b (vec (seq "bc")))
  (index a)
  (index b)

  [[[0] :editscript.core/-] [[] :editscript.core/+ \c]]
  [[[0] :editscript.core/-] [[2] :editscript.core/r \c]]
  (diff a b)
  [[[0] :editscript.core/-] [[1] :editscript.core/+ \c]]
  [[[0] :editscript.core/-] [[1] :editscript.core/r \c]]
  (editscript.diff.base/diff a b))

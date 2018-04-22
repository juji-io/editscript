(ns editscript.diff.a-star
  (:require [clojure.set :as set]
            [clojure.data.priority-map :as p]
            [editscript.core :refer :all])
  (:import [clojure.lang PersistentTreeMap]))

;; (set! *warn-on-reflection* true)
;; (set! *unchecked-math* :warn-on-boxed)

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
  (current [this] "Start location of the step")
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
  (get-came [this] "Get the local succession map")
  (set-came [this came] "Set the local succession map")
  (get-open [this] "Get the open priority queue")
  (set-open [this open] "Set the open priority queue")
  (get-g [this] "Get the g cost map")
  (set-g [this g] "Set the g cost map"))

(deftype State [^:volatile-mutable came
                ^:volatile-mutable open
                ^:volatile-mutable g]
  IState
  (get-came [_] came)
  (set-came [this c] (set! came c) this)
  (get-open [_] open)
  (set-open [this o] (set! open o) this)
  (get-g [_] g)
  (set-g [this got] (set! g got) this))

(defn- get-state
  [state]
  ((juxt get-came get-open get-g) state))

(defn- access-g
  [g cur]
  (get g cur Long/MAX_VALUE))

(declare A*)

(defn- compute-cost
  [ai bi came g [x y :as current] [x' y'] op]
  (let [na   (ai x)
        nb   (bi y)
        sa   (get-size na)
        sb   (get-size nb)
        sb+1 (inc sb)
        gc   (access-g g current)]
    (+ gc (if (= gc Long/MAX_VALUE)
            0
            (case op
              :=      0
              :-      1
              (:a :i) sb+1
              :r      (if (or (= sa 1) (= sb 1))
                        sb+1
                        (min (A* na nb came)
                             sb+1)))))))

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
  (let [[came' open g]                   (get-state state)
        [op [x y :as cu] [x' y' :as nb]] (get-step step)
        tmp-g                            (compute-cost ai bi came g cu nb op)]
    (if (>= tmp-g (access-g g nb))
      (if (open nb)
        state
        (set-open state (assoc open nb Long/MAX_VALUE)))
      (doto state
        (set-came (assoc came' [(ai x') (bi y')] [[(ai x) (bi y)] op]))
        (set-open (assoc open nb (+ tmp-g (heuristic nb goal))))
        (set-g (assoc g nb tmp-g))))))

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
        x=gx (= x gx)
        x<gx (< x gx)
        y<gy (< y gy)]
    (if (and x<gx y<gy a=b)
      [(conj (->Step := cur [x+1 y+1]))]
      (cond-> []
        (and x<gx y<gy) (conj (->Step :r cur [x+1 y+1])) ; replace
        x<gx            (conj (->Step :- cur [x+1 y]))   ; delete
        (and x=gx y<gy) (conj (->Step :a cur [x y+1]))   ; append at the end
        (and x<gx y<gy) (conj (->Step :i cur [x y+1])))))) ; insert in front

(defn- children-nodes
  [node]
  (-> node get-children vals vec))

(defn- A*
  [ra rb came]
  (if (= (get-size ra) (get-size rb) 1)
    (do (vswap! came assoc [ra rb] {})
        (if (values=? (get-value ra) (get-value rb))
          0
          2))
    (let [ai   (conj (children-nodes ra) ra)
          bi   (conj (children-nodes rb) rb)
          goal [(dec (count ai)) (dec (count bi))]
          init [0 0]]
      (loop [state (->State {}
                            (p/priority-map init (heuristic init goal))
                            {init 0})]
        (let [[came' open g] (get-state state)]
          (if (empty? open)
            (throw (ex-info "A* fails to find a solution" {:ra ra :rb rb}))
            (let [[cur cost] (peek open)]
              (if (= cur goal)
                (let [end [ra rb]]
                  (vswap! came assoc end came')
                  ;; (vswap! came assoc end (adjust-append came' end))
                  cost)
                (recur (reduce
                        (partial explore ai bi came goal)
                        (set-open state (pop open))
                        (frontier ai bi cur goal)))))))))))

(defn- adjust-delete-insert
  [trie op path]
  (loop [newp     []
         prev     []
         [k & ks] path]
    (if k
      (let [d (get-in @trie (conj prev ::delta) 0)]
        (recur (conj newp (if (int? k) (+ k d) k))
               (conj prev k)
               ks))
      (let [l    (last newp)
            p    (-> newp butlast vec)
            seen (conj p ::delta)
            d    (get-in @trie seen 0)]
        (vswap! trie assoc-in seen (case op :- (dec d) :i (inc d) d))
        newp))))

(defn- adjust-append
  [trie op node path]
  ;; path
  (if (= op :a)
    (conj path (+ (-> node get-children count)
                  (get-in @trie (conj path ::delta) 0)))
    path))

(defn- convert-path
  [trie op node path]
  ;; path
  (->> path
       (adjust-delete-insert trie op)
       (adjust-append trie op node)))

(defn- write-script
  [steps script]
  (reduce
   (fn [trie [op na nb]]
     (let [path  (convert-path trie op na (get-path na))
           value (get-value nb)]
       (case op
         :-      (delete-data script path)
         :r      (replace-data script path value)
         (:a :i) (add-data script path value)
         nil)
       trie))
   (volatile! {})
   steps))

(defn- trace*
  [came cur steps]
  (if-let [m (came cur)]
    (loop [v (m cur)]
      (if v
        (let [[[na nb :as prev] op] v]
          (if (came prev)
            (trace* came prev steps)
            (vswap! steps conj [op na nb]))
          (recur (m prev)))
        steps))
    steps))

(defn- trace
  ([came [ra rb :as cur]]
   (if (seq (came cur))
     @(trace* came cur (volatile! '()))
     [[:r ra rb]]))
  ([came cur script]
   (-> (trace came cur)
       (write-script script))))

(defn diff
  "Create an EditScript that represents the minimal difference between `b` and `a`"
  [a b]
  (let [script (->EditScript [] 0 0 0)]
    (when-not (identical? a b)
      (let [roota (last (index a))
            rootb (last (index b))
            came  (volatile! {})
            cost  (A* roota rootb came)]
        ;; (println "cost is" cost)
        ;; (println "came is" (map (fn [[k v]]
        ;;                           {(map get-order k)
        ;;                            (mapcat (fn [[k [p o]]]
        ;;                                      {(map get-order k)
        ;;                                       [(map get-order p) o]}) v)}) @came))
        ;; (println "explored" (reduce + (map count (vals @came))))
        (trace @came [roota rootb] script)))
    script))

(comment

  (def a 1)
  (index a)
  (def b 2)
  [[[] :r 2]]
  (patch a (diff a b))

  )

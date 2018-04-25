(ns editscript.diff.a-star
  (:require [clojure.set :as set]
            [clojure.data.priority-map :as p]
            [editscript.core :refer :all])
  (:import [clojure.lang PersistentVector PersistentHashMap]
           [java.io Writer]))

;; (set! *warn-on-reflection* true)
;; (set! *unchecked-math* :warn-on-boxed)

;; indexing

(defprotocol INode
  (get-path [this] "Get the path to the node from root")
  (get-value [this] "Get the actual data")
  (get-children [this] "Get the children ")
  (add-child [this node] "Add a child node")
  (get-size [this] "Get the size of sub-tree, used to estimate cost")
  (set-size [this s] "Set the size of sub-tree"))

(defn- get-key
  [node]
  (-> node get-path peek))

(deftype Node [^PersistentVector path
               value
               ^:volatile-mutable ^PersistentHashMap children
               ^:volatile-mutable ^long size]
  INode
  (get-path [this] path)
  (get-value [this] value)
  (get-children [this] children)
  (add-child [this node]
    (set! children (assoc children (get-key node) node))
    this)
  (get-size [this] size)
  (set-size [this s] (set! size (long s)) this))

(defmethod print-method Node
  [x ^Writer writer]
  (print-method {:path     (get-path x)
                 :value    (get-value x)
                 :children (mapv (fn [[k v]] [k v]) (get-children x))
                 :size     (get-size x)}
                writer))

(declare index*)

(defn- index-associative
  [path data parent]
  (let [node (->Node path data {} 1)]
    (add-child parent node)
    (reduce-kv
     (fn [_ k v]
       (index* (conj path k) v node))
     nil
     data)
    (set-size node (+ (get-size node) (->> (get-children node)
                                           vals
                                           (map get-size)
                                           (reduce +))))))

(defn- index-value
  [path data parent]
  (add-child parent (->Node path data nil 1)))

(defn- index*
  [path data parent]
  (case (get-type data)
    (:map :vec) (index-associative path data parent)
    :val        (index-value path data parent)))

(defn- index
  "Traverse data to build an indexing vector of Nodes in post-order,
  compute path, sizes of sub-trees, etc. for each Node"
  [data]
  (let [dummy (->Node [] ::dummy {} 0)]
    (index* [] data dummy)))

;; diffing

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
  [x ^Writer writer]
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

(declare diff*)

(defn- compute-cost
  [na nb came g cur op]
  (let [sa   (get-size na)
        sb   (get-size nb)
        sb+1 (inc sb)
        gc   (access-g g cur)]
    (+ gc (if (= gc Long/MAX_VALUE)
            0
            (case op
              :=      0
              :-      1
              (:a :i) sb+1
              :r      (if (or (= sa 1) (= sb 1))
                        sb+1
                        (diff* na nb came)))))))

(defn- heuristic
  "An optimistic estimate of the cost to reach goal when at (x y).
  For sequences with positive goal differential (delta), the optimal number of
  edits is deletion dependent, equals to 2p+delta, where p is number of deletions.
  Optimistically assuming no new deletion will be needed after (x, y), the cost
  estimate is delta-(y-x). The same logic applies to negative delta.
  For nested structure, multiple edits may be merged into one.
  Also, because addition/replacement requires new value to be present in
  editscript, whereas deletion does not, we assign estimate differently."
  [type [x y] [gx gy]]
  (case type
    :map 0
    :vec (let [delta (- gy gx)
               cost  (Math/abs (- ^long delta (- ^long y ^long x)))]
           (if (= cost 0)
             0
             (if (< delta 0)
               1
               (inc cost))))))

(defn- get-node
  [x gx ra ca]
  (if (= gx x)
    ra
    (ca x)))

(defn- connect-nodes
  [type came x' y' gx gy ra rb ca cb na nb op]
  (case type
    :vec (assoc came
                [(get-node x' gx ra ca) (get-node y' gy rb cb)]
                [[na nb] op])
    :map (let [neighborb (if (= y' 0) ())]
           (assoc came
                 [(get-node x' gx ra ca)
                  (if () ())]))))

(defn- explore
  [type ra rb ca cb came [gx gy :as goal] state step]
  (let [[came' open g]                     (get-state state)
        [op [x y :as cur] [x' y' :as nbr]] (get-step step)
        na                                 (get-node x gx ra ca)
        nb                                 (get-node y gy rb cb)
        tmp-g                              (compute-cost na nb came g cur op)]
    (if (>= tmp-g (access-g g nbr))
      (if (open nbr)
        state
        (set-open state (assoc open nbr Long/MAX_VALUE)))
      (doto state
        (set-came (connect-nodes type came' x' y' gx gy ra rb ca cb na nb op))
        (set-open (assoc open nbr (+ tmp-g (heuristic type nbr goal))))
        (set-g (assoc g nbr tmp-g))))))

(defn- values=?
  [va vb]
  (or (identical? va vb)
      (and (= (get-type va) (get-type vb) :val) (= va vb))))

(defn- vec-frontier
  [na nb [x y :as cur] [gx gy]]
  (let [a=b  (values=? (get-value na) (get-value nb))
        x+1  (inc x)
        y+1  (inc y)
        x=gx (= x gx)
        x<gx (< x gx)
        y<gy (< y gy)]
    (if (and x<gx y<gy a=b)
      [(->Step := cur [x+1 y+1])]
      (cond-> []
        (and x<gx y<gy) (conj (->Step :r cur [x+1 y+1])) ; replace
        x<gx            (conj (->Step :- cur [x+1 y]))   ; delete
        (and x=gx y<gy) (conj (->Step :a cur [x y+1]))   ; append at the end
        (and x<gx y<gy) (conj (->Step :i cur [x y+1])))))) ; insert in front

(defn- map-frontier
  [ra rb na nb [x y :as cur] [gx gy]]
  (let [va   (get-value na)
        mb   (get-value rb)
        ka   (get-key na)
        kb   (get-key nb)]
    (if (and (< x gx) (= y 0))
      (if (contains? mb ka)
        [(->Step (if (values=? va (mb ka))
                   :=
                   :r)
                 cur [(inc x) 0])]
        [(->Step :- cur [(inc x) 0])])
      (if (contains? (get-value ra) kb)
        [(->Step := cur [gx (inc y)])]
        [(->Step :a cur [gx (inc y)])]))))

(defn- frontier
  [type ra rb ca cb [x y :as cur] [gx gy :as goal]]
  (let [na (get-node x gx ra ca)
        nb (get-node y gy rb cb)]
    (case type
      :vec (vec-frontier na nb cur goal)
      :map (map-frontier ra rb na nb cur goal))))

(defn- A*
  [type ra rb came]
  (let [ca   (get-children ra)
        cb   (get-children rb)
        goal [(count ca) (count cb)]
        init [0 0]]
    (loop [state (->State {}
                          (p/priority-map init (heuristic type init goal))
                          {init 0})]
      (let [[came' open g] (get-state state)]
        (if (empty? open)
          (throw (ex-info "A* fails to find a solution" {:ra ra :rb rb}))
          (let [[cur cost] (peek open)]
            (if (= cur goal)
              (let [end [ra rb]]
                (vswap! came assoc end came')
                cost)
              (recur (reduce
                      (partial explore type ra rb ca cb came goal)
                      (set-open state (pop open))
                      (frontier type ra rb ca cb cur goal))))))))))

(defn- diff*
  [ra rb came]
  (let [sizeb  (get-size rb)
        typea  (-> ra get-value get-type)
        update #(vswap! came assoc [ra rb] {})]
    (cond
      (= 1 sizeb (get-size ra))
      (do (update)
          (if (values=? (get-value ra) (get-value rb))
            0
            2))
      (= typea (-> rb get-value get-type))
      (A* typea ra rb came)
      :else
      (do (update)
          (inc sizeb)))))

;; generating editscript

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
      (let [seen (conj (if (seq newp) (pop newp) newp) ::delta)
            d    (get-in @trie seen 0)]
        (vswap! trie assoc-in seen (case op :- (dec d) :i (inc d) d))
        newp))))

(defn- adjust-append
  [trie op node path]
  (if (= op :a)
    (conj path (+ (-> node get-children count)
                  (get-in @trie (conj path ::delta) 0)))
    path))

(defn- convert-path
  [trie op node path]
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
      (let [roota (index a)
            rootb (index b)
            came  (volatile! {})
            cost  (diff* roota rootb came)]
        (println "cost is" cost)
        (println "explored" (reduce + (map count (vals @came))))
        (trace @came [roota rootb] script)))
    script))

(comment

  (def a {:a {:o 4} :b 'b})
  (index a)
  (def b {:a {:o 3} :b 'c :c 42})
  (index b)
  (diff a b)
  (diff a b)
  (patch a (diff a b))

  )

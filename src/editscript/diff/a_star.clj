(ns editscript.diff.a-star
  (:require [clojure.set :as set]
            [clojure.data.priority-map :as p]
            [editscript.core :refer :all])
  (:import [clojure.lang PersistentVector]
           [java.io Writer]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

;; indexing

(defprotocol INode
  (get-path [this] "Get the path to the node from root")
  (get-value [this] "Get the actual data")
  (get-children [this] "Get all children node in a map")
  (add-child [this node] "Add a child node")
  (get-key [this] "Get the key of this node")
  (get-first [this] "Get the first child node")
  (get-last [this] "Get the last child node")
  (get-next [this] "Get the next sibling node")
  (set-next [this node] "Set the next sibling node")
  (^long get-index [this] "Get the index of this node among siblings")
  (set-index [this i] "Set the index of this node among siblings")
  (^long get-size [this] "Get the size of sub-tree, used to estimate cost")
  (set-size [this s] "Set the size of sub-tree"))

(deftype Node [^PersistentVector path
               value
               ^:volatile-mutable children
               ^:volatile-mutable first
               ^:volatile-mutable last
               ^:volatile-mutable next
               ^:volatile-mutable ^long index
               ^:volatile-mutable ^long size]
  INode
  (get-path [this] path)
  (get-key [this] (-> this get-path peek))
  (get-value [this] value)
  (get-children [this] children)
  (get-first [this] first)
  (get-last [this] last)
  (get-next [this] next)
  (set-next [this n] (set! next n))
  (get-index [this] index)
  (set-index [this i] (set! index (long i)))
  (get-size [this] size)
  (set-size [this s] (set! size (long s)) this)
  (add-child [this node]
    (set! children (assoc children (get-key node) node))
    (when last
      (set-next last node)
      (set-index node (inc (get-index last))))
    (when-not first (set! first node))
    (set! last node)
    node))

(defmethod print-method Node
  [x ^Writer writer]
  (print-method {:path     (get-path x)
                 :value    (get-value x)
                 :children (mapv (fn [[k v]] [k (get-size v)]) (get-children x))
                 :first    (get-first x)
                 :last     (get-last x)
                 :next     (get-next x)
                 :index    (get-index x)
                 :size     (get-size x)}
                writer))

(declare index*)

(defn- index-associative
  [path data parent]
  (let [node (->Node path data {} nil nil nil 0 1)]
    (add-child parent node)
    (reduce-kv
     (fn [_ k v]
       (index* (conj path k) v node))
     nil
     data)
    (let [^long cs (->> (get-children node) vals (map get-size) (reduce +))]
      (set-size node (+ (get-size node) cs)))))

(defn- index-value
  [path data parent]
  (add-child parent (->Node path data nil nil nil nil 0 1)))

(defn- index*
  [path data parent]
  (case (get-type data)
    (:map :vec) (index-associative path data parent)
    :val        (index-value path data parent)))

(defn- index
  "Traverse data to build an indexing tree of Nodes in post-order,
  compute path, sizes of sub-trees, siblings, and so on for each Node"
  [data]
  (index* [] data (->Node [] ::dummy {} nil nil nil 0 0)))

;; diffing

(defprotocol IStep
  (operator [this] "Operator to try")
  (current [this] "Starting pair of nodes")
  (neighbor [this] "Destination pair of nodes"))

(deftype Step [op cur nbr]
  IStep
  (operator [_] op)
  (current [_] cur)
  (neighbor [_] nbr))

(defmethod print-method Step
  [x ^Writer writer]
  (print-method {:op (operator x)
                 :cur (current x)
                 :nbr (neighbor x)}
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
  [[na nb :as cur] came g op]
  (let [sa       (get-size na)
        sb       (get-size nb)
        sb+1     (inc ^long sb)
        ^long gc (access-g g cur)
        ^long co (if (= gc Long/MAX_VALUE)
                   0
                   (case op
                     :=      0
                     :-      1
                     (:a :i) sb+1
                     :r      (if (or (= sa 1) (= sb 1))
                               sb+1
                               (diff* na nb came))))]
    (+ gc co)))

(defn- heuristic
  "An optimistic estimate of the cost to reach goal when at (x y).
  For sequences with positive goal differential (delta), the optimal number of
  edits is deletion dependent, equals to 2p+delta, where p is number of deletions.
  Optimistically assuming no new deletion will be needed after (x, y), the number
  of edits is delta-(y-x). The same logic applies to negative delta.
  For nested structure, multiple edits may be merged into one.
  Also, because addition/replacement requires new value to be present in
  editscript, whereas deletion does not, we assign estimate differently."
  [type [na nb] [ra rb] [gx gy]]
  (case type
    :map 0
    :vec 0 #_(let [x     (if (identical? ra na) gx (get-index na))
               y     (if (identical? rb nb) gy (get-index nb))
               delta (- ^long gy ^long gx)
               cost  (Math/abs (- delta (- ^long y ^long x)))]
           (if (= cost 0)
             0
             (if (<= delta 0)
               1
               cost)))))

(defn- explore
  [type end came goal state step]
  (let [[came' open g]                         (get-state state)
        [op [na nb :as cur] [na' nb' :as nbr]] (get-step step)
        tmp-g                                  (compute-cost cur came g op)]
    (if (>= ^long tmp-g ^long (access-g g nbr))
      (if (open nbr)
        state
        (set-open state (assoc open nbr Long/MAX_VALUE)))
      (doto state
        (set-came (assoc came' [na' nb'] [[na nb] op]))
        (set-open (assoc open nbr
                         (+ ^long tmp-g ^long (heuristic type nbr end goal))))
        (set-g (assoc g nbr tmp-g))))))

(defn- values=?
  [va vb]
  (or (identical? va vb)
      (and (= (get-type va) (get-type vb) :val) (= va vb))))

(defn- next-node
  [na ra]
  (or (get-next na) ra))

(defn- vec-frontier
  [[ra rb :as end] [na nb :as cur]]
  (let [a=b  (values=? (get-value na) (get-value nb))
        x=gx (identical? na ra)
        x<gx (not x=gx)
        y<gy (not (identical? nb rb))
        na'  (next-node na ra)
        nb'  (next-node nb rb)]
    (cond-> []
      (and x<gx y<gy a=b) (conj (->Step := cur [na' nb']))
      x<gx                (conj (->Step :- cur [na' nb]))  ; delete
      (and x<gx y<gy)     (conj (->Step :r cur [na' nb'])) ; replace
      (and x=gx y<gy)     (conj (->Step :a cur [na nb']))  ; append at the end
      (and x<gx y<gy)     (conj (->Step :i cur [na nb']))))) ; insert in front

(defn- map-frontier
  [[ra rb :as end] [na nb :as cur]]
  (let [va   (get-value na)
        mb   (get-value rb)
        x<gx (not (identical? na ra))
        y<gy (not (identical? nb rb))
        ka   (get-key na)
        kb   (get-key nb)
        na'  (next-node na ra)
        nb'  (next-node nb rb)
        nb'' (or ((get-children rb) (get-key na')) nb)]
    (if (and x<gx y<gy)
      (if (contains? mb ka)
        [(->Step (if (values=? va (mb ka)) := :r) cur [na' nb''])]
        [(->Step :- cur [na' nb])])
      (if (contains? (get-value ra) kb)
        [(->Step := cur [ra nb'])]
        [(->Step :a cur [ra nb'])]))))

(defn- frontier
  [type end cur]
  (case type
    :vec (vec-frontier end cur)
    :map (map-frontier end cur)))

(defn- A*
  [type ra rb came]
  (let [end  [ra rb]
        goal [(-> ra get-children count) (-> rb get-children count)]
        init [(get-first ra) (get-first rb)]]
    (loop [state (->State {}
                          (p/priority-map init (heuristic type init end goal))
                          {init 0})]
      (let [[came' open g] (get-state state)]
        ;; (println "open:" (mapv (fn [[k v]] [(map get-value k) v]) open))
        ;; (println "g:" (mapv (fn [[k v]] [(map get-value k) v]) g))
        (if (empty? open)
          (throw (ex-info "A* fails to find a solution" {:ra ra :rb rb}))
          (let [[cur cost] (peek open)]
            (if (= cur end)
              (do (vswap! came assoc end came')
                  cost)
              (recur (reduce
                      (partial explore type end came goal)
                      (set-open state (pop open))
                      (frontier type end cur))))))))))

(defn- diff*
  [ra rb came]
  (let [^long sizeb (get-size rb)
        typea       (-> ra get-value get-type)
        update      #(vswap! came assoc [ra rb] {})]
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
          (inc ^long sizeb)))))

;; generating editscript

(defn- adjust-delete-insert
  [trie op path]
  (loop [newp     []
         prev     []
         [k & ks] path]
    (if k
      (let [^long d (get-in @trie (conj prev ::delta) 0)]
        (recur (conj newp (if (int? k) (+ ^long k d) k))
               (conj prev k)
               ks))
      (let [seen    (conj (if (seq newp) (pop newp) newp) ::delta)
            ^long d (get-in @trie seen 0)]
        (vswap! trie assoc-in seen (case op :- (dec d) :i (inc d) d))
        newp))))

(defn- adjust-append
  [trie op na nb path]
  (if (= op :a)
    (case (-> na get-value get-type)
      :vec (conj path (let [^long d (get-in @trie (conj path ::delta) 0)]
                        (+ d (-> na get-children count))))
      :map (conj path (get-key nb)))
    path))

(defn- convert-path
  [trie op na nb path]
  (->> path
       (adjust-delete-insert trie op)
       (adjust-append trie op na nb)))

(defn- write-script
  [steps script]
  (reduce
   (fn [trie [op na nb]]
     (let [path  (convert-path trie op na nb (get-path na))
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
    (if (seq m)
      (loop [v (m cur)]
        (if v
          (let [[[na nb :as prev] op] v]
            (if (came prev)
              (trace* came prev steps)
              (vswap! steps conj [op na nb]))
            (recur (m prev)))
          steps))
      (let [[ra rb] cur]
         (vswap! steps conj [:r ra rb])
         steps))
    steps))

(defn- trace
  ([came [ra rb :as cur]]
   @(trace* came cur (volatile! '())))
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

  (def a [:a :e [:b :c]])
  (index a)
  (def b [:d :a :b :e])
  (diff a b)
  (patch a (diff a b))
  (def a [{:a [3 4] :b [1 2]} [42 nil "life"]])
  (index a)
  (def b [{:a [3] :b {:a 3} :c 42}])
  [[[:a 1] :-] [[:b] :r {:a 3}] [[:c] :+ 42]]
  (diff a b)
  (index b)
  (index 1)
  (index [])
  (def a [[:u]])
  (def b [:s :t])
  (def a [:a :b])
  (def b [:b :c])
  (patch a (diff a b))

  )

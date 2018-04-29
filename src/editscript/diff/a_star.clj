;;
;; Copyright (c) Huahai Yang. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.
;;

(ns editscript.diff.a-star
  (:require [clojure.set :as set]
            [editscript.util.priority :as p]
            [editscript.core :refer :all])
  (:import [clojure.lang PersistentVector Keyword]
           [java.io Writer]
           [java.lang Comparable]))

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
  (^long get-order [this] "Get the order of this node in traversal")
  (^long get-size [this] "Get the size of sub-tree, used to estimate cost")
  (set-size [this s] "Set the size of sub-tree"))

(deftype Node [^PersistentVector path
               value
               ^:volatile-mutable children
               ^:volatile-mutable first
               ^:volatile-mutable last
               ^:volatile-mutable next
               ^:volatile-mutable ^long index
               ^long order
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
  (get-order [this] order)
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
  (print-method {:path  (get-path x)
                 :value (get-value x)
                 :children (get-children x)}
                writer))

(declare index*)

(defn- index-associative
  [order path data parent]
  (let [node (->Node path data {} nil nil nil 0 @order 1)]
    (vswap! order (fn [o] (inc ^long o)))
    (add-child parent node)
    (reduce-kv
     (fn [_ k v]
       (index* order (conj path k) v node))
     nil
     data)
    (let [^long cs (->> (get-children node) vals (map get-size) (reduce +))]
      (set-size node (+ (get-size node) cs)))))

(defn- index-value
  [order path data parent]
  (let [node (->Node path data nil nil nil nil 0 @order 1)]
    (add-child parent node)
    (vswap! order (fn [o] (inc ^long o)))
    node))

(defn- index*
  [order path data parent]
  (case (get-type data)
    (:map :vec) (index-associative order path data parent)
    :val        (index-value order path data parent)))

(defn- index
  "Traverse data to build an indexing tree of Nodes,
  compute path, sizes of sub-trees, siblings, etc. for each Node"
  [data]
  (let [order (volatile! 0)]
    (index* order [] data (->Node [] ::dummy {} nil nil nil 0 -1 0))))

;; diffing

(deftype Coord [^Node a
                ^Node b]
  Object
  (hashCode [_]
    (let [x (int (get-order a))
          y (int (get-order b))]
      ;; Szudzik's paring function
      (if (> y x)
        (+ x (* y y))
        (+ x y (* x x)))))
  (equals [this that]
    (= (.hashCode this) (.hashCode that)))
  (toString [_]
    (str "[" (get-order a) "," (get-order b) "]"))

  Comparable
  (compareTo [this o]
    (- (.hashCode this) (.hashCode o))))

(defn- get-coord
  [^Coord coord]
  [(.-a coord) (.-b coord)])

(defprotocol IStep
  (operator [this] "Operator to try")
  (current [this] "Starting pair of nodes")
  (neighbor [this] "Destination pair of nodes"))

(deftype Step [^Keyword op
               ^Coord cur
               ^Coord nbr]
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
  [^Coord cur came g op]
  (let [^long gc (access-g g cur)]
    (case op
      :=      gc
      :-      (inc gc)
      (:a :i) (let [sb (get-size (.-b cur))]
                (+ gc (inc ^long sb)))
      :r      (let [na (.-a cur)
                    nb (.-b cur)
                    sb (get-size nb)
                    sa (get-size na)]
                (if (or (= sa 1) (= sb 1))
                  (+ gc (inc ^long sb))
                  (+ gc ^long (diff* na nb came)))))))

(defn- heuristic
  "An optimistic estimate of the cost to reach goal when at (x y).
  For sequences with positive goal differential (delta), the optimal number of
  edits is deletion dependent, equals to 2p+delta, where p is number of deletions.
  Optimistically assuming no new deletion will be needed after (x, y), the number
  of edits is delta-k, where k=y-x. The same logic applies to negative delta.
  For nested structure, multiple deletion may be merged longo one.
  Also, because addition/replacement requires new value to be present in
  editscript, whereas deletion does not, we assign estimate differently."
  [type cur end [gx gy]]
  (case type
    :map 0
    :vec (let [[na nb] (get-coord cur)
               [ra rb] (get-coord end)
               x       (if (identical? ra na) gx (get-index na))
               y       (if (identical? rb nb) gy (get-index nb))
               delta   (- ^long gy ^long gx)
               k       (- ^long y ^long x)
               cost    (- delta k)]
           (if (= cost 0)
             0
             (if (>= delta 0)
               (if (> k delta) 1 0)
               (if (< k delta) (inc cost) 0))))))

(defn- explore
  [type end came goal state step]
  (let [[came' open g] (get-state state)
        [op cur nbr]   (get-step step)
        tmp-g          (compute-cost cur came g op)]
    (if (>= ^long tmp-g ^long (access-g g nbr))
      state
      (doto state
        (set-came (assoc! came' nbr [cur op]))
        (set-open (assoc open nbr
                         (+ ^long tmp-g ^long (heuristic type nbr end goal))))
        (set-g (assoc! g nbr tmp-g))))))

(defn- values=?
  [va vb]
  (or (identical? va vb)
      (and (= :val (get-type va) (get-type vb)) (= va vb))))

(defn- next-node
  [na ra]
  (or (get-next na) ra))

(defn- vec-frontier
  [end cur]
  (let [[ra rb] (get-coord end)
        [na nb] (get-coord cur)
        a=b     (values=? (get-value na) (get-value nb))
        x=gx    (identical? na ra)
        x<gx    (not x=gx)
        y<gy    (not (identical? nb rb))
        na'     (next-node na ra)
        nb'     (next-node nb rb)]
    (if (and x<gx y<gy a=b)
      [(->Step := cur (->Coord na' nb'))]
      (cond-> []
        x<gx            (conj (->Step :- cur (->Coord na' nb)))  ; delete
        (and x<gx y<gy) (conj (->Step :r cur (->Coord na' nb'))) ; replace
        (and x=gx y<gy) (conj (->Step :a cur (->Coord na nb')))  ; append
        (and x<gx y<gy) (conj (->Step :i cur (->Coord na nb'))))))) ; insert

(defn- map-frontier
  [end cur]
  (let [[ra rb] (get-coord end)
        [na nb] (get-coord cur)
        va   (get-value na)
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
        [(->Step (if (values=? va (mb ka)) := :r) cur (->Coord na' nb''))]
        [(->Step :- cur (->Coord na' nb))])
      (if (contains? (get-value ra) kb)
        [(->Step := cur (->Coord ra nb'))]
        [(->Step :a cur (->Coord ra nb'))]))))

(defn- frontier
  [type end cur]
  (case type
    :vec (vec-frontier end cur)
    :map (map-frontier end cur)))

(defn- A*
  [type ra rb came]
  (let [end  (->Coord ra rb)
        goal [(-> ra get-children count) (-> rb get-children count)]
        init (->Coord (get-first ra) (get-first rb))]
    (loop [state (->State (transient {})
                          (p/priority-map init (heuristic type init end goal))
                          (transient {init 0}))]
      (let [[came' open g] (get-state state)]
        (if (empty? open)
          (throw (ex-info "A* diff fails to find a solution" {:ra ra :rb rb}))
          (let [[cur cost] (peek open)]
            (if (= cur end)
              (do (vswap! came assoc end (persistent! came'))
                  cost)
              (recur (reduce
                      (partial explore type end came goal)
                      (set-open state (pop open))
                      (frontier type end cur))))))))))

(defn- ^long diff*
  [ra rb came]
  (let [^long sizeb (get-size rb)
        typea       (-> ra get-value get-type)
        update      #(vswap! came assoc (->Coord ra rb) {})]
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
  (if (= op :=)
    path
    (loop [newp     []
           prev     []
           [k & ks] path]
      (if k
        (let [^long d (get-in @trie (conj prev :delta) 0)]
          (recur (conj newp (if (integer? k) (+ ^long k d) k))
                 (conj prev k)
                 ks))
        (let [seen    (conj (if (seq path) (pop path) path) :delta)
              ^long d (get-in @trie seen 0)]
          (vswap! trie assoc-in seen (case op :- (dec d) :i (inc d) d))
          newp)))))

(defn- adjust-append
  [trie op na nb path path']
  (if (= op :a)
    (case (-> na get-value get-type)
      :vec (conj path'
                 (let [^long d (get-in @trie (conj path :delta) 0)]
                   (+ d (-> na get-children count))))
      :map (conj path' (get-key nb)))
    path'))

(defn- convert-path
  [trie op na nb path]
  (->> path
       (adjust-delete-insert trie op)
       (adjust-append trie op na nb path)))

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
   (volatile! {:delta 0})
   steps))

(defn- trace*
  [came cur steps]
  (if-let [m (came cur)]
    (if (seq m)
      (loop [v (m cur)]
        (if v
          (let [[prev op] v
                [na nb]   (get-coord prev)]
            (if (and (came prev) (not= op :-))
              (trace* came prev steps)
              (vswap! steps conj [op na nb]))
            (recur (m prev)))
          steps))
      (let [[ra rb] (get-coord cur)]
        (vswap! steps conj [:r ra rb])
        steps))
    steps))

(defn- trace
  ([came cur]
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
        ;; (println "cost is" cost)
        ;; (let [total          (* (get-size roota) (get-size rootb))
        ;;       ^long explored (reduce + (map count (vals @came)))]
        ;;   (printf "explored %d of %d %.1f%%\n"
        ;;           explored total (* 100 (double (/ explored total)))))
        (trace @came (->Coord roota rootb) script)))
    script))

(comment

  (def a [[:a [:b :c] :d] :e :f])
  (def b [:b :c [:e] :f :g])
  (time (diff a b))
  (patch a (diff a b))

  (def a [[:a] :b [:c [:d] [:e] :f]])
  (def b [[:b] [:c [:e] [:f] :d]])
  (diff a b)
  (patch a (diff a b))

  (diff a b)
  (patch a (diff a b))
  (def a [{:a [3 4] :b [1 2]} [42 nil "life"]])
  (index a)
  (def b [{:a [3] :b {:a 3} :c 42}])
  [[[:a 1] :-] [[:b] :r {:a 3}] [[:c] :+ 42]]
  (diff a b)
  (index a)
  (index b)
  (index [])
  (def a [[:u]])
  (def b [:s :t])
  (def a 1)
  (def b 2)
  (def a {(->Coord (index a) (index b)) 1})
  (a (->Coord (index a) (index b)))
  (patch a (diff a b))
  (diff a b)


  )

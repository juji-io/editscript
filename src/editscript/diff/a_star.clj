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
  (:require [editscript.edit :as e]
            [editscript.util.priority :as p])
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
  (get-parent [this] "Get the parent node")
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
               parent
               ^:volatile-mutable children
               ^:volatile-mutable first
               ^:volatile-mutable last
               ^:volatile-mutable next
               ^:volatile-mutable index
               ^long order
               ^:volatile-mutable ^long size]
  INode
  (get-path [this] path)
  (get-key [this] (-> this get-path peek))
  (get-value [this] value)
  (get-parent [this] parent)
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
  (print-method {:path     (get-path x)
                 :value    (get-value x)
                 :children (get-children x)}
                writer))

(declare index*)

(defn- associative-children
  "map and vector are associative"
  [order path data parent]
  (reduce-kv
   (fn [_ k v]
     (index* order (conj path k) v parent))
   nil
   data))

(defn- set-children
  "set is a map of keys to themselves"
  [order path data parent]
  (doseq [x data]
    (index* order (conj path x) x parent)))

(defn- list-children
  "add index as key"
  [order path data parent]
  (reduce
   (fn [i x]
     (index* order (conj path i) x parent)
     (inc ^long i))
   0
   data))

(defn- index-collection
  [type order path data parent]
  (let [node (->Node path data parent {} nil nil nil 0 @order 1)]
    (vswap! order (fn [o] (inc ^long o)))
    (add-child parent node)
    (case type
      (:map :vec) (associative-children order path data node)
      :set        (set-children order path data node)
      :lst        (list-children order path data node))
    (let [^long cs (->> (get-children node) vals (map get-size) (reduce +))]
      (set-size node (+ (get-size node) cs)))))

(defn- index-value
  [order path data parent]
  (let [node (->Node path data parent nil nil nil nil 0 @order 1)]
    (add-child parent node)
    (vswap! order (fn [o] (inc ^long o)))
    node))

(defn- index*
  [order path data parent]
  (let [type (e/get-type data)]
    (if (= type :val)
      (index-value order path data parent)
      (index-collection type order path data parent))))

(defn- index
  "Traverse data to build an indexing tree of Nodes,
  compute path, sizes of sub-trees, siblings, etc. for each Node.
  This takes little time"
  [data]
  (let [order (volatile! 0)]
    (index* order [] data (->Node [] ::dummy nil {} nil nil nil 0 -1 0))))

;; diffing

(deftype Coord [^Node a
                ^Node b]
  ;; Java's native hash is too slow,
  ;; overriding hashCode significantly speeds things up
  Object
  (hashCode [_]
    (let [x (get-order a)
          y (get-order b)]
      ;; Szudzik's paring function
      (if (> ^long y ^long x)
        (+ ^long x (* ^long y ^long y))
        (+ ^long x ^long y (* ^long x ^long x)))))
  (equals [this that]
    (= (.hashCode this) (.hashCode that)))
  (toString [_]
    (str "[" (get-value a) "," (get-value b) "]"))

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
  "Profiling shows that more than 70% of running time is spent in this function,
  not counting the recursive call to `diff*`, that means there's not much else
  can be optimized except to devise better heuristic, so the number of steps tried
  can be reduced."
  [^Coord cur came g op]
  (let [^long gc (access-g g cur)]
    (case op
      :=      gc
      ;; delete only cost 1, for not including deleted data in script
      :-      (inc gc)
      ;; these cost the size of included data, plus 1
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
  "A simplistic but optimistic estimate of the cost to reach goal when at (x y).

  For sequences with positive goal differential (delta), the optimal number of
  edits is deletion dependent, equals to 2p+delta, where p is number of deletions.
  Optimistically assuming no new deletion will be needed after (x, y), the number
  of edits is delta-k, where k=y-x. The same logic applies to negative delta.
  For nested structure, multiple deletion may be merged into one.
  Also, because addition/replacement requires new value to be present in
  editscript, whereas deletion does not, we assign estimate differently."
  [type cur end [gx gy]]
  (case type
    (:map :set) 0
    (:vec :lst) (let [[na nb] (get-coord cur)
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
      (and (= :val (e/get-type va) (e/get-type vb))
           (if va
             (.equals ^Object va vb)
             (= va vb)))))

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
  [^Coord init end cur]
  (let [[ra rb] (get-coord end)
        [na nb] (get-coord cur)
        va      (get-value na)
        vb      (get-value nb)
        mb      (get-value rb)
        x<gx    (not (identical? na ra))
        y<gy    (not (identical? nb rb))
        ka      (get-key na)
        kb      (get-key nb)
        na'     (next-node na ra)
        x'=gx   (identical? na' ra)
        cb      (get-children rb)]
    (cond
      ;; transition point from testing keys of a to that of b
      (and x<gx x'=gx)
      (let [startb (->Coord ra (.-b init))
            enda   (->Coord na (cb ka))]
        (if (contains? mb ka)
          (if (= ka kb)
            [(->Step (if (values=? va vb) := :r) cur startb)]
            [(->Step := cur enda)
             (->Step :r enda startb)])
          [(->Step :- cur startb)]))
      ;; testing keys of a
      (and x<gx y<gy)
      [(if (contains? mb ka)
         (if (= ka kb)
           (->Step (if (values=? va vb) := :r)
                   cur (->Coord na' (or (cb (get-key na')) nb)))
           (->Step := cur (->Coord na (cb ka))))
         (->Step :- cur (->Coord na' nb)))]
      ;; keys of b
      :else
      [(->Step (if (contains? (get-value ra) kb) := :a)
               cur (->Coord ra (next-node nb rb)))])))

(defn- frontier
  [type init end cur]
  (case type
    (:vec :lst) (vec-frontier end cur)
    (:map :set) (map-frontier init end cur)))

(defn- A*
  [type ra rb came]
  (let [end  (->Coord ra rb)
        init (->Coord (get-first ra) (get-first rb))
        goal [(-> ra get-children count) (-> rb get-children count)]]
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
                      (frontier type init end cur))))))))))

(defn- ^long diff*
  [ra rb came]
  (let [sa     ^long (get-size ra)
        sb     ^long (get-size rb)
        typea  (-> ra get-value e/get-type)
        update #(vswap! came assoc (->Coord ra rb) {})]
    (cond
      ;; both are :val, skip or replace
      (= 1 sa sb)
      (do (update)
          (if (values=? (get-value ra) (get-value rb))
            0
            2))
      ;; one of them is :val, replace
      (or (= 1 sa) (= 1 sb))
      (do (update)
          (inc ^long sb))
      ;; run A* for children
      (= typea (-> rb get-value e/get-type))
      (A* typea ra rb came)
      ;; types differ, can only replace
      :else
      (do (update)
          (inc ^long sb)))))

;; generating editscript

(defn- index-key?
  [node]
  (#{:vec :lst} (-> node get-value e/get-type)))

(defn- adjust-delete-insert
  [trie op root path]
  (if (= op :=)
    path
    (loop [newp []
           prev []
           node root
           left path]
      (if (seq left)
        (let [[k & ks] left
              ^long d  (get-in @trie (conj prev :delta) 0)]
          (recur (conj newp (if (index-key? node) (+ ^long k d) k))
                 (conj prev k)
                 ((get-children node) k)
                 ks))
        (if (index-key? (get-parent node))
          (let [seen    (conj (if (seq path) (pop path) path) :delta)
                ^long d (get-in @trie seen 0)]
            (vswap! trie assoc-in seen (case op :- (dec d) :i (inc d) d))
            newp)
          newp)))))

(defn- adjust-append
  [trie op na nb path path']
  (if (= op :a)
    (if (index-key? na)
      (conj path' (let [seen    (conj path :delta)
                        ^long d (get-in @trie seen 0)]
                    (vswap! trie assoc-in seen (inc d))
                    (+ d (-> na get-children count))))
      (conj path' (get-key nb)))
    path'))

(defn- convert-path
  [trie op roota na nb path]
  (->> path
       (adjust-delete-insert trie op roota)
       (adjust-append trie op na nb path)))

(defn- write-script
  [steps roota script]
  (reduce
   (fn [trie [op na nb]]
     (let [path  (convert-path trie op roota na nb (get-path na))
           value (get-value nb)]
       (case op
         :-      (e/delete-data script path)
         :r      (e/replace-data script path value)
         (:a :i) (e/add-data script path value)
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
            (if (and (came prev) (= op :r))
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
  ([came ^Coord cur script]
   (-> (trace came cur)
       (write-script (.-a cur) script))))

(defn diff
  "Create an EditScript that represents the minimal difference between `b` and `a`"
  [a b]
  (let [script (e/->EditScript [] false 0 0 0 0)]
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
        (trace @came (->Coord roota rootb) script)
        (e/set-size script ^long cost)))
    script))

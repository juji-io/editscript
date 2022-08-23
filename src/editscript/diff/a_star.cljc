;;
;; Copyright (c) Huahai Yang. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.
;;

(ns ^:no-doc editscript.diff.a-star
  (:require [editscript.edit :as e]
            [editscript.util.pairing :as pa]
            [editscript.util.common :as co]
            #?(:cljs [goog.math.Long :refer [getMaxValue]]))
  #?(:clj (:import [clojure.lang PersistentVector Keyword]
                   [java.io Writer]
                   [java.lang Comparable])))

#?(:clj (set! *warn-on-reflection* true))
#?(:clj (set! *unchecked-math* :warn-on-boxed))

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
  (set-order [this o] "Set the traversal order of this node")
  (^long get-order [this] "Get the order of this node in traversal")
  (^long get-size [this] "Get the size of sub-tree, used to estimate cost")
  (set-size [this s] "Set the size of sub-tree"))

(deftype Node [^PersistentVector path
               value
               parent
               ^:unsynchronized-mutable children
               ^:unsynchronized-mutable first
               ^:unsynchronized-mutable last
               ^:unsynchronized-mutable next
               ^:unsynchronized-mutable index
               ^:unsynchronized-mutable ^long order
               ^:unsynchronized-mutable ^long size]
  INode
  (get-path [_] path)
  (get-key [this] (-> this get-path peek))
  (get-value [_] value)
  (get-parent [_] parent)
  (get-children [_] children)
  (get-first [_] first)
  (get-last [_] last)
  (get-next [_] next)
  (set-next [_ n] (set! next n))
  (get-order [_] order)
  (set-order [this o] (set! order (long o)) this)
  (get-size [_] size)
  (set-size [this s] (set! size (long s)) this)
  (add-child [_ node]
    (set! children (assoc children (get-key node) node))
    (when last (set-next last node))
    (when-not first (set! first node))
    (set! last node)
    node))

#?(:clj
   (defmethod print-method Node
     [x ^Writer writer]
     (print-method {:value    (get-value x)
                    :order    (get-order x)
                    :children (get-children x)}
                   writer)))

;; using defn instead of declare, see http://dev.clojure.org/jira/browse/CLJS-1871
(defn ^:declared index* [order path data parent])

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

(defn- inc-order
  "order value reflects the size of elements"
  [order ^long size]
  (vswap! order (fn [o] (+ size ^long o))))

(defn- index-collection
  [type order path data parent]
  (let [node (->Node path data parent {} nil nil nil 0 0 1)]
    (add-child parent node)
    (case type
      (:map :vec) (associative-children order path data node)
      :set        (set-children order path data node)
      :lst        (list-children order path data node))
    (let [^long cs (->> (get-children node) vals (map get-size) (reduce +))
          size     (+ (get-size node) cs)]
      (doto node
        (set-order @order)
        (set-size size))
      (inc-order order size))
    node))

(defn- index-value
  [order path data parent]
  (let [node (->Node path data parent nil nil nil nil 0 @order 1)]
    (add-child parent node)
    (inc-order order 1)
    node))

(defn- index*
  [order path data parent]
  (let [type (e/get-type data)]
    (if (or (= type :val) (= type :str))
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

(defn- coord-hash
  [a b]
  (co/szudzik (get-order a) (get-order b)))

#?(:clj
   (deftype Coord [^Node a
                   ^Node b]
     ;; Java's native hash is too slow,
     ;; overriding hashCode significantly speeds things up
     Object
     (hashCode [_] (coord-hash a b))
     (equals [this that]
       (= (.hashCode this) (.hashCode that)))
     (toString [_]
       (str "[" (get-value a) "," (get-value b) "]"))

     Comparable
     (compareTo [this that]
       (- (.hashCode this) (.hashCode that))))

   :cljs
   (deftype Coord [^Node a
                   ^Node b]
     IHash
     (-hash [_] (coord-hash a b))

     IEquiv
     (-equiv [this that]
       (= (-hash this) (-hash that)))

     IComparable
     (-compare [this that]
       (- (-hash this) (-hash that)))))

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

#?(:clj
   (defmethod print-method Step
     [x ^Writer writer]
     (print-method {:op  (operator x)
                    :cur (current x)
                    :nbr (neighbor x)}
                   writer)))

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

(deftype State [^:unsynchronized-mutable came
                ^:unsynchronized-mutable open
                ^:unsynchronized-mutable g]
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
  (get g cur #?(:clj Long/MAX_VALUE
                :cljs (getMaxValue))))

(defn ^:declared diff* [ra rb came opts])

(defn- compute-cost
  [^Coord cur came g op opts]
  (let [^long gc (access-g g cur)]
    (case op
      :=      gc
      ;; delete only cost 1, for not including deleted data in script
      :-      (inc gc)
      ;; these cost the size of included data, plus 1
      (:a :i) (let [sb (get-size (.-b cur))]
                (+ gc (inc ^long sb)))
      :r      (+ gc ^long (diff* (.-a cur) (.-b cur) came opts)))))

(defn- heuristic
  "A simplistic but optimistic estimate of the cost to reach goal when at (x y).

  For nested structure, multiple deletion may be merged into one.
  Also, because addition/replacement requires new value to be present in
  editscript, whereas deletion does not, we assign estimate differently. "
  ^long [type cur end [gx gy]]
  (case type
    (:map :set) 0
    (:vec :lst) (let [[na nb] (get-coord cur)
                      [ra rb] (get-coord end)
                      x       (if (identical? ra na) gx (get-order na))
                      y       (if (identical? rb nb) gy (get-order nb))
                      dy      (- ^long gy ^long y)
                      dx      (- ^long gx ^long x)]
                  (cond
                    (== dx 0) dy
                    (== dy 0) 1
                    (> dx dy) 3
                    (< dx dy) (- dy dx)
                    :else     2))))

(defn- explore
  [type end came goal state step opts]
  (let [[came' open g] (get-state state)
        [op cur nbr]   (get-step step)
        tmp-g          (compute-cost cur came g op opts)]
    (if (>= ^long tmp-g ^long (access-g g nbr))
      state
      (doto state
        (set-came (assoc! came' nbr [cur op]))
        (set-open (assoc open nbr
                         (+ ^long tmp-g ^long (heuristic type nbr end goal))))
        (set-g (assoc! g nbr tmp-g))))))

(defn- next-node
  [na ra]
  (or (get-next na) ra))

(defn- vec-frontier
  [end cur]
  (let [[ra rb] (get-coord end)
        [na nb] (get-coord cur)
        a=b     (= (get-value na) (get-value nb))
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
        ka      (get-key na)
        kb      (get-key nb)]
    (if (identical? na ra)
      ;; testing keys of b
      [(->Step (if (contains? (get-value ra) kb) := :a)
               cur (->Coord ra (next-node nb rb)))]
      (let [va  (get-value na)
            vb  (get-value nb)
            mb  (get-value rb)
            na' (next-node na ra)
            cb  (get-children rb)]
        (if (identical? na' ra)
          ;; transition point from testing keys of a to that of b
          (let [startb (->Coord ra (.-b init))
                enda   (->Coord na (cb ka))]
            (if (contains? mb ka)
              (if (= ka kb)
                [(->Step (if (= va vb) := :r) cur startb)]
                [(->Step := cur enda)
                 (->Step :r enda startb)])
              [(->Step :- cur startb)]))
          ;; testing keys of a
          [(if (contains? mb ka)
             (if (= ka kb)
               (->Step (if (= va vb) := :r)
                       cur (->Coord na' (or (cb (get-key na')) nb)))
               (->Step := cur (->Coord na (cb ka))))
             (->Step :- cur (->Coord na' nb)))])))))

(defn- frontier
  [type init end cur]
  (case type
    (:vec :lst) (vec-frontier end cur)
    (:map :set) (map-frontier init end cur)))

(defn- A*
  [type ra rb came opts]
  (let [end  (->Coord ra rb)
        init (->Coord (get-first ra) (get-first rb))
        goal [(get-order ra) (get-order rb)]]
    (loop [state (->State (transient {})
                          (pa/priority-map init (heuristic type init end goal))
                          (transient {init 0}))]
      (let [[came' open _] (get-state state)]
        (if (empty? open)
          (throw (ex-info "A* diff fails to find a solution" {:ra ra :rb rb}))
          (let [[cur cost] (peek open)]
            (if (= cur end)
              (do (vswap! came assoc end (persistent! came'))
                  cost)
              (recur (reduce
                       #(explore type end came goal %1 %2 opts)
                       (set-open state (pop open))
                       (frontier type init end cur))))))))))

(defn- vec-fn
  [node]
  (let [v (get-value node)]
    (if (= :vec (e/get-type v))
      v
      (vec v))))

(defn- use-quick
  ^long [ra rb came opts]
  (loop [[op & ops] (co/vec-edits (vec-fn ra) (vec-fn rb) opts)
         na         (get-first ra)
         nb         (get-first rb)
         m          (transient {})
         cost       0]
    (if op
      (let [na' (next-node na ra)
            nb' (next-node nb rb)
            cur (->Coord na nb)
            sb  (get-size nb)]
        (if (integer? op)
          (recur (if (> ^long op 1) `[~(dec ^long op) ~@ops] ops)
                 na' nb'
                 (assoc! m (->Coord na' nb') [cur :=])
                 (long cost))
          (case op
            :- (recur ops na' nb
                      (assoc! m (->Coord na' nb) [cur op])
                      (inc (long cost)))
            :+ (recur ops na nb'
                      (assoc! m (->Coord na nb')
                              [cur (if (identical? na ra) :a :i)])
                      (+ (long cost) 1 (long sb)))
            :r (recur ops na' nb'
                      (assoc! m (->Coord na' nb') [cur op])
                      (+ (long cost) 1 (long sb))))))
      (let [root (->Coord ra rb)]
        (vswap! came assoc root (persistent! m))
        cost))))

(defn- diff*
  ^long [ra rb came opts]
  (let [sa     ^long (get-size ra)
        sb     ^long (get-size rb)
        va     (get-value ra)
        vb     (get-value rb)
        typea  (e/get-type va)
        update #(vswap! came assoc (->Coord ra rb) {})]
    (cond
      ;; both are leaves, skip or replace
      (= 1 sa sb)
      (do (update)
          (if (= va vb)
            0
            2))
      ;; one of them is leaf, replace
      (or (= 1 sa) (= 1 sb))
      (do (update)
          (inc ^long sb))
      ;; non-empty coll with same type, drill down
      (= typea (e/get-type vb))
      (if (= va vb)
        (do (update) 0)
        (let [a (if (and (#{:vec :lst} typea)
                         (let [cc+1 #(-> % get-children count inc)]
                           (or (= sa (cc+1 ra)) (= sb (cc+1 rb)))))
                  ;; vec or lst contains leaves only, safe to use quick algo.
                  (use-quick ra rb came opts)
                  ;; otherwise run A*
                  (A* typea ra rb came opts))
              r (inc ^long sb)]
          (if (< r ^long a)
            (do (update)
                r)
            a)))
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
  [steps roota script {:keys [str-diff?]
                       :or   {str-diff? false}
                       :as   opts}]
  (reduce
    (fn [trie [op na nb]]
      (let [path (convert-path trie op roota na nb (get-path na))
            va   (get-value na)
            vb   (get-value nb)]
        (case op
          :-      (e/delete-data script path)
          :r      (if (and (= :str (e/get-type va) (e/get-type vb)) str-diff?)
                    (co/diff-str script path va vb opts)
                    (e/replace-data script path vb))
          (:a :i) (e/add-data script path vb)
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
        (vswap! steps conj [(if (= (get-value ra) (get-value rb)) := :r)
                            ra rb])
        steps))
    steps))

(defn- trace
  ([came cur]
   @(trace* came cur (volatile! '())))
  ([came ^Coord cur script opts]
   (-> (trace came cur)
       (write-script (.-a cur) script opts))))

(defn diff
  "Create an EditScript that represents the minimal difference between `b` and `a`"
  [a b & opts]
  (let [script (e/edits->script [])]
    (when-not (= a b)
      (let [roota (index a)
            rootb (index b)
            came  (volatile! {})
            cost  (diff* roota rootb came opts)]
        ;; #?(:clj (let [total          (* (get-size roota) (get-size rootb))
        ;;               ^long explored (reduce + (map count (vals @came)))]
        ;;           (printf "cost is %d, explored %d of %d - %.1f%%\n"
        ;;                   cost explored total
        ;;                   (* 100 (double (/ explored total))))))
        (trace @came (->Coord roota rootb) script opts)
        script))
    script))

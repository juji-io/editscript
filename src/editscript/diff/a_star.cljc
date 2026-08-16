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
            [editscript.util.index :as i]
            [editscript.util.common :as co]
            #?(:cljs [goog.math.Long :refer [getMaxValue]]))
  #?(:clj (:import [java.lang Comparable]
                   [editscript.util.index Node])
     :cljr (:import [editscript.util.index Node])))

#?(:clj (set! *warn-on-reflection* true))
#?(:cljr (set! *warn-on-reflection* true))
#?(:clj (set! *unchecked-math* :warn-on-boxed))

;; diffing

(defn- coord-hash
  ^long [a b]
  (co/szudzik (i/get-order a) (i/get-order b)))

;; Index nodes are canonical within one diff. Coordinate equality can therefore
;; use node identity, while the stable traversal-order hash is computed once.
#?(:clj
   (deftype Coord [^Node a
                   ^Node b
                   ^long h]
     ;; Java's native hash is too slow,
     ;; overriding hashCode significantly speeds things up
     Object
     #?@(:clj [(hashCode [_] (int h))])
     (equals [_ that]
       (and (instance? Coord that)
            (identical? a (.-a ^Coord that))
            (identical? b (.-b ^Coord that))))
     (toString [_]
       (str "[" (i/get-value a) "," (i/get-value b) "]"))

     #?@(:bb []
         :clj [Comparable
               (compareTo [_ that]
                          (compare h (.-h ^Coord that)))]))
   :cljr
   (deftype Coord [^Node a
                   ^Node b
                   ^long h]
     ;; Java's native hash is too slow,
     ;; overriding hashCode significantly speeds things up
     Object
     (GetHashCode [_] (int h))
     (Equals  [_ that]
       (and (instance? Coord that)
            (identical? a (.-a ^Coord that))
            (identical? b (.-b ^Coord that))))
     (ToString [_]
       (str "[" (i/get-value a) "," (i/get-value b) "]"))

     IComparable
     (CompareTo [_ that]
       (compare h (.-h ^Coord that))))

   :cljs
   (deftype Coord [^Node a
                   ^Node b
                   ^long h]
     IHash
     (-hash [_] h)

     IEquiv
     (-equiv [_ that]
       (and (instance? Coord that)
            (identical? a (.-a ^Coord that))
            (identical? b (.-b ^Coord that))))

     IComparable
     (-compare [_ that]
       (compare h (.-h ^Coord that)))))

(defn- coord
  [a b]
  (->Coord a b (coord-hash a b)))

(defn- coord-or-end
  [^Coord end a b]
  (if (and (identical? a (.-a end))
           (identical? b (.-b end)))
    end
    (coord a b)))

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

(defn- access-g
  [g cur]
  (get g cur #?(:clj Long/MAX_VALUE
                :cljr Int64/MaxValue
                :cljs (getMaxValue))))

(declare diff*)

(defn- compute-cost
  [^Coord cur came g op opts]
  (let [^long gc (access-g g cur)]
    (case op
      :=      gc
      ;; delete only cost 1, for not including deleted data in script
      :-      (inc gc)
      ;; these cost the size of included data, plus 1
      (:a :i) (let [sb (i/get-size (.-b cur))]
                (+ gc (inc ^long sb)))
      :r      (+ gc ^long (diff* (.-a cur) (.-b cur) came opts)))))

(defn- heuristic
  "A simplistic but optimistic estimate of the cost to reach goal when at (x y).

  For nested structure, multiple deletion may be merged into one.
  Also, because addition/replacement requires new value to be present in
  editscript, whereas deletion does not, we assign estimate differently. "
  [type na nb ra rb gx gy]
  (let [^long gx gx
        ^long gy gy]
    (case type
      (:map :set) 0
      (:vec :lst) (let [x  (if (identical? ra na) gx (i/get-order na))
                        y  (if (identical? rb nb) gy (i/get-order nb))
                        dy (- gy ^long y)
                        dx (- gx ^long x)]
                    (cond
                      (== dx 0) dy
                      (== dy 0) 1
                      (> dx dy) 3
                      (< dx dy) (- dy dx)
                      :else     2)))))

(defn- explore
  [type ^Coord end came gx gy ^State state op ^Coord cur
   na nb nbr opts upper-bound]
  (let [came'    (get-came state)
        open     (get-open state)
        g        (get-g state)
        ra       (.-a end)
        rb       (.-b end)
        tmp-g    (compute-cost cur came g op opts)
        estimate (+ ^long tmp-g
                    ^long (heuristic type na nb ra rb gx gy))]
    (if (> ^long estimate ^long upper-bound)
      state
      (let [^Coord nbr (or nbr (coord-or-end end na nb))]
        (if (>= ^long tmp-g ^long (access-g g nbr))
          state
          (doto state
            (set-came (assoc! came' nbr [cur op]))
            (set-open (assoc open nbr estimate))
            (set-g (assoc! g nbr tmp-g))))))))

(defn- next-node
  [na ra]
  (or (i/get-next na) ra))

(defn- vec-frontier
  [type ^Coord end came gx gy ^State state ^Coord cur opts upper-bound]
  (let [ra   (.-a end)
        rb   (.-b end)
        na   (.-a cur)
        nb   (.-b cur)
        a=b  (= (i/get-value na) (i/get-value nb))
        x=gx (identical? na ra)
        x<gx (not x=gx)
        y<gy (not (identical? nb rb))
        na'  (next-node na ra)
        nb'  (next-node nb rb)]
    (if (and x<gx y<gy a=b)
      (explore type end came gx gy state := cur na' nb' nil opts upper-bound)
      (let [state (if x<gx
                    (explore type end came gx gy state :- cur na' nb nil
                             opts upper-bound)
                    state)
            state (if (and x<gx y<gy)
                    (explore type end came gx gy state :r cur na' nb' nil
                             opts upper-bound)
                    state)
            state (if (and x=gx y<gy)
                    (explore type end came gx gy state :a cur na nb' nil
                             opts upper-bound)
                    state)]
        (if (and x<gx y<gy)
          (explore type end came gx gy state :i cur na nb' nil
                   opts upper-bound)
          state)))))

(defn- map-frontier
  [type ^Coord init ^Coord end came gx gy ^State state ^Coord cur opts
   upper-bound]
  (let [ra (.-a end)
        rb (.-b end)
        na (.-a cur)
        nb (.-b cur)
        ka (i/get-key na)
        kb (i/get-key nb)]
    (if (identical? na ra)
      ;; testing keys of b
      (explore type end came gx gy state
               (if (contains? (i/get-value ra) kb) := :a)
               cur ra (next-node nb rb) nil opts upper-bound)
      (let [va  (i/get-value na)
            vb  (i/get-value nb)
            mb  (i/get-value rb)
            na' (next-node na ra)
            cb  (i/get-children rb)]
        (if (identical? na' ra)
          ;; transition point from testing keys of a to that of b
          (let [start-nb (.-b init)
                enda-nb  (cb ka)]
            (if (contains? mb ka)
              (if (= ka kb)
                (explore type end came gx gy state (if (= va vb) := :r)
                         cur ra start-nb nil opts upper-bound)
                (let [enda  (coord na enda-nb)
                      state (explore type end came gx gy state := cur
                                     na enda-nb enda opts upper-bound)]
                  (explore type end came gx gy state :r enda
                           ra start-nb nil opts upper-bound)))
              (explore type end came gx gy state :- cur
                       ra start-nb nil opts upper-bound)))
          ;; testing keys of a
          (if (contains? mb ka)
            (if (= ka kb)
              (explore type end came gx gy state (if (= va vb) := :r) cur
                       na' (or (cb (i/get-key na')) nb) nil opts upper-bound)
              (explore type end came gx gy state := cur
                       na (cb ka) nil opts upper-bound))
            (explore type end came gx gy state :- cur
                     na' nb nil opts upper-bound)))))))

(defn- frontier
  [type init end came gx gy state cur opts upper-bound]
  (case type
    (:vec :lst) (vec-frontier type end came gx gy state cur opts upper-bound)
    (:map :set) (map-frontier type init end came gx gy state cur opts
                              upper-bound)))

(defn- A*
  [type ^Coord end came opts upper-bound]
  (let [ra   (.-a end)
        rb   (.-b end)
        ^Coord init (coord (i/get-first ra) (i/get-first rb))
        gx   (i/get-order ra)
        gy   (i/get-order rb)
        initial-estimate (heuristic type (.-a init) (.-b init) ra rb gx gy)
        sequence? (#{:vec :lst} type)]
    (if (> ^long initial-estimate ^long upper-bound)
      ::bounded
      (loop [^State state (->State (transient {})
                                   (pa/priority-map init initial-estimate)
                                   (transient {init 0}))]
        (let [came' (get-came state)
              open  (get-open state)]
          (cond
            (empty? open)
            ::bounded

            (and sequence? (co/vec-timed-out? opts))
            ::timeout

            :else
            (let [[cur cost] (peek open)]
              (cond
                (> ^long cost ^long upper-bound)
                ::bounded

                (= cur end)
                (do (vswap! came assoc end (persistent! came'))
                    cost)

                :else
                (recur (frontier type init end came gx gy
                                 (set-open state (pop open)) cur opts
                                 upper-bound))))))))))

(defn- vec-fn
  [node]
  (let [v (i/get-value node)]
    (if (= :vec (e/get-type v))
      v
      (vec v))))

(defn- use-quick
  [ra rb ^Coord root came opts]
  (let [edits (co/vec-edits (vec-fn ra) (vec-fn rb) opts)]
    (if (= edits :timeout)
      edits
      (let [first-a (i/get-first ra)
            first-b (i/get-first rb)]
        (loop [[op & ops] edits
               na         first-a
               nb         first-b
               cur        (coord first-a first-b)
               m          (transient {})
               cost       0]
          (if op
            (let [na' (next-node na ra)
                  nb' (next-node nb rb)
                  sb  (i/get-size nb)]
              (if (integer? op)
                (let [nbr (coord-or-end root na' nb')]
                  (recur (if (> ^long op 1) `[~(dec ^long op) ~@ops] ops)
                         na' nb' nbr
                         (assoc! m nbr [cur :=])
                         (long cost)))
                (case op
                  :- (let [nbr (coord-or-end root na' nb)]
                       (recur ops na' nb nbr
                              (assoc! m nbr [cur op])
                              (inc (long cost))))
                  :+ (let [nbr (coord-or-end root na nb')]
                       (recur ops na nb' nbr
                              (assoc! m nbr
                                      [cur (if (identical? na ra) :a :i)])
                              (+ (long cost) 1 (long sb))))
                  :r (let [nbr (coord-or-end root na' nb')]
                       (recur ops na' nb' nbr
                              (assoc! m nbr [cur op])
                              (+ (long cost) 1 (long sb)))))))
            (do (vswap! came assoc cur (persistent! m))
                cost)))))))

(defn- replace-cost
  ^long [came coord ^long target-size]
  (vswap! came assoc coord {})
  (inc target-size))

(defn- compute-diff
  [ra rb ^Coord coord came opts]
  (let [sa      ^long (i/get-size ra)
        sb      ^long (i/get-size rb)
        va      (i/get-value ra)
        vb      (i/get-value rb)
        typea   (e/get-type va)]
    (cond
      ;; both are leaves, skip or replace
      (= 1 sa sb)
      (do (vswap! came assoc coord {})
          (if (= va vb)
            0
            2))
      ;; one of them is leaf, replace
      (or (= 1 sa) (= 1 sb))
      (replace-cost came coord sb)
      ;; non-empty coll with same type, drill down
      (= typea (e/get-type vb))
      (if (= va vb)
        (do (vswap! came assoc coord {}) 0)
        (let [r (inc ^long sb)
              a (if (and (#{:vec :lst} typea)
                         (or (= sa (inc (count (i/get-children ra))))
                             (= sb (inc (count (i/get-children rb))))))
                  ;; vec or lst contains leaves only, safe to use quick algo.
                  (let [res (use-quick ra rb coord came opts)]
                    (if (= res :timeout) (inc r) res))
                  ;; otherwise run A*
                  (A* typea coord came opts r))]
          (cond
            (or (= a ::bounded) (= a ::timeout)) (replace-cost came coord sb)
            (< r (long a))                       (replace-cost came coord sb)
            :else                                (long a))))
      ;; types differ, can only replace
      :else
      (replace-cost came coord sb))))

(defn- diff-coord*
  [ra rb ^Coord coord came opts]
  (if-let [memo (::cost-memo opts)]
    (let [cached (find @memo coord)]
      (if cached
        (long (val cached))
        (let [cost (compute-diff ra rb coord came opts)]
          (vswap! memo assoc! coord cost)
          cost)))
    (compute-diff ra rb coord came opts)))

(defn- diff*
  ^long [ra rb came opts]
  (diff-coord* ra rb (coord ra rb) came opts))

;; generating editscript

(defn- index-key?
  [node]
  (and node
       (#{:vec :lst} (-> node i/get-value e/get-type))))

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
                 ((i/get-children node) k)
                 ks))
        (if (index-key? (i/get-parent node))
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
                    (+ d (-> na i/get-children count))))
      (conj path' (i/get-key nb)))
    path'))

(defn- convert-path
  [trie op roota na nb path]
  (->> path
       (adjust-delete-insert trie op roota)
       (adjust-append trie op na nb path)))

(defn- write-script
  [steps roota script {:keys [str-diff]
                       :or   {str-diff :none}
                       :as   opts}]
  (reduce
    (fn [trie [op na nb]]
      (let [path (convert-path trie op roota na nb (i/get-path na))
            va   (i/get-value na)
            vb   (i/get-value nb)]
        (case op
          :-      (e/delete-data script path)
          :r      (if (and (= :str (e/get-type va) (e/get-type vb))
                           (not= str-diff :none))
                    (co/diff-str script path va vb opts)
                    (e/replace-data script path vb))
          (:a :i) (e/add-data script path vb)
          nil)
        trie))
    (volatile! {:delta 0})
    steps))

(defn- trace*
  [came ^Coord cur steps]
  (if-let [m (came cur)]
    (if (seq m)
      (loop [v (m cur)]
        (if v
          (let [[prev op] v
                na        (.-a ^Coord prev)
                nb        (.-b ^Coord prev)]
            (if (and (came prev) (= op :r))
              (trace* came prev steps)
              (vswap! steps conj [op na nb]))
            (recur (m prev)))
          steps))
      (let [ra (.-a cur)
            rb (.-b cur)]
        (vswap! steps conj [(if (= (i/get-value ra) (i/get-value rb)) := :r)
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
  ([a b]
   (diff a b nil))
  ([a b opts]
   (let [script (e/edits->script [])]
     (when-not (= a b)
       (let [opts  (-> opts
                       co/with-vec-deadline
                       (assoc ::cost-memo (volatile! (transient {}))))
             index-context (i/index-context)
             roota (i/index a index-context)
             rootb (i/index b index-context)
             root-coord (coord roota rootb)
             came  (volatile! {})
             cost  (diff-coord* roota rootb root-coord came opts)]
         ;; #?(:clj (let [total          (* (get-size roota) (get-size rootb))
         ;;               ^long explored (reduce + (map count (vals @came)))]
         ;;           (printf "cost is %d, explored %d of %d - %.1f%%\n"
         ;;                   cost explored total
         ;;                   (* 100 (double (/ explored total))))))
         (trace @came root-coord script opts)
         script))
     script)))

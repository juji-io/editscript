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
        head (->Node [] ::head 0 (sorted-map) 0)]
    (vswap! nodes conj head)
    (index* nodes [] data head)
    @nodes))

(defprotocol IState
  (operator [this])
  (cost [this])
  (neighbor [this]))

(deftype State [op co nb]
  IState
  (operator [_] op)
  (cost [_] co)
  (neighbor [_] nb))

(defmethod print-method State
  [x ^java.io.Writer writer]
  (print-method {:op (operator x)
                 :co (cost x)
                 :nb (neighbor x)}
                writer))

(defn- heuristic
  "An optimistic (never over-estimate) estimate of cost to reach goal when at (x y).

  For positive goal differential (delta), the optimal number of edits is deletion
  dependent, equals to 2p+delta, where p is number of deletions. Optimistically
  assuming no new deletion will be needed after (x, y), the cost works out to
  be delta-(y-x). The same logic applies to negative delta.

  Because addition/replacement requires new value to be present in
  editscript, whereas deletion does not, we assign unit cost 2 to
  addition/replacement, 1 to deletion."
  [[x y] [gx gy]]
  (let [delta (- gy gx)
        cost  (Math/abs (- delta (- y x)))]
    (if (< delta 0)
      cost
      (* 2 cost))))

(defn- trace
  [came cur]
  ;; (println (str "explored: " (count came)))
  (loop [c cur ops '()]
    (if-let [[prev op] (came c)]
      (recur prev (conj ops op))
      (vec ops))))

(defn- explore
  [cur goal {:keys [open closed came g] :as m} state]
  (println "open is" open)
  (let [[co op nb] ((juxt cost operator neighbor) state)]
    (if (closed nb)
     m
     (let [gc    (get g cur Long/MAX_VALUE)
           tmp-g (if (= gc Long/MAX_VALUE) gc (+ gc co))]
       (println "exploring: " state " at " cur)
       (if (>= tmp-g (get g nb Long/MAX_VALUE))
         (if (open nb)
           m
           (assoc! m :open (assoc open nb Long/MAX_VALUE)))
         (do
           (println "got nb" nb "with cost" tmp-g)
           (assoc! m
                  :came (assoc! came nb [cur op])
                  :g (assoc! g nb tmp-g)
                  :open (assoc open nb (+ tmp-g (heuristic nb goal))))))))))

(defn- value+size [node] ((juxt get-value get-size) node))

(defn- frontier-local
  [ai bi [x y] [gx gy] global]
  (let [na      (get ai x)
        nb      (get bi y)
        [va sa] (value+size na)
        [vb sb] (value+size nb)
        ta      (get-type va)
        tb      (get-type vb)
        x+1     (inc x)
        y+1     (inc y)]
    (if (and (= ta tb) (= va vb))
      [(->State := 0 [x+1 y+1])]
      (cond-> []
        (< x gx)       (conj (->State :- 1 [x+1 y]))
        (and (< x gx)
             (< y gy)) (conj (->State :r
                                      (let [r (get global
                                                   [(get-order na) (get-order nb)]
                                                   Long/MAX_VALUE)]
                                        (println "r " r)
                                        (if (= r Long/MAX_VALUE) r (inc r)))
                                      [x+1 y+1]))
        (< y gy)       (conj (->State :+ (inc sb) [x y+1]))))))

(defn A*-local
  "A* algorithm, works on the indices of input data"
  [ai bi global]
  (println "local ai " ai)
  (println "local bi " bi)
  (let [rank (fn [idx] (-> idx last get-children count))
        goal [(rank ai) (rank bi)]
        init [0 0]]
    (loop [{:keys [open closed came g] :as m}
           (transient
            {:open   (p/priority-map init (heuristic init goal))
             :closed (transient #{})
             :came   (transient {})
             :g      (transient {init 0})})]
      (if (empty? open)
        "failed"
        (let [[cur cost] (peek open)]
          (if (= cur goal)
            {:cur cur :cost cost :trace (trace (persistent! came) cur)}
            (recur (reduce
                    (partial explore cur goal)
                    (assoc! m :open (pop open) :closed (conj! closed cur))
                    (frontier-local ai bi cur goal global)))))))))

(defn children-nodes [node] (-> node get-children vals vec))

(defn- replace-cost
  [ai bi na nb g]
  (println "replace na " na " with " nb)
  (let []
    ))

(defn- frontier
  [ai bi [x y] [gx gy] g]
  (let [na      (get ai x)
        nb      (get bi y)
        [va sa] (value+size na)
        [vb sb] (value+size nb)
        x+1     (inc x)
        y+1     (inc y)]
    (cond-> []
      (< x gx)
      (conj (->State :- 1 [x+1 y]))

      (and (< x gx) (< y gy))
      (conj
       (if (= va vb)
         (->State := 0 [x+1 y+1])
         (->State :r
                  (if (and (= (get-type va) (get-type vb)) (> sa 1))
                    (:cost (A*-local (conj (children-nodes na) na)
                                     (conj (children-nodes nb) nb)
                                     g))
                    (inc sb))
                  [x+1 y+1])))

      (< y gy)
      (conj (->State :+ (inc (get-size nb)) [x y+1])))))

(defn- init-g [init [gx gy]]
  (merge {init 0}
         (zipmap (map (fn [x] [x 0]) (range 1 (inc gx))) (repeat 1))
         (zipmap (map (fn [x] [0 x]) (range 1 (inc gy))) (repeat 1))))

(defn A*
  "A* algorithm, works on the indices of input data"
  [ai bi]
  (let [size (fn [idx] (-> idx last get-size))
        goal [(size ai) (size bi)]
        init [1 1]]
    (println "goal " goal)
    (loop [{:keys [open closed came g] :as m}
           (transient
            {:open   (p/priority-map init (heuristic init goal))
             :closed (transient #{})
             :came   (transient {})
             :g      (transient (init-g init goal))})]
      (if (empty? open)
        "failed"
        (let [[cur cost] (peek open)]
          (if (= cur goal)
            {:cur cur :cost cost :trace (trace (persistent! came) cur)}
            (recur (reduce
                    (partial explore cur goal)
                    (assoc! m :open (pop open) :closed (conj! closed cur))
                    (frontier ai bi cur goal g)))))))))

(defn diff
  "Create an EditScript that represents the minimal difference between `b` and `a`"
  [a b]
  (let [script (->EditScript [] 0 0 0)]
    (when-not (identical? a b)
      (let [{:keys [cur cost came]} (A* a b)]
        (trace came cur)))
    script))

(comment

  (def a [[:a] [:s :t] [:u]])
  (def b [[:s] :t :s])
  (A* (index a) (index b))
  (index a)
  (index b)

  (def a (vec (seq "acbdeacbed")))
  (def b (vec (seq "acebdabbabed")))
  (A* (index a) (index b))

  (def a (vec (seq "ab")))
  (def b (vec (seq "bc")))
  (A* (index a) (index b))
  )

(ns editscript.diff.a-star
  (:require [clojure.set :as set]
            [clojure.data.priority-map :as p]
            [editscript.core :refer :all]))

(set! *warn-on-reflection* true)

(defrecord Node [path type size order value])

(defn- index-map [nodes i path data]
  (let [n  (count data)
        i' (+ i n)]
    (vswap! nodes conj (->Node path :map n i' data))
    (reduce-kv
     (fn [_ k v]
       (index* nodes i' (conj path k) v))
     nil
     data)))

(defn- index* [nodes i path data]
  (println "data is " data)
  (case (get-type data)
    :map (index-map nodes i path data)
    :val (vswap! nodes conj (->Node path :val 1 (inc i) data))
    ))

(defn- index
  "Traverse data as a tree to build a vector of Node"
  [data]
  (let [nodes (volatile! [])]
    (index* nodes 0 [] data)
    nodes))
(index {:a 3})

(defn- diag [[x y]] (- y x))

(defn- heuristic [cur goal]
  (Math/abs (- ^long (diag cur) ^long (diag goal))))

(defn- trace [came cur]
  (loop [c cur ops '()]
    (if-let [[prev op] (came c)]
      (recur prev (conj ops op))
      (vec ops))))

(defn A* [a b]
  (let [gx   (count a)
        gy   (count b)
        goal [gx gy]
        dist (fn [[x y] [x' y']]
               (if (and (= (get a x) (get b y))
                        (= 1 (- x' x))
                        (= 1 (- y' y)))
                 0
                 1))
        ops  (fn [[x y]]
               (cond-> []
                 (and (< x gx) (< y gy)) (conj :r)
                 (< x gx)                (conj :-)
                 (< y gy)                (conj :+)))
        dest (fn [[x y] op]
               (case op
                 :+ [x (inc y)]
                 :- [(inc x) y]
                 :r [(inc x) (inc y)]))]
    (loop [open   (p/priority-map [0 0] (heuristic [0 0] goal))
           closed #{}
           came   {}
           g      {[0 0] 0}]
      (if (empty? open)
        "failed"
        (let [[cx cy :as cur] (key (peek open))]
          (if (= cur goal)
            (trace came cur)
            (let [{:keys [open closed came g]}
                  (reduce
                   (fn [{:keys [open closed came g] :as m} op]
                     (let [neighbor (dest cur op)]
                       (if (closed neighbor)
                         m
                         (let [tmp-g (+ (get g cur Long/MAX_VALUE) (dist cur neighbor))]
                           (if (>= tmp-g (get g neighbor Long/MAX_VALUE))
                             (assoc m :open (assoc open neighbor Long/MAX_VALUE))
                             (assoc m
                                    :came (assoc came neighbor [cur op])
                                    :g (assoc g neighbor tmp-g)
                                    :open (assoc open neighbor
                                                 (+ tmp-g (heuristic cur goal) ))))))))
                   {:open   (pop open)
                    :closed (conj closed cur)
                    :came   came
                    :g      g}
                   (ops cur))]
              (recur open closed came g))))))))

(def a (vec (seq "a")))
(def b (vec (seq "ac")))
(def a (vec (seq "acbdeacbed")))
(def b (vec (seq "acebdabbabed")))
(A* a b)

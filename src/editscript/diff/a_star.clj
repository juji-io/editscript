(ns editscript.diff.a-star
  (:require [clojure.set :as set]
            [clojure.data.priority-map :as p]
            [editscript.core :refer :all]))

(defn- diag [[x y]] (- y x))

(defn- heuristic [cur goal]
  (Math/abs (- (diag cur) (diag goal))))

(defn vec-a* [a b]
  (let [goal [(count a) (count b)]
        dist (fn [[x y] [x' y']]
               (if (and (= (get a x) (get b y))
                        (= 1 (- x' x))
                        (= 1 (- y' y)))
                 0
                 1))]
    (loop [open   (p/priority-map [0 0] (heuristic [0 0] goal))
           closed #{}
           came   {}
           g      {[0 0] 0}]
      (if (empty? open)
        "failed"
        (let [[cx cy :as cur] (key (peek open))]
          (if (= cur goal)
            [came cur]
            (let [{:keys [open closed came g]}
                  (reduce
                   (fn [{:keys [open closed came g] :as m} neighbor]
                     (if (closed neighbor)
                       m
                       (let [tmp-g (+ (get g cur Long/MAX_VALUE) (dist cur neighbor))]
                         (if (>= tmp-g (get g neighbor Long/MAX_VALUE))
                           (assoc m :open (assoc open neighbor Long/MAX_VALUE))
                           (assoc m
                                  :came (assoc came neighbor cur)
                                  :g (assoc g neighbor tmp-g)
                                  :open (assoc open neighbor
                                               (+ tmp-g (heuristic cur goal) )))))))
                   {:open   (pop open)
                    :closed (conj closed cur)
                    :came   came
                    :g      g}
                   [[cx (inc cy)]
                    [(inc cx) cy]
                    [(inc cx) (inc cy)]])]
              (recur open closed came g))))))))

(def a (vec (seq "a")))
(def b (vec (seq "ab")))
(vec-a* a b)

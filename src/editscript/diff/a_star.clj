(ns editscript.diff.a-star
  (:require [clojure.set :as set]
            [clojure.data.priority-map :as p]
            [editscript.core :refer :all]))

(defn- diag [[x y]] (- y x))

(defn- heuristic [cur goal]
  (Math/abs (- (diag cur) (diag goal))))

(defn vec-a* [a b]
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
                 (and (< x gx)
                      (< y gy)) (conj [(inc x) (inc y)])
                 (< x gx)       (conj [(inc x) y])
                 (< y gy)       (conj [x (inc y)])))]
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
                   (ops cur))]
              (recur open closed came g))))))))

(def a (vec (seq "acbdeacbed")))
(def b (vec (seq "acebdabbabed")))
(vec-a* a b)

(ns editscript.core
  (:require [clojure.set :as set]))

(def a {:a 1 :b 'b :c {:d 3}})
(def b {:b 'c :c {:d 2} :e 5 :f 6})

[[[:e] ::+ 5]
 [[:f] ::+ 6]
 [[:a] ::-]
 [[:c :d] ::+ 2]
 [[:b] ::+ 'c]]

(def c [3 'c {:a 3} 4])
(def d [3 'c {:b 3} 4])

[[[2 :a] ::-]
 [[2 :b] ::+ 3]]

(defn get-type [v]
  (cond
    (nil? v)                :nil
    (map? v)                :map
    (vector? v)             :map
    (and (sequential? v)
         (not (string? v))) :seq
    :else                   :val))

(defn path? [p]
  (and (vector? p)
       (::path (meta p))))

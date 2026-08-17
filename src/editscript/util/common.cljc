;;
;; Copyright (c) Huahai Yang. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.
;;

(ns ^:no-doc editscript.util.common
  (:refer-clojure :exclude [slurp])
  (:require [editscript.edit :as e]
            [editscript.util.arrays :as arrays
             #?@(:cljs [:include-macros true])]
            [clojure.string :as s])
  #?(:cljr (:import [System.Diagnostics Stopwatch])))

#?(:clj (set! *warn-on-reflection* true))
#?(:clj (set! *unchecked-math* :warn-on-boxed))

(defn szudzik
  "Szudzik's paring function"
  [^long x ^long y]
  (if (> y x)
    (+ x (* y y))
    (+ x y (* x x))))

(defmacro coll-case
  [a b script path type diff-fn opts]
  `(case (e/get-type ~b)
     :nil  (e/delete-data ~script ~path)
     ~type (~diff-fn ~script ~path ~a ~b ~opts)
     (e/replace-data ~script ~path ~b)))

(defn current-time
  []
  #?(:clj  (quot (System/nanoTime) 1000000)
     :cljs (long (.now js/performance))
     :cljr (quot (* (Stopwatch/GetTimestamp) 1000)
                 Stopwatch/Frequency)))

(defn with-vec-deadline
  "Attach one absolute vector-diff deadline to an options map.

  Recursive vector, list, and string searches reuse this deadline instead of
  restarting the timeout for every nested comparison. A nil `:vec-timeout`
  continues to disable the timeout."
  [opts]
  (let [opts    (or opts {})
        timeout (get opts :vec-timeout 1000)]
    (if (or (contains? opts ::vec-deadline) (nil? timeout))
      opts
      (assoc opts ::vec-deadline
             (+ (long (current-time)) (long timeout))))))

(defn vec-timed-out?
  [opts]
  (when-let [deadline (::vec-deadline opts)]
    (<= ^long deadline (long (current-time)))))

(def ^:private deadline-check-mask (long 255))

(defn- periodic-timeout?
  [deadline ^long iterations]
  (and deadline
       (pos? iterations)
       (zero? (bit-and iterations (long deadline-check-mask)))
       (<= ^long deadline (long (current-time)))))

#?(:cljs
   (defprotocol ITrace
     (addTrace [trace previous edge])
     (previousAt [trace trace-id])
     (edgeAt [trace trace-id]))
   :bb
   (defprotocol ITrace
     (addTrace [trace previous edge])
     (previousAt [trace trace-id])
     (edgeAt [trace trace-id]))
   :default
   (definterface ITrace
     (^long addTrace [^long previous ^long edge])
     (^long previousAt [^long trace-id])
     (^long edgeAt [^long trace-id])))

(deftype Trace [^:unsynchronized-mutable previous
                ^:unsynchronized-mutable edges
                ^:unsynchronized-mutable ^long size]
  ITrace
  (addTrace [_ predecessor edge]
    (let [index size]
      (when (= index (arrays/ints-count previous))
        (set! previous (arrays/grow-ints previous))
        (set! edges (arrays/grow-ints edges)))
      (arrays/set-int-at! previous index predecessor)
      (arrays/set-int-at! edges index edge)
      (set! size (inc (long index)))
      index))
  (previousAt [_ trace-id]
    (arrays/int-at previous trace-id))
  (edgeAt [_ trace-id]
    (arrays/int-at edges trace-id)))

(defn- make-trace
  [capacity]
  (->Trace (arrays/make-ints capacity)
           (arrays/make-ints capacity)
           0))

(defn- trace-edits
  [^Trace trace ^long trace-id]
  (loop [trace-id (long trace-id)
         reversed (transient [])]
    (if (neg? trace-id)
      (into [] (rseq (persistent! reversed)))
      (let [previous       (long #?(:cljs (previousAt trace trace-id)
                                    :bb (previousAt trace trace-id)
                                    :default
                                    (.previousAt ^ITrace trace trace-id)))
            edge           (long #?(:cljs (edgeAt trace trace-id)
                                    :bb (edgeAt trace trace-id)
                                    :default
                                    (.edgeAt ^ITrace trace trace-id)))
            snake          (dec #?(:clj  (Math/abs edge)
                                   :cljs (js/Math.abs edge)
                                   :cljr (Math/Abs edge)))
            reversed (if (pos? snake) (conj! reversed snake) reversed)
            reversed (if (neg? previous)
                       reversed
                       (conj! reversed (if (neg? edge) :- :+)))]
        (recur (long previous) reversed)))))

(defn- vec-edits*
  "Based on 'Wu, S. et al., 1990, An O(NP) Sequence Comparison Algorithm,
  Information Processing Letters, 35:6, p317-23.'

  A greedy algorithm, attempting to get to the furthest points with a given
  number of edits. Very fast. However, it does not have replacement operations,
  so it is not very useful for nested trees. It can also only do unit cost for
  addition and deletion. "
  [a b n m deadline]
  (let [^long n n
        ^long m m
        delta   (- n m)
        offset  (inc m)
        ;; Zero is the missing-value sentinel in both arrays. Furthest x
        ;; positions and trace ids are stored incremented so p=0 needs no
        ;; initialization pass, which matters when the length delta is large.
        furthest (arrays/make-ints (+ n m 3))
        paths    (arrays/make-ints (+ n m 3))
        trace    (make-trace (max 16 (inc delta)))
        timed-out? (volatile! false)
        snake   (fn [^long k ^long x]
                  (loop [x x
                         y (- x k)
                         until-check (long 256)]
                    (if (and (< x n)
                             (< y m)
                             (let [ax (get a x)
                                   by (get b y)]
                               (and (= (type ax) (type by))
                                    (= ax by))))
                      (let [x'          (inc x)
                            y'          (inc y)
                            until-check (dec until-check)]
                        (if (zero? until-check)
                          (if (and deadline
                                   (<= ^long deadline (long (current-time))))
                            (do (vreset! timed-out? true) x')
                            (recur x' y' 256))
                          (recur x' y' until-check)))
                      x)))
        fp-fn   (fn [^long k]
                  (let [index        (long (+ offset k))
                        from-delete  (long (arrays/int-at furthest
                                                          (dec index)))
                        from-add     (dec (long (arrays/int-at
                                                 furthest (inc index))))
                        delete?      (> from-delete from-add)
                        x            (long (if delete? from-delete from-add))
                        previous     (dec (long (arrays/int-at
                                                 paths
                                                 (if delete?
                                                   (dec index)
                                                   (inc index)))))
                        ^long sk     (snake k x)
                        snake-length (long (- sk x))
                        edge         (long (if delete?
                                             (- (inc snake-length))
                                             (inc snake-length)))
                        trace-id     (long #?(:cljs (addTrace trace previous
                                                              edge)
                                             :bb (addTrace trace previous edge)
                                             :default
                                             (.addTrace ^ITrace trace previous
                                                        edge)))]
                    (arrays/set-int-at! furthest index (inc sk))
                    (arrays/set-int-at! paths index (inc trace-id))))]
    (loop [p (long 0)]
      (if (and deadline (<= ^long deadline (long (current-time))))
        :timeout
        (let [low-boundary  (+ offset (- p) -1)
              high-boundary (+ offset delta p 1)
              _             (do
                              (arrays/set-int-at! furthest low-boundary 0)
                              (arrays/set-int-at! paths low-boundary 0)
                              (arrays/set-int-at! furthest high-boundary 0)
                              (arrays/set-int-at! paths high-boundary 0))
              ^long updates
              (loop [k (- p)
                     updates (long 0)]
                (if (< k delta)
                  (if (and deadline
                           (periodic-timeout? deadline updates))
                    -1
                    (do (fp-fn k)
                        (if (and deadline @timed-out?)
                          -1
                          (recur (inc k) (inc updates)))))
                  updates))]
          (if (neg? updates)
            :timeout
            (let [^long updates
                  (loop [k (+ delta p)
                         updates (long updates)]
                    (if (< delta k)
                      (if (and deadline
                               (periodic-timeout? deadline updates))
                        -1
                        (do (fp-fn k)
                            (if (and deadline @timed-out?)
                              -1
                              (recur (dec k) (inc updates)))))
                      updates))]
              (if (neg? updates)
                :timeout
                (do
                  (fp-fn delta)
                  (cond
                    (or (and deadline @timed-out?)
                        (and deadline
                             (<= ^long deadline (long (current-time)))))
                    :timeout

                    (= (inc n) (arrays/int-at furthest (+ offset delta)))
                    (trace-edits trace
                                 (dec (arrays/int-at paths (+ offset delta))))

                    :else
                    (recur (inc p))))))))))))

(defn- swap-ops
  [edits]
  (mapv (fn [op] (case op :+ :- :- :+ op)) edits))

(defn min+plus->replace
  "Aggressively turn :- and :+ into replacements."
  [v]
  (into []
        (comp
          (partition-by integer?)
          (mapcat
            (fn [coll]
              (let [m (nth coll 0)]
                (if (or (integer? m) (= 1 (count coll)))
                  coll
                  (let [p       (if (= m :-) :+ :-)
                        [ms ps] (split-with #(= % m) coll)
                        mc      (count ms)
                        pc      (count ps)
                        delta   (#?(:cljr Math/Abs :default Math/abs) (- mc pc))
                        rs      (repeat (- (max mc pc) delta) :r)]
                    (cond
                      (< mc pc) (concat rs (repeat delta p))
                      (= mc pc) rs
                      :else     (concat (repeat delta m) rs))))))))
        v))

(defn vec-edits
  [a b opts]
  (let [opts (with-vec-deadline opts)
        deadline (::vec-deadline opts)
        a (vec a)
        b (vec b)
        n (count a)
        m (count b)
        e (if (< n m)
            (vec-edits* b a m n deadline)
            (vec-edits* a b n m deadline))]
    (if (= e :timeout)
      e
      (min+plus->replace (if (< n m) (swap-ops e) e)))))

(defn- group-strs
  [edits b level]
  (let [sf (if (= level :character) subs subvec)
        i  (volatile! 0)]
    (into []
          (comp
            (partition-by identity)
            (mapcat
              (fn [coll]
                (let [x (nth coll 0)
                      c (count coll)]
                  (cond
                    (integer? x) (do (vswap! i (partial + x)) coll)
                    (= :- x)     [[x c]]
                    (= :r x)     (let [s (sf b @i (+ ^long @i c))]
                                   (vswap! i (partial + c))
                                   [[x s]])
                    (= :+ x)     (let [s (sf b @i (+ ^long @i c))]
                                   (vswap! i (partial + c))
                                   [[x s]]))))))
          edits)))

(defn transform-str
  [s level]
  (case level
    :character s
    :word      (vec (s/split s #" "))
    :line      (vec (s/split-lines s))
    (throw (ex-info "Unknown string diff level" {:str-diff level}))))

(defn diff-str
  [script path a b {:keys [str-change-limit str-diff]
                    :or   {str-change-limit 0.2}
                    :as   opts}]
  (let [a'    (transform-str a str-diff)
        b'    (transform-str b str-diff)
        edits (vec-edits a' b' opts)]
    (if (= edits :timeout)
      (e/replace-data script path b)
      (let [ca        (count a')
            unchanged (double (transduce (filter integer?) + edits))]
        (if (and (< 0 str-change-limit 1.0)
                 (< (* ca (- 1.0 ^double str-change-limit)) unchanged))
          (let [edits' (group-strs edits b' str-diff)]
            (e/replace-str script path edits' str-diff))
          (e/replace-data script path b))))))

#?(:clj (defmacro vslurp
          [file]
          (clojure.core/slurp file)))

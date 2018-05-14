;;
;; Copyright (c) Huahai Yang. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.
;;

(ns editscript.util.common
  (:refer-clojure :exclude [slurp]))

#?(:clj (set! *warn-on-reflection* true))
#?(:clj (set! *unchecked-math* :warn-on-boxed))

(defn szudzik
  "Szudzik's paring function"
  [^long x ^long y]
  (if (> y x)
    (+ x (* y y))
    (+ x y (* x x))))

#?(:clj
   (defn string->bytes
     [^String s]
     (->> (.getBytes s "UTF-16BE")
          (map byte)))
   :cljs
   (defn string->bytes
     [s]
     (->> s
          (mapcat #(let [u (.charCodeAt % 0)]
                     [(-> u (bit-and 0xFF00) (bit-shift-right 8))
                      (bit-and u 0xFF)]))
          (map #(if (>= % 128)
                  (- % 256)
                  %))
          (map byte))))

(defn ->bytes
  [value]
  (-> value pr-str string->bytes))

(defmacro coll-case
  [a b script path type diff-fn]
  `(case (e/get-type ~b)
     :nil  (e/delete-data ~script ~path)
     ~type (~diff-fn ~script ~path ~a ~b)
     (e/replace-data ~script ~path ~b)))

#?(:clj (defmacro vslurp
          [file]
          (clojure.core/slurp file)))

(defn bit-shift-right-hint
  [^long x ^long n]
  (bit-shift-right x n))

(defn bit-or-hint
  [^long x ^long y]
  (bit-or x y))

(defn bit-and-hint
  ^long [^long x ^long y]
  (bit-and x y))

(defmacro log2step
  [r x b s]
  `(when (> (bit-and-hint @~x ~b) 0)
     (vswap! ~x bit-shift-right-hint ~s)
     (vswap! ~r bit-or-hint  ~s)))


(comment



  )

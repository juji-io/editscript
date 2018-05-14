;;
;; Copyright (c) Huahai Yang. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.
;;

(ns editscript.util.rabin
  (:require [editscript.util.common :refer [log2step]
             #?@(:cljs [:include-macros true])]))

#?(:clj (set! *warn-on-reflection* true))
#?(:clj (set! *unchecked-math* :warn-on-boxed))

(defn padd
  "Add two polynomials from finite field GF(2^53)"
  ^long [^long x ^long y]
  (bit-xor x y))

(defn pdeg
  "Find the degree of a polynomial x, return -1 if x = 0"
  ^long [^long x]
  (if (= x 0)
    -1
    (let [r (volatile! 0)
          x (volatile! x)]
      (log2step r x 0xffffffff00000000 32)
      (log2step r x 0xffff0000 16)
      (log2step r x 0xff00 8)
      (log2step r x 0xf0 4)
      (log2step r x 0xc 2)
      (log2step r x 0x2 1)
      @r)))

(defn pmul
  ^long [^long x ^long y]
  (if (= 0 x y)
    0
    ))

(comment

  (type (padd 0 1))

  )

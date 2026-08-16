;;
;; Copyright (c) Huahai Yang. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of the license.
;; You must not remove this notice, or any other, from this software.
;;

(ns ^:no-doc editscript.util.arrays)

(defmacro ints-count
  [array]
  #?(:cljr `(alength ~array)
     :default
     (if (:ns &env)
       `(.-length ~array)
       (let [array (vary-meta array assoc :tag 'ints)]
         `(alength ~array)))))

(defmacro int-at
  [array index]
  #?(:cljr `(aget ~array (int ~index))
     :default
     (if (:ns &env)
       `(aget ~array ~index)
       (let [array (vary-meta array assoc :tag 'ints)]
         `(aget ~array (int ~index))))))

(defmacro set-int-at!
  [array index value]
  #?(:cljr `(aset ~array (int ~index) (int ~value))
     :default
     (if (:ns &env)
       `(aset ~array ~index ~value)
       (let [array (vary-meta array assoc :tag 'ints)]
         `(aset-int ~array (int ~index) (int ~value))))))

(defn make-ints
  "Create zero-initialized native 32-bit integer storage."
  [size]
  #?(:clj  (int-array (int size))
     :cljs (js/Int32Array. size)
     :cljr (int-array (int size))))

(defn grow-ints
  "Double the capacity of native integer storage, retaining its contents."
  [array]
  (let [length #?(:clj  (alength ^ints array)
                  :cljs (.-length array)
                  :cljr (alength array))
        grown  (make-ints (max 16 (* 2 length)))]
    #?(:clj  (System/arraycopy array 0 grown 0 length)
       :cljs (.set grown array)
       :cljr (loop [index (long 0)]
               (when (< index length)
                 (aset grown (int index) (aget array (int index)))
                 (recur (inc index)))))
    grown))

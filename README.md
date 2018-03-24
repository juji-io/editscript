# editscript

A Clojure library designed to extract the differences between two Clojure data structures, `a` and `b`, as an "Edit Script", which represents the hopefully minimal steps necessary to transfrom `a` to `b`. `a` and `b` can be arbitary nested Clojure data consisting of maps, vectors, lists, sets and primitive values.

## Usage

For example:

```Clojure
(def a ["abc" 24 23 {:a [1 2 3]} 1 3 #{1 2}])
(def b [24 23 {:a [2 3]} 1 3 #{1 2 3}])

(use 'editscript.core)

;; Show edits between `a` and `b`
(get-edits (diff a b))

;;==>
;;   [[[0] :editscript.core/-] 
;;   [[2 :a 0] :editscript.core/-]
;;   [[5 3] :editscript.core/+ 3]] 

;; From the diff, we can patch `a` to get `b`
(= b (patch a (diff a b)))
;;==> true

```

## License

Copyright © 2018 Juji, Inc.

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.

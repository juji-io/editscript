# editscript

A Clojure library designed to extract the differences between two Clojure data
structures as an "Edit Script", which represents the minimal modification
necessary to transform one to another.

Currently, the library can diff and patch any nested Clojure data structures
consisting of regular maps, vectors, lists, sets and primitive values. It is our
hope that this could be a useful tool to further support the Clojure's unique
strength of "Data-Oriented Programming".

## Usage

A minimal example:

```Clojure
(use 'editscript.core)

;; Here are two pieces of data, a and b
(def a ["abc" 24 23 {:a [1 2 3]} 1 3 #{1 2}])
(def b [24 23 {:a [2 3]} 1 3 #{1 2 3}])

;; compute the editscript between a and b
(def d (diff a b))

;; get the edit distance
(edit-distance d)
;;==> 3

;; print the editscript
(get-edits d)
;;==>
;;  [[[0] :editscript.core/-]
;;   [[2 :a 0] :editscript.core/-]
;;   [[5 3] :editscript.core/+ 3]]

;; patch a with the editscript to get back b, so that
(= b (patch a d))
;;==> true

```

## License

Copyright © 2018 Juji, Inc.

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.

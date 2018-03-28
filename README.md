# editscript

A Clojure library designed to extract the differences between two Clojure data
structures as an "editscript", which represents the minimal modification
necessary to transform one to another.

Currently, the library can diff and patch any nested Clojure data structures
consisting of regular maps, vectors, lists, sets and primitive values.

Hopefully this little tool could be useful to further enhance the Clojure's unique
strength of [Data-Oriented Programming](https://livebook.manning.com/#!/book/the-joy-of-clojure-second-edition/chapter-14/1).

## Usage

The library is available at clojars

![](https://clojars.org/juji/editscript/latest-version.svg)

Here is a minimal example:

```Clojure
(use 'editscript.core)

;; Here are two pieces of data, a and b
(def a ["abc" 24 23 {:a [1 2 3]} 1 3 #{1 2}])
(def b [24 23 {:a [2 3]} 1 3 #{1 2 3}])

;; compute the editscript between a and b
(def d (diff a b))
;;==>
;;  [[[0] :editscript.core/-]
;;   [[2 :a 0] :editscript.core/-]
;;   [[5 3] :editscript.core/+ 3]]

;; get the edit distance
(edit-distance d)
;;==> 3

;; patch a with the editscript to get back b, so that
(= b (patch a d))
;;==> true

```

An editscript is a vector of edits, where each edit is a vector of two or three
elements.

The first element of an edit is the path, similar to the path vector in the
function call `update-in`. However, `update-in` only works for associative data
structures (map and vector), whereas the editscript works for map, vector, list
and set alike.

The second element of an edit is a keyword representing the edit operation,
which is one of `:editscript.core/-` (deletion), `:editscript.core/+` (addition),
and `:editscript.core/r `(replacement).

For addition and replacement operation, the third element is the value of new data.

## Prior work

For sequence comparison, we implement:

> Wu, S. et al., 1990, An O(NP) Sequence Comparison Algorithm, Information Processing Letters, 35:6, p317-23.'

This is the same algorithm implemented in
[diffit](https://github.com/friemen/diffit). Using their benchmark (see
commented code in editscript.core-test), our implementation has almost
identical performance as theirs.

Of course, our library also deals with arbitrary nested composition of maps, lists,
vectors and sets, for which we have not found an equivalent library.

## Caveat

In special cases where consecutive deletions involving a nested element occur in
a sequence, we do not guarantee that the generated editscript is minimal.

For example, `a` is `[2 3 {:a 4} 6]`, `b` is `[2 {:a 5} 6]`, `(diff a b)` will
generate `[[[1] :editscript.core/-] [[1] :editscript.core/-] [[1] :editscript.core/+ {:a 5}]]`, instead of the minimal `[[[1] :editscript.core/-] [[1 :a] :editscript.core/r 5] ]`. The reason is that it would be expensive to
resolve the ambiguity of which deletion (`3` or `{:a 4}`) could be a candidate
for replacement (i.e. to drill down), so we currently only drill down the
unambiguous cases. We may change this in the future.

## License

Copyright © 2018 Juji, Inc.

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.

# editscript

A Clojure library designed to extract the differences between two Clojure data
structures as an "editscript", which represents the minimal modification
necessary to transform one to another.

Currently, the library can diff and patch any nested Clojure data structures
consisting of regular maps, vectors, lists, sets and primitive values. I have
not found an equivalent library so I implemented my own. Hopefully this little
tool could be useful to further enhance the Clojure's unique
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

d
;;==>
;;  [[[0] :-]
;;   [[2 :a 0] :-]
;;   [[5 3] :+ 3]]

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
which is one of `:-` (deletion), `:+` (addition), and `:r `(replacement).

For addition and replacement operation, the third element is the value of new data.

## Diffing Algorithms

The library currently implements two differing algorithms. The default algorithm
produces diffs that are optimal in the number of transformations and the
resulting script size. A quick and dirty algorithm is also provided, which does not
guarantee optimal results but is very fast.

### Optimizing diffing

This algorithm aims to achieve optimal diffing in term of minimal size of resulting
editscript. It is inspired by the following papers:

> Shin-Yee Lu, 1979, A Tree-to-tree distance and its application to cluster
> analysis. IEEE Transactions on Pattern Analysis and Machine Intelligence. Vol.
> PAMI-1 No.2. p219-224

> EIICHI Tanaka, 1995, A note on a tree-to-tree editing problem. International
> Journal of Pattern Recognition and Artificial Intelligence. p167-172

Unlike many other tree differing algorithms, our algorithm is structure preserving,
a notion formally defined in the above two papers. Roughly speaking, the edit
distance is defined on sub-trees rather than nodes, such that the ancestor-descendant
relationship and tree traversal order are preserved, and nodes in the original tree does
not split or merge. These properties are useful for diffing Clojure's immutable
data structures because we want to leverage structure sharing and use
`identical?` reference check to speedup comparison.

The two papers above describe diffing algorithms with O(|a||b|) time and space
complexity. I designed an A* based algorithm to achieve some speedup over that
bound. Instead of searching the whole transformation matrix, we will typically search
a portion of it along the diagonal. Currently, we are using a naive heuristic,
future work may improve it to obtain faster differing.

### Quick and dirty diffing

This algorithm simply does an one pass comparison of two trees so it is very
fast.

For sequence (vector and list) comparison, we implement:

> Wu, S. et al., 1990, An O(NP) Sequence Comparison Algorithm, Information Processing Letters, 35:6, p317-23.

This is a sequence diffing algorithm with O(NP) time complexity, where P is the number of deletions if `b` is longer than `a`.  The same sequence diffing algorithm is
also implemented in
[diffit](https://github.com/friemen/diffit). Using their benchmark (see
commented code in editscript.core-test), our implementation has almost identical
performance. Keep in mind that our algorithm handles nested Clojure data structures.

The quick and dirty algorithm does not always produce optimizing results. For
instances, when consecutive deletions involving a nested element occur in a
sequence, the generated editscript is not minimal. For example, `a` is `[2 3 {:a
4} 6]`, `b` is `[2 {:a 5} 6]`, this algorithm will generate `[[[1] :-] [[1] :-]
[[1] :+ {:a 5}]]`, instead of the minimal `[[[1] :-] [[1 :a] :r 5] ]`. The
reason is that there is ambiguity in which deletion (`3` or `{:a 4}`) needed to
be drilled down. This algorithm currently only
drills down in one unambiguous case, where `:-` is immediately followed by `:+`
and there's no `:-` before them. More such cases may be added.
However, adding more cases would not cover all the situations. An optimizing
algorithm is needed if minimal diffs are desired.

## License

Copyright © 2018 Juji, Inc.

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.

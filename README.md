# editscript

A Clojure library designed to extract the differences between two Clojure data
structures as an "editscript", which represents the minimal modification
necessary to transform one to another. Currently, the library can diff and patch
any nested Clojure data structures consisting of regular maps, vectors, lists,
sets and values.

At Juji, we need to take snapshots of our AI agents' states and later
restore them. Such a use case requires a good diffing library to avoid
overwhelming our storage systems. I have not found such a library in Clojure
ecosystem, so I implemented my own. Hopefully this little
library could be of some use to further enhance the Clojure's unique
strength of [Data-Oriented Programming](https://livebook.manning.com/#!/book/the-joy-of-clojure-second-edition/chapter-14/1).

## Usage

The library is available at clojars

![](https://clojars.org/juji/editscript/latest-version.svg)

Here is a usage example:

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

;; get the size of the editscript, size = new-data-size + 1
(get-size d)
;;==> 4

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

The library currently implements two diffing algorithms. The default algorithm
produces diffs that are optimal in the number of editing operations and the
resulting script size. A quick algorithm is also provided, which does not
guarantee optimal results but is very fast.

### A* diffing

This A* algorithm aims to achieve optimal diffing in term of minimal size of resulting
editscript, useful for storage, query and restoration. This is an original
algorithm that has some unique properties: unlike many other general tree
differing algorithms such as Zhang & Shasha 1989, our algorithm is structure preserving.

Roughly speaking, the edit distance is defined on sub-trees rather than nodes,
such that the ancestor-descendant relationship and tree traversal order are
preserved, and nodes in the original tree does not split or merge. These
properties are useful for diffing and patching Clojure's immutable data
structures because we want to leverage structure sharing and use `identical?`
reference checks. The additional constraints also yield algorithms with better run time
performance than the general ones. Finally, these constraints feel natural for a
Clojure programmer.

The structure preserving properties were proposed in Lu 1979 and Tanaka 1995.
These papers describe diffing algorithms with O(|a||b|) time and space
complexity. We designed an A* based algorithm to achieve some speedup. Instead
of searching the whole editing graph, we typically search a portion of it along
the diagonal.

The implementation is optimized for speed. Currently the algorithm spent most of
its running time calculating the cost of next steps, perhaps due to the use of a very
generic heuristic. A more specialized heuristic for our case should reduce the number of
steps considered.

Although much slower than the non-optimizing quick algorithm below, the algorithm is
practical for common Clojure data that include lots of maps. Maps and sets do
not incur the penalty of a large search space in the cases of vectors and lists.
For a [drawing data set](https://github.com/justsml/json-diff-performance), the
diffing time of the algorithm is in the range of 2ms to 5ms on a 2014 2.8 GHz
Core i5 16GB MacBook Pro.

### Quick diffing

This quick diffing algorithm simply does an one pass comparison of two trees so
it is very fast. For sequence (vector and list) comparison, we implement Wu et
al. 1990, an algorithm with O(NP) time complexity, where P is the
number of deletions if `b` is longer than `a`.  The same sequence diffing algorithm is
also implemented in [diffit](https://github.com/friemen/diffit). Using their
benchmark, our implementation has slightly better performance due to more
optimizations. Keep in mind that our algorithm also handles nested Clojure data
structures. Compared  with our A* algorithm, our quick algorithm is about two
orders of magnitude faster.

The Wu algorithm does not have replacement operations, and assumes each edit has
a unit cost. These do not work well for tree diffing. Consequently, the quick
algorithm does not produce optimizing results in term of
script size. In principle, simply changing a pointer to point to `b` instead of
`a` produces the fastest "diffing" algorithm of the world, but that is not very
useful. The quick algorithm has a similar problem.

For instances, when consecutive deletions involving nested elements occur in a
sequence, the generated editscript can be large. For example:

```Clojure
(def a [2 {:a 42} {:b 4} {:c 29}])
(def b [{:a 5} {:b 5} {:c 29}])

(editscript.diff.quick/diff a b)
;;==>
;;[[[0] :-]
;; [[0] :-]
;; [[0] :-]
;; [[0] :+ {:a 5}]
;; [[1] :+ {:b 5}]]

(editscript.diff.a-star/diff a b)
;;==>
;;[[[0] :-]
;; [[0 :a] :r 5]
;; [[1 :b] :r 5]]

```
In this case, the quick algorithm basically deletes the original and then add
new ones back. The reason is that the quick algorithm does not drill down
(i.e. do replacement) when it is needed. It currently only drills down in one
unambiguous case, where `:-` is immediately followed by `:+` and there's no `:-` before
them. More such cases may be added. However, adding more special cases would not
cover all the situations. An optimizing algorithm is needed if minimal diffs are
desired.

## Roadmap

There are a few things I plan to work on. Ideas, suggestions and contributions
are welcome.

* ClojureScript support, obviously.
* Better heuristic for the A* algorithm. Currently, we do not track the progress
  in term of the overall sizes covered, doing so should produce much more
  accurate estimate of work ahead, and significantly reduces the number of
  steps needed to be considered.
* Design a different API for the quick algorithm, since it is more suitable for detecting
  changes rather than producing editscript. Need to see some use cases.
* Support other data types, if people want...
* Explore other diffing algorithms

## References

+ Lu, S. 1979, A Tree-to-tree distance and its application to cluster analysis. IEEE Transactions on Pattern Analysis and Machine Intelligence. Vol. PAMI-1 No.2. p219-224

+ Tanaka, E., 1995, A note on a tree-to-tree editing problem. International
 Journal of Pattern Recognition and Artificial Intelligence. p167-172

+ Wu, S. et al., 1990, An O(NP) Sequence Comparison Algorithm, Information Processing Letters, 35:6, p317-23.

+ Zhang, K. and Shasha, D. 1989, Simple fast algorithms for the editing distance between trees and related problems. SIAM Journal of Computing, 18:1245–1262

## License

Copyright © 2018 Juji, Inc.

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.

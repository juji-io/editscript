(ns editscript.test
  (:require  [doo.runner :refer-macros [doo-tests]]
             [editscript.core-test]
             [editscript.patch-test]
             [editscript.util.index-test]
             [editscript.util.pairing-test]
             [editscript.diff.a-star-test]
             [editscript.diff.quick-test]))

(doo-tests 'editscript.util.pairing-test
           'editscript.util.index-test
           'editscript.diff.a-star-test
           'editscript.diff.quick-test
           'editscript.patch-test
           'editscript.core-test)

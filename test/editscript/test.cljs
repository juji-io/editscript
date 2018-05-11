(ns editscript.test
  (:require  [doo.runner :refer-macros [doo-tests]]
             editscript.core-test
             editscript.util.pairing-test
             editscript.util.common-test
             editscript.diff.a-star-test
             editscript.diff.quick-test))

(doo-tests 'editscript.util.pairing-test
           'editscript.util.common-test
           'editscript.diff.a-star-test
           'editscript.diff.quick-test
           'editscript.core-test)

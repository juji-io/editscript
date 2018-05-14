;;
;; Copyright (c) Huahai Yang. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file LICENSE at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.
;;

(ns editscript.util.common-test
  (:require [editscript.util.common :refer [string->bytes]]
            [clojure.test :refer [is testing deftest]]
            #?(:clj [clojure.test :as t]
               :cljs [cljs.test :as t :include-macros true])))

(deftest string->bytes-test
  (testing "convert string to a seq of bytes"
    (is (= (string->bytes "𝌆") '(-40 52 -33 6)) )
    (is (= (string->bytes "hello") '(0 104 0 101 0 108 0 108 0 111)) )
    (is (= (string->bytes "杨华海") '(103 104 83 78 109 119)))))

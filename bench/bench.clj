(ns bench
  (:require [clojure.data :as clj]
            [clojure.pprint :as pp]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.clojure-test :as test]
            [clojure.math.combinatorics :as combo]
            [criterium.core :as c]
            [differ.core :as differ]
            [editscript.core :as editscript]
            [lambdaisland.deep-diff2 :as deep]
            [taoensso.nippy :as nippy]))

(def data1 (-> "../resources/drawing1.edn"
               slurp
               read-string))
(def data2 (-> "../resources/drawing2.edn"
               slurp
               read-string))
(def data3 (-> "../resources/drawing3.edn"
               slurp
               read-string))
(def data4 (-> "../resources/drawing4.edn"
               slurp
               read-string))

;; original data size

(def size1 (count (nippy/freeze data1))) ;1004
(def size2 (count (nippy/freeze data2))) ;1004
(def size3 (count (nippy/freeze data3))) ;1016
(def size4 (count (nippy/freeze data4))) ;555

;; diff function candidates
(def diffs {:editscript-a*    #'editscript/diff
            :editscript-quick #(editscript/diff %1 %2 {:algo :quick})
            :differ           #'differ/diff
            :clj              #'clj/diff
            :deep             #'deep/diff})

;; diff sizes

(defn diff-size
  [k [d1 d2]]
  (count (nippy/fast-freeze ((diffs k) d1 d2))))

(pp/pprint
 (let [ids (combo/permuted-combinations [1 2 3 4] 2)
       res (for [k  (keys diffs)
                 ds (combo/permuted-combinations [data1 data2 data3 data4] 2)]
             [k (diff-size k ds)])]
   (map (fn[i r] [i r]) (cycle ids) res)))
;; ==>
;; [[1 2] [:editscript-a* 28]]
;; [[2 1] [:editscript-a* 28]]
;; [[1 3] [:editscript-a* 98]]
;; [[3 1] [:editscript-a* 96]]
;; [[1 4] [:editscript-a* 234]]
;; [[4 1] [:editscript-a* 778]]
;; [[2 3] [:editscript-a* 73]]
;; [[3 2] [:editscript-a* 71]]
;; [[2 4] [:editscript-a* 234]]
;; [[4 2] [:editscript-a* 778]]
;; [[3 4] [:editscript-a* 234]]
;; [[4 3] [:editscript-a* 780]]
;; [[1 2] [:editscript-quick 28]]
;; [[2 1] [:editscript-quick 28]]
;; [[1 3] [:editscript-quick 98]]
;; [[3 1] [:editscript-quick 96]]
;; [[1 4] [:editscript-quick 749]]
;; [[4 1] [:editscript-quick 1348]]
;; [[2 3] [:editscript-quick 73]]
;; [[3 2] [:editscript-quick 71]]
;; [[2 4] [:editscript-quick 749]]
;; [[4 2] [:editscript-quick 1348]]
;; [[3 4] [:editscript-quick 749]]
;; [[4 3] [:editscript-quick 1350]]
;; [[1 2] [:differ 22]]
;; [[2 1] [:differ 22]]
;; [[1 3] [:differ 61]]
;; [[3 1] [:differ 49]]
;; [[1 4] [:differ 259]]
;; [[4 1] [:differ 759]]
;; [[2 3] [:differ 46]]
;; [[3 2] [:differ 34]]
;; [[2 4] [:differ 259]]
;; [[4 2] [:differ 759]]
;; [[3 4] [:differ 259]]
;; [[4 3] [:differ 771]]
;; [[1 2] [:clj 1026]]
;; [[2 1] [:clj 1026]]
;; [[1 3] [:clj 1065]]
;; [[3 1] [:clj 1065]]
;; [[1 4] [:clj 1233]]
;; [[4 1] [:clj 1233]]
;; [[2 3] [:clj 1050]]
;; [[3 2] [:clj 1050]]
;; [[2 4] [:clj 1233]]
;; [[4 2] [:clj 1233]]
;; [[3 4] [:clj 1245]]
;; [[4 3] [:clj 1245]]
;; [[1 2] [:deep 1061]]
;; [[2 1] [:deep 1061]]
;; [[1 3] [:deep 1292]]
;; [[3 1] [:deep 1292]]
;; [[1 4] [:deep 2477]]
;; [[4 1] [:deep 2352]]
;; [[2 3] [:deep 1231]]
;; [[3 2] [:deep 1231]]
;; [[2 4] [:deep 2477]]
;; [[4 2] [:deep 2352]]
;; [[3 4] [:deep 2489]]
;; [[4 3] [:deep 2364]]

;; diff times

(def datas [data1 data2 data3 data4])

(defn diff-time
  [k [i1 i2]]
  (println [(inc i1) (inc i2)])
  (println k)
  (c/quick-bench ((diffs k) (datas i1) (datas i2)))
  (println "---"))

(doseq [k  (keys diffs)
        ds (combo/permuted-combinations [0 1 2 3] 2)]
  (diff-time k ds))
;;==>
;; [1 2]
;; :editscript-a*
;; Evaluation count : 798 in 6 samples of 133 calls.
;;              Execution time mean : 773.644301 µs
;;     Execution time std-deviation : 26.850668 µs
;;    Execution time lower quantile : 752.916910 µs ( 2.5%)
;;    Execution time upper quantile : 805.456752 µs (97.5%)
;;                    Overhead used : 9.988271 ns
;; ---
;; [2 1]
;; :editscript-a*
;; Evaluation count : 804 in 6 samples of 134 calls.
;;              Execution time mean : 797.029423 µs
;;     Execution time std-deviation : 55.806233 µs
;;    Execution time lower quantile : 746.675090 µs ( 2.5%)
;;    Execution time upper quantile : 872.635299 µs (97.5%)
;;                    Overhead used : 9.988271 ns
;; ---
;; [1 3]
;; :editscript-a*
;; Evaluation count : 624 in 6 samples of 104 calls.
;;              Execution time mean : 1.040535 ms
;;     Execution time std-deviation : 54.681728 µs
;;    Execution time lower quantile : 989.369183 µs ( 2.5%)
;;    Execution time upper quantile : 1.103212 ms (97.5%)
;;                    Overhead used : 9.988271 ns
;; ---
;; [3 1]
;; :editscript-a*
;; Evaluation count : 642 in 6 samples of 107 calls.
;;              Execution time mean : 975.321445 µs
;;     Execution time std-deviation : 49.837246 µs
;;    Execution time lower quantile : 929.690944 µs ( 2.5%)
;;    Execution time upper quantile : 1.052720 ms (97.5%)
;;                    Overhead used : 9.988271 ns

;; Found 1 outliers in 6 samples (16.6667 %)
;; 	low-severe	 1 (16.6667 %)
;;  Variance from outliers : 13.8962 % Variance is moderately inflated by outliers
;; ---
;; [1 4]
;; :editscript-a*
;; Evaluation count : 174 in 6 samples of 29 calls.
;;              Execution time mean : 3.806282 ms
;;     Execution time std-deviation : 414.903587 µs
;;    Execution time lower quantile : 3.407939 ms ( 2.5%)
;;    Execution time upper quantile : 4.427827 ms (97.5%)
;;                    Overhead used : 9.988271 ns
;; ---
;; [4 1]
;; :editscript-a*
;; Evaluation count : 168 in 6 samples of 28 calls.
;;              Execution time mean : 3.738995 ms
;;     Execution time std-deviation : 167.046468 µs
;;    Execution time lower quantile : 3.580161 ms ( 2.5%)
;;    Execution time upper quantile : 3.979797 ms (97.5%)
;;                    Overhead used : 9.988271 ns
;; ---
;; [2 3]
;; :editscript-a*
;; Evaluation count : 654 in 6 samples of 109 calls.
;;              Execution time mean : 932.668098 µs
;;     Execution time std-deviation : 17.850828 µs
;;    Execution time lower quantile : 916.113275 µs ( 2.5%)
;;    Execution time upper quantile : 960.310296 µs (97.5%)
;;                    Overhead used : 9.988271 ns
;; ---
;; [3 2]
;; :editscript-a*
;; Evaluation count : 654 in 6 samples of 109 calls.
;;              Execution time mean : 980.827651 µs
;;     Execution time std-deviation : 25.683973 µs
;;    Execution time lower quantile : 950.096853 µs ( 2.5%)
;;    Execution time upper quantile : 1.008664 ms (97.5%)
;;                    Overhead used : 9.988271 ns
;; ---
;; [2 4]
;; :editscript-a*
;; Evaluation count : 180 in 6 samples of 30 calls.
;;              Execution time mean : 3.500376 ms
;;     Execution time std-deviation : 61.447302 µs
;;    Execution time lower quantile : 3.438388 ms ( 2.5%)
;;    Execution time upper quantile : 3.598248 ms (97.5%)
;;                    Overhead used : 9.988271 ns

;; Found 1 outliers in 6 samples (16.6667 %)
;; 	low-severe	 1 (16.6667 %)
;;  Variance from outliers : 13.8889 % Variance is moderately inflated by outliers
;; ---
;; [4 2]
;; :editscript-a*
;; Evaluation count : 168 in 6 samples of 28 calls.
;;              Execution time mean : 3.725563 ms
;;     Execution time std-deviation : 262.610936 µs
;;    Execution time lower quantile : 3.521143 ms ( 2.5%)
;;    Execution time upper quantile : 4.111818 ms (97.5%)
;;                    Overhead used : 9.988271 ns
;; ---
;; [3 4]
;; :editscript-a*
;; Evaluation count : 180 in 6 samples of 30 calls.
;;              Execution time mean : 3.677128 ms
;;     Execution time std-deviation : 88.113806 µs
;;    Execution time lower quantile : 3.597034 ms ( 2.5%)
;;    Execution time upper quantile : 3.812988 ms (97.5%)
;;                    Overhead used : 9.988271 ns
;; ---
;; [4 3]
;; :editscript-a*
;; Evaluation count : 180 in 6 samples of 30 calls.
;;              Execution time mean : 3.730000 ms
;;     Execution time std-deviation : 153.056555 µs
;;    Execution time lower quantile : 3.571016 ms ( 2.5%)
;;    Execution time upper quantile : 3.883019 ms (97.5%)
;;                    Overhead used : 9.988271 ns
;; ---
;; [1 2]
;; :editscript-quick
;; Evaluation count : 12198 in 6 samples of 2033 calls.
;;              Execution time mean : 51.070344 µs
;;     Execution time std-deviation : 2.763842 µs
;;    Execution time lower quantile : 49.251526 µs ( 2.5%)
;;    Execution time upper quantile : 55.442976 µs (97.5%)
;;                    Overhead used : 9.914485 ns
;; ---
;; [2 1]
;; :editscript-quick
;; Evaluation count : 12186 in 6 samples of 2031 calls.
;;              Execution time mean : 50.069141 µs
;;     Execution time std-deviation : 1.492264 µs
;;    Execution time lower quantile : 49.280554 µs ( 2.5%)
;;    Execution time upper quantile : 52.591602 µs (97.5%)
;;                    Overhead used : 9.914485 ns

;; Found 1 outliers in 6 samples (16.6667 %)
;; 	low-severe	 1 (16.6667 %)
;;  Variance from outliers : 13.8889 % Variance is moderately inflated by outliers
;; ---
;; [1 3]
;; :editscript-quick
;; Evaluation count : 10968 in 6 samples of 1828 calls.
;;              Execution time mean : 55.998657 µs
;;     Execution time std-deviation : 2.093162 µs
;;    Execution time lower quantile : 54.985847 µs ( 2.5%)
;;    Execution time upper quantile : 59.620896 µs (97.5%)
;;                    Overhead used : 9.914485 ns

;; Found 1 outliers in 6 samples (16.6667 %)
;; 	low-severe	 1 (16.6667 %)
;;  Variance from outliers : 13.8889 % Variance is moderately inflated by outliers
;; ---
;; [3 1]
;; :editscript-quick
;; Evaluation count : 10620 in 6 samples of 1770 calls.
;;              Execution time mean : 57.524290 µs
;;     Execution time std-deviation : 3.249091 µs
;;    Execution time lower quantile : 54.687223 µs ( 2.5%)
;;    Execution time upper quantile : 62.746505 µs (97.5%)
;;                    Overhead used : 9.914485 ns

;; Found 1 outliers in 6 samples (16.6667 %)
;; 	low-severe	 1 (16.6667 %)
;;  Variance from outliers : 14.3992 % Variance is moderately inflated by outliers
;; ---
;; [1 4]
;; :editscript-quick
;; Evaluation count : 4374 in 6 samples of 729 calls.
;;              Execution time mean : 142.368241 µs
;;     Execution time std-deviation : 4.595377 µs
;;    Execution time lower quantile : 136.890344 µs ( 2.5%)
;;    Execution time upper quantile : 147.411795 µs (97.5%)
;;                    Overhead used : 9.914485 ns
;; ---
;; [4 1]
;; :editscript-quick
;; Evaluation count : 3264 in 6 samples of 544 calls.
;;              Execution time mean : 191.118444 µs
;;     Execution time std-deviation : 10.000057 µs
;;    Execution time lower quantile : 180.558489 µs ( 2.5%)
;;    Execution time upper quantile : 206.721659 µs (97.5%)
;;                    Overhead used : 9.914485 ns

;; Found 1 outliers in 6 samples (16.6667 %)
;; 	low-severe	 1 (16.6667 %)
;;  Variance from outliers : 14.0245 % Variance is moderately inflated by outliers
;; ---
;; [2 3]
;; :editscript-quick
;; Evaluation count : 10278 in 6 samples of 1713 calls.
;;              Execution time mean : 57.021706 µs
;;     Execution time std-deviation : 1.986452 µs
;;    Execution time lower quantile : 54.187186 µs ( 2.5%)
;;    Execution time upper quantile : 59.102288 µs (97.5%)
;;                    Overhead used : 9.914485 ns
;; ---
;; [3 2]
;; :editscript-quick
;; Evaluation count : 11100 in 6 samples of 1850 calls.
;;              Execution time mean : 55.908022 µs
;;     Execution time std-deviation : 1.817713 µs
;;    Execution time lower quantile : 53.433376 µs ( 2.5%)
;;    Execution time upper quantile : 57.914810 µs (97.5%)
;;                    Overhead used : 9.914485 ns
;; ---
;; [2 4]
;; :editscript-quick
;; Evaluation count : 4362 in 6 samples of 727 calls.
;;              Execution time mean : 152.033446 µs
;;     Execution time std-deviation : 9.858735 µs
;;    Execution time lower quantile : 140.585902 µs ( 2.5%)
;;    Execution time upper quantile : 165.442018 µs (97.5%)
;;                    Overhead used : 9.914485 ns

;; Found 1 outliers in 6 samples (16.6667 %)
;; 	low-severe	 1 (16.6667 %)
;;  Variance from outliers : 14.9464 % Variance is moderately inflated by outliers
;; ---
;; [4 2]
;; :editscript-quick
;; Evaluation count : 3222 in 6 samples of 537 calls.
;;              Execution time mean : 192.213030 µs
;;     Execution time std-deviation : 5.130478 µs
;;    Execution time lower quantile : 183.224266 µs ( 2.5%)
;;    Execution time upper quantile : 196.667713 µs (97.5%)
;;                    Overhead used : 9.914485 ns

;; Found 1 outliers in 6 samples (16.6667 %)
;; 	low-severe	 1 (16.6667 %)
;;  Variance from outliers : 13.8889 % Variance is moderately inflated by outliers
;; ---
;; [3 4]
;; :editscript-quick
;; Evaluation count : 4308 in 6 samples of 718 calls.
;;              Execution time mean : 143.629388 µs
;;     Execution time std-deviation : 2.164626 µs
;;    Execution time lower quantile : 140.990128 µs ( 2.5%)
;;    Execution time upper quantile : 145.594010 µs (97.5%)
;;                    Overhead used : 9.914485 ns
;; ---
;; [4 3]
;; :editscript-quick
;; Evaluation count : 3276 in 6 samples of 546 calls.
;;              Execution time mean : 190.921336 µs
;;     Execution time std-deviation : 8.595175 µs
;;    Execution time lower quantile : 182.267267 µs ( 2.5%)
;;    Execution time upper quantile : 202.823187 µs (97.5%)
;;                    Overhead used : 9.914485 ns
;; ---
;; [1 2]
;; :differ
;; Evaluation count : 4692 in 6 samples of 782 calls.
;;              Execution time mean : 130.251689 µs
;;     Execution time std-deviation : 4.965379 µs
;;    Execution time lower quantile : 125.748990 µs ( 2.5%)
;;    Execution time upper quantile : 137.095243 µs (97.5%)
;;                    Overhead used : 9.914485 ns
;; ---
;; [2 1]
;; :differ
;; Evaluation count : 4788 in 6 samples of 798 calls.
;;              Execution time mean : 131.736144 µs
;;     Execution time std-deviation : 5.533533 µs
;;    Execution time lower quantile : 126.604781 µs ( 2.5%)
;;    Execution time upper quantile : 139.900011 µs (97.5%)
;;                    Overhead used : 9.914485 ns
;; ---
;; [1 3]
;; :differ
;; Evaluation count : 3624 in 6 samples of 604 calls.
;;              Execution time mean : 132.421802 µs
;;     Execution time std-deviation : 6.584703 µs
;;    Execution time lower quantile : 125.880368 µs ( 2.5%)
;;    Execution time upper quantile : 139.977395 µs (97.5%)
;;                    Overhead used : 9.914485 ns
;; ---
;; [3 1]
;; :differ
;; Evaluation count : 4554 in 6 samples of 759 calls.
;;              Execution time mean : 136.006504 µs
;;     Execution time std-deviation : 7.906025 µs
;;    Execution time lower quantile : 128.161360 µs ( 2.5%)
;;    Execution time upper quantile : 149.353638 µs (97.5%)
;;                    Overhead used : 9.914485 ns

;; Found 2 outliers in 6 samples (33.3333 %)
;; 	low-severe	 1 (16.6667 %)
;; 	low-mild	 1 (16.6667 %)
;;  Variance from outliers : 14.5259 % Variance is moderately inflated by outliers
;; ---
;; [1 4]
;; :differ
;; Evaluation count : 11184 in 6 samples of 1864 calls.
;;              Execution time mean : 55.110374 µs
;;     Execution time std-deviation : 1.581278 µs
;;    Execution time lower quantile : 53.332328 µs ( 2.5%)
;;    Execution time upper quantile : 57.182550 µs (97.5%)
;;                    Overhead used : 9.914485 ns
;; ---
;; [4 1]
;; :differ
;; Evaluation count : 11442 in 6 samples of 1907 calls.
;;              Execution time mean : 53.383037 µs
;;     Execution time std-deviation : 966.106907 ns
;;    Execution time lower quantile : 51.765721 µs ( 2.5%)
;;    Execution time upper quantile : 54.300939 µs (97.5%)
;;                    Overhead used : 9.914485 ns
;; ---
;; [2 3]
;; :differ
;; Evaluation count : 4596 in 6 samples of 766 calls.
;;              Execution time mean : 134.591399 µs
;;     Execution time std-deviation : 4.340681 µs
;;    Execution time lower quantile : 129.622903 µs ( 2.5%)
;;    Execution time upper quantile : 140.494157 µs (97.5%)
;;                    Overhead used : 9.914485 ns

;; Found 1 outliers in 6 samples (16.6667 %)
;; 	low-severe	 1 (16.6667 %)
;;  Variance from outliers : 13.8889 % Variance is moderately inflated by outliers
;; ---
;; [3 2]
;; :differ
;; Evaluation count : 4698 in 6 samples of 783 calls.
;;              Execution time mean : 132.884566 µs
;;     Execution time std-deviation : 12.182945 µs
;;    Execution time lower quantile : 126.524015 µs ( 2.5%)
;;    Execution time upper quantile : 153.863420 µs (97.5%)
;;                    Overhead used : 9.914485 ns

;; Found 1 outliers in 6 samples (16.6667 %)
;; 	low-severe	 1 (16.6667 %)
;;  Variance from outliers : 15.8060 % Variance is moderately inflated by outliers
;; ---
;; [2 4]
;; :differ
;; Evaluation count : 11286 in 6 samples of 1881 calls.
;;              Execution time mean : 56.391931 µs
;;     Execution time std-deviation : 2.320742 µs
;;    Execution time lower quantile : 53.704304 µs ( 2.5%)
;;    Execution time upper quantile : 59.801777 µs (97.5%)
;;                    Overhead used : 9.914485 ns
;; ---
;; [4 2]
;; :differ
;; Evaluation count : 11328 in 6 samples of 1888 calls.
;;              Execution time mean : 56.260110 µs
;;     Execution time std-deviation : 6.948697 µs
;;    Execution time lower quantile : 51.563122 µs ( 2.5%)
;;    Execution time upper quantile : 67.361381 µs (97.5%)
;;                    Overhead used : 9.914485 ns

;; Found 1 outliers in 6 samples (16.6667 %)
;; 	low-severe	 1 (16.6667 %)
;;  Variance from outliers : 31.4365 % Variance is moderately inflated by outliers
;; ---
;; [3 4]
;; :differ
;; Evaluation count : 11280 in 6 samples of 1880 calls.
;;              Execution time mean : 60.423843 µs
;;     Execution time std-deviation : 5.352219 µs
;;    Execution time lower quantile : 54.745572 µs ( 2.5%)
;;    Execution time upper quantile : 66.492008 µs (97.5%)
;;                    Overhead used : 9.914485 ns
;; ---
;; [4 3]
;; :differ
;; Evaluation count : 11598 in 6 samples of 1933 calls.
;;              Execution time mean : 54.761285 µs
;;     Execution time std-deviation : 2.302883 µs
;;    Execution time lower quantile : 51.987826 µs ( 2.5%)
;;    Execution time upper quantile : 56.866173 µs (97.5%)
;;                    Overhead used : 9.914485 ns
;; ---
;; [1 2]
;; :clj
;; Evaluation count : 3936 in 6 samples of 656 calls.
;;              Execution time mean : 154.576426 µs
;;     Execution time std-deviation : 4.893273 µs
;;    Execution time lower quantile : 150.463867 µs ( 2.5%)
;;    Execution time upper quantile : 161.947815 µs (97.5%)
;;                    Overhead used : 9.914485 ns
;; ---
;; [2 1]
;; :clj
;; Evaluation count : 4026 in 6 samples of 671 calls.
;;              Execution time mean : 153.409655 µs
;;     Execution time std-deviation : 3.696940 µs
;;    Execution time lower quantile : 151.692890 µs ( 2.5%)
;;    Execution time upper quantile : 159.804653 µs (97.5%)
;;                    Overhead used : 9.914485 ns

;; Found 1 outliers in 6 samples (16.6667 %)
;; 	low-severe	 1 (16.6667 %)
;;  Variance from outliers : 13.8889 % Variance is moderately inflated by outliers
;; ---
;; [1 3]
;; :clj
;; Evaluation count : 1746 in 6 samples of 291 calls.
;;              Execution time mean : 354.272901 µs
;;     Execution time std-deviation : 19.061883 µs
;;    Execution time lower quantile : 332.853773 µs ( 2.5%)
;;    Execution time upper quantile : 373.026287 µs (97.5%)
;;                    Overhead used : 9.914485 ns
;; ---
;; [3 1]
;; :clj
;; Evaluation count : 1824 in 6 samples of 304 calls.
;;              Execution time mean : 335.982365 µs
;;     Execution time std-deviation : 5.528502 µs
;;    Execution time lower quantile : 330.878714 µs ( 2.5%)
;;    Execution time upper quantile : 342.536182 µs (97.5%)
;;                    Overhead used : 9.914485 ns
;; ---
;; [1 4]
;; :clj
;; Evaluation count : 744 in 6 samples of 124 calls.
;;              Execution time mean : 901.907897 µs
;;     Execution time std-deviation : 28.394167 µs
;;    Execution time lower quantile : 862.539403 µs ( 2.5%)
;;    Execution time upper quantile : 934.038488 µs (97.5%)
;;                    Overhead used : 9.914485 ns
;; ---
;; [4 1]
;; :clj
;; Evaluation count : 696 in 6 samples of 116 calls.
;;              Execution time mean : 869.258593 µs
;;     Execution time std-deviation : 10.533938 µs
;;    Execution time lower quantile : 856.507681 µs ( 2.5%)
;;    Execution time upper quantile : 881.642520 µs (97.5%)
;;                    Overhead used : 9.914485 ns
;; ---
;; [2 3]
;; :clj
;; Evaluation count : 2178 in 6 samples of 363 calls.
;;              Execution time mean : 272.796599 µs
;;     Execution time std-deviation : 2.491734 µs
;;    Execution time lower quantile : 269.475882 µs ( 2.5%)
;;    Execution time upper quantile : 275.772893 µs (97.5%)
;;                    Overhead used : 9.914485 ns
;; ---
;; [3 2]
;; :clj
;; Evaluation count : 2262 in 6 samples of 377 calls.
;;              Execution time mean : 272.622480 µs
;;     Execution time std-deviation : 6.976664 µs
;;    Execution time lower quantile : 266.480286 µs ( 2.5%)
;;    Execution time upper quantile : 281.139195 µs (97.5%)
;;                    Overhead used : 9.914485 ns
;; ---
;; [2 4]
;; :clj
;; Evaluation count : 744 in 6 samples of 124 calls.
;;              Execution time mean : 818.873198 µs
;;     Execution time std-deviation : 22.921880 µs
;;    Execution time lower quantile : 803.636685 µs ( 2.5%)
;;    Execution time upper quantile : 857.770223 µs (97.5%)
;;                    Overhead used : 9.914485 ns

;; Found 1 outliers in 6 samples (16.6667 %)
;; 	low-severe	 1 (16.6667 %)
;;  Variance from outliers : 13.8889 % Variance is moderately inflated by outliers
;; ---
;; [4 2]
;; :clj
;; Evaluation count : 702 in 6 samples of 117 calls.
;;              Execution time mean : 855.612900 µs
;;     Execution time std-deviation : 6.675871 µs
;;    Execution time lower quantile : 846.686197 µs ( 2.5%)
;;    Execution time upper quantile : 863.637165 µs (97.5%)
;;                    Overhead used : 9.914485 ns
;; ---
;; [3 4]
;; :clj
;; Evaluation count : 750 in 6 samples of 125 calls.
;;              Execution time mean : 820.253348 µs
;;     Execution time std-deviation : 14.866068 µs
;;    Execution time lower quantile : 809.295496 µs ( 2.5%)
;;    Execution time upper quantile : 839.954299 µs (97.5%)
;;                    Overhead used : 9.914485 ns
;; ---
;; [4 3]
;; :clj
;; Evaluation count : 714 in 6 samples of 119 calls.
;;              Execution time mean : 856.006521 µs
;;     Execution time std-deviation : 9.584943 µs
;;    Execution time lower quantile : 845.812042 µs ( 2.5%)
;;    Execution time upper quantile : 868.638539 µs (97.5%)
;;                    Overhead used : 9.914485 ns
;; ---
;; [1 2]
;; :deep
;; Evaluation count : 1332 in 6 samples of 222 calls.
;;              Execution time mean : 457.038023 µs
;;     Execution time std-deviation : 4.092248 µs
;;    Execution time lower quantile : 452.023311 µs ( 2.5%)
;;    Execution time upper quantile : 460.988014 µs (97.5%)
;;                    Overhead used : 9.914485 ns
;; ---
;; [2 1]
;; :deep
;; Evaluation count : 1326 in 6 samples of 221 calls.
;;              Execution time mean : 470.568112 µs
;;     Execution time std-deviation : 26.063443 µs
;;    Execution time lower quantile : 456.316244 µs ( 2.5%)
;;    Execution time upper quantile : 515.434426 µs (97.5%)
;;                    Overhead used : 9.914485 ns

;; Found 1 outliers in 6 samples (16.6667 %)
;; 	low-severe	 1 (16.6667 %)
;;  Variance from outliers : 14.3086 % Variance is moderately inflated by outliers
;; ---
;; [1 3]
;; :deep
;; Evaluation count : 1320 in 6 samples of 220 calls.
;;              Execution time mean : 460.199146 µs
;;     Execution time std-deviation : 9.230948 µs
;;    Execution time lower quantile : 453.169941 µs ( 2.5%)
;;    Execution time upper quantile : 475.247320 µs (97.5%)
;;                    Overhead used : 9.914485 ns

;; Found 1 outliers in 6 samples (16.6667 %)
;; 	low-severe	 1 (16.6667 %)
;;  Variance from outliers : 13.8889 % Variance is moderately inflated by outliers
;; ---
;; [3 1]
;; :deep
;; Evaluation count : 1326 in 6 samples of 221 calls.
;;              Execution time mean : 464.841305 µs
;;     Execution time std-deviation : 10.891692 µs
;;    Execution time lower quantile : 457.804995 µs ( 2.5%)
;;    Execution time upper quantile : 483.401153 µs (97.5%)
;;                    Overhead used : 9.914485 ns

;; Found 1 outliers in 6 samples (16.6667 %)
;; 	low-severe	 1 (16.6667 %)
;;  Variance from outliers : 13.8889 % Variance is moderately inflated by outliers
;; ---
;; [1 4]
;; :deep
;; Evaluation count : 426 in 6 samples of 71 calls.
;;              Execution time mean : 1.390776 ms
;;     Execution time std-deviation : 95.708291 µs
;;    Execution time lower quantile : 1.326539 ms ( 2.5%)
;;    Execution time upper quantile : 1.546719 ms (97.5%)
;;                    Overhead used : 9.914485 ns

;; Found 1 outliers in 6 samples (16.6667 %)
;; 	low-severe	 1 (16.6667 %)
;;  Variance from outliers : 15.1392 % Variance is moderately inflated by outliers
;; ---
;; [4 1]
;; :deep
;; Evaluation count : 1044 in 6 samples of 174 calls.
;;              Execution time mean : 588.101194 µs
;;     Execution time std-deviation : 6.167238 µs
;;    Execution time lower quantile : 580.577155 µs ( 2.5%)
;;    Execution time upper quantile : 595.124251 µs (97.5%)
;;                    Overhead used : 9.914485 ns
;; ---
;; [2 3]
;; :deep
;; Evaluation count : 1332 in 6 samples of 222 calls.
;;              Execution time mean : 460.737584 µs
;;     Execution time std-deviation : 12.330097 µs
;;    Execution time lower quantile : 449.005968 µs ( 2.5%)
;;    Execution time upper quantile : 480.430364 µs (97.5%)
;;                    Overhead used : 9.914485 ns

;; Found 1 outliers in 6 samples (16.6667 %)
;; 	low-severe	 1 (16.6667 %)
;;  Variance from outliers : 13.8889 % Variance is moderately inflated by outliers
;; ---
;; [3 2]
;; :deep
;; Evaluation count : 1302 in 6 samples of 217 calls.
;;              Execution time mean : 460.100093 µs
;;     Execution time std-deviation : 14.219767 µs
;;    Execution time lower quantile : 450.581051 µs ( 2.5%)
;;    Execution time upper quantile : 479.889208 µs (97.5%)
;;                    Overhead used : 9.914485 ns
;; ---
;; [2 4]
;; :deep
;; Evaluation count : 456 in 6 samples of 76 calls.
;;              Execution time mean : 1.331892 ms
;;     Execution time std-deviation : 11.169730 µs
;;    Execution time lower quantile : 1.318721 ms ( 2.5%)
;;    Execution time upper quantile : 1.346833 ms (97.5%)
;;                    Overhead used : 9.914485 ns

;; Found 1 outliers in 6 samples (16.6667 %)
;; 	low-severe	 1 (16.6667 %)
;;  Variance from outliers : 13.8889 % Variance is moderately inflated by outliers
;; ---
;; [4 2]
;; :deep
;; Evaluation count : 1056 in 6 samples of 176 calls.
;;              Execution time mean : 596.826086 µs
;;     Execution time std-deviation : 23.872597 µs
;;    Execution time lower quantile : 574.151489 µs ( 2.5%)
;;    Execution time upper quantile : 633.844636 µs (97.5%)
;;                    Overhead used : 9.914485 ns

;; Found 1 outliers in 6 samples (16.6667 %)
;; 	low-severe	 1 (16.6667 %)
;;  Variance from outliers : 13.8889 % Variance is moderately inflated by outliers
;; ---
;; [3 4]
;; :deep
;; Evaluation count : 450 in 6 samples of 75 calls.
;;              Execution time mean : 1.385942 ms
;;     Execution time std-deviation : 39.387977 µs
;;    Execution time lower quantile : 1.346710 ms ( 2.5%)
;;    Execution time upper quantile : 1.446104 ms (97.5%)
;;                    Overhead used : 9.914485 ns

;; Found 1 outliers in 6 samples (16.6667 %)
;; 	low-severe	 1 (16.6667 %)
;;  Variance from outliers : 13.8889 % Variance is moderately inflated by outliers
;; ---
;; [4 3]
;; :deep
;; Evaluation count : 996 in 6 samples of 166 calls.
;;              Execution time mean : 586.546811 µs
;;     Execution time std-deviation : 15.558826 µs
;;    Execution time lower quantile : 573.689090 µs ( 2.5%)
;;    Execution time upper quantile : 610.668840 µs (97.5%)
;;                    Overhead used : 9.914485 ns

;; Found 1 outliers in 6 samples (16.6667 %)
;; 	low-severe	 1 (16.6667 %)
;;  Variance from outliers : 13.8889 % Variance is moderately inflated by outliers
;; ---


(def patches {:editscript-a*    #'editscript/patch
              :editscript-quick #'editscript/patch
              :differ           #'differ/patch})

(defn roundtrip-time
  [k [i1 i2]]
  (println [(inc i1) (inc i2)])
  (println k)
  (let [d1       (datas i1)
        d2       (datas i2)
        diff     (diffs k)
        patch    (patches k)
        correct? (= d2 (patch d1 (diff d1 d2)))]
    (if correct?
      (c/quick-bench (patch d1 (diff d1 d2)))
      "Round trip failed!"))
  (println "---"))

(doseq [k  (keys patches)
        ds (combo/permuted-combinations [0 1 2 3] 2)]
  (roundtrip-time k ds))
;;==>
;; [1 2]
;; :editscript-a*
;; Evaluation count : 792 in 6 samples of 132 calls.
;;              Execution time mean : 782.505826 µs
;;     Execution time std-deviation : 27.790884 µs
;;    Execution time lower quantile : 767.194864 µs ( 2.5%)
;;    Execution time upper quantile : 830.085004 µs (97.5%)
;;                    Overhead used : 9.988271 ns

;; Found 1 outliers in 6 samples (16.6667 %)
;; 	low-severe	 1 (16.6667 %)
;;  Variance from outliers : 13.8889 % Variance is moderately inflated by outliers
;; ---
;; [2 1]
;; :editscript-a*
;; Evaluation count : 810 in 6 samples of 135 calls.
;;              Execution time mean : 770.773636 µs
;;     Execution time std-deviation : 26.228973 µs
;;    Execution time lower quantile : 753.641770 µs ( 2.5%)
;;    Execution time upper quantile : 814.878689 µs (97.5%)
;;                    Overhead used : 9.988271 ns

;; Found 1 outliers in 6 samples (16.6667 %)
;; 	low-severe	 1 (16.6667 %)
;;  Variance from outliers : 13.8889 % Variance is moderately inflated by outliers
;; ---
;; [1 3]
;; :editscript-a*
;; Evaluation count : 648 in 6 samples of 108 calls.
;;              Execution time mean : 974.291469 µs
;;     Execution time std-deviation : 64.711039 µs
;;    Execution time lower quantile : 935.178028 µs ( 2.5%)
;;    Execution time upper quantile : 1.086269 ms (97.5%)
;;                    Overhead used : 9.988271 ns

;; Found 1 outliers in 6 samples (16.6667 %)
;; 	low-severe	 1 (16.6667 %)
;;  Variance from outliers : 15.0269 % Variance is moderately inflated by outliers
;; ---
;; [3 1]
;; :editscript-a*
;; Evaluation count : 654 in 6 samples of 109 calls.
;;              Execution time mean : 1.373500 ms
;;     Execution time std-deviation : 984.964126 µs
;;    Execution time lower quantile : 927.320835 µs ( 2.5%)
;;    Execution time upper quantile : 3.081797 ms (97.5%)
;;                    Overhead used : 9.988271 ns

;; Found 1 outliers in 6 samples (16.6667 %)
;; 	low-severe	 1 (16.6667 %)
;;  Variance from outliers : 82.9817 % Variance is severely inflated by outliers
;; ---
;; [1 4]
;; :editscript-a*
;; Evaluation count : 162 in 6 samples of 27 calls.
;;              Execution time mean : 4.167034 ms
;;     Execution time std-deviation : 708.791990 µs
;;    Execution time lower quantile : 3.483550 ms ( 2.5%)
;;    Execution time upper quantile : 5.190779 ms (97.5%)
;;                    Overhead used : 9.988271 ns
;; ---
;; [4 1]
;; :editscript-a*
;; Evaluation count : 168 in 6 samples of 28 calls.
;;              Execution time mean : 3.792227 ms
;;     Execution time std-deviation : 201.068082 µs
;;    Execution time lower quantile : 3.564360 ms ( 2.5%)
;;    Execution time upper quantile : 4.076572 ms (97.5%)
;;                    Overhead used : 9.988271 ns
;; ---
;; [2 3]
;; :editscript-a*
;; Evaluation count : 648 in 6 samples of 108 calls.
;;              Execution time mean : 1.021790 ms
;;     Execution time std-deviation : 94.866031 µs
;;    Execution time lower quantile : 900.245435 µs ( 2.5%)
;;    Execution time upper quantile : 1.108911 ms (97.5%)
;;                    Overhead used : 9.988271 ns
;; ---
;; [3 2]
;; :editscript-a*
;; Evaluation count : 672 in 6 samples of 112 calls.
;;              Execution time mean : 992.308868 µs
;;     Execution time std-deviation : 74.617345 µs
;;    Execution time lower quantile : 915.572438 µs ( 2.5%)
;;    Execution time upper quantile : 1.075321 ms (97.5%)
;;                    Overhead used : 9.988271 ns
;; ---
;; [2 4]
;; :editscript-a*
;; Evaluation count : 168 in 6 samples of 28 calls.
;;              Execution time mean : 3.639602 ms
;;     Execution time std-deviation : 367.064257 µs
;;    Execution time lower quantile : 3.386002 ms ( 2.5%)
;;    Execution time upper quantile : 4.147231 ms (97.5%)
;;                    Overhead used : 9.988271 ns
;; ---
;; [4 2]
;; :editscript-a*
;; Evaluation count : 174 in 6 samples of 29 calls.
;;              Execution time mean : 3.654095 ms
;;     Execution time std-deviation : 115.227379 µs
;;    Execution time lower quantile : 3.543291 ms ( 2.5%)
;;    Execution time upper quantile : 3.766448 ms (97.5%)
;;                    Overhead used : 9.988271 ns
;; ---
;; [3 4]
;; :editscript-a*
;; Evaluation count : 180 in 6 samples of 30 calls.
;;              Execution time mean : 3.547096 ms
;;     Execution time std-deviation : 94.554180 µs
;;    Execution time lower quantile : 3.444556 ms ( 2.5%)
;;    Execution time upper quantile : 3.648524 ms (97.5%)
;;                    Overhead used : 9.988271 ns
;; ---
;; [4 3]
;; :editscript-a*
;; Evaluation count : 180 in 6 samples of 30 calls.
;;              Execution time mean : 3.561021 ms
;;     Execution time std-deviation : 95.419541 µs
;;    Execution time lower quantile : 3.468076 ms ( 2.5%)
;;    Execution time upper quantile : 3.670192 ms (97.5%)
;;                    Overhead used : 9.988271 ns
;; ---
;; [1 2]
;; :editscript-quick
;; Evaluation count : 12324 in 6 samples of 2054 calls.
;;              Execution time mean : 49.535497 µs
;;     Execution time std-deviation : 1.872670 µs
;;    Execution time lower quantile : 48.124697 µs ( 2.5%)
;;    Execution time upper quantile : 52.711871 µs (97.5%)
;;                    Overhead used : 9.988271 ns

;; Found 1 outliers in 6 samples (16.6667 %)
;; 	low-severe	 1 (16.6667 %)
;;  Variance from outliers : 13.8889 % Variance is moderately inflated by outliers
;; ---
;; [2 1]
;; :editscript-quick
;; Evaluation count : 12264 in 6 samples of 2044 calls.
;;              Execution time mean : 48.943583 µs
;;     Execution time std-deviation : 482.652975 ns
;;    Execution time lower quantile : 48.351955 µs ( 2.5%)
;;    Execution time upper quantile : 49.397873 µs (97.5%)
;;                    Overhead used : 9.988271 ns
;; ---
;; [1 3]
;; :editscript-quick
;; Evaluation count : 10956 in 6 samples of 1826 calls.
;;              Execution time mean : 55.253968 µs
;;     Execution time std-deviation : 814.446115 ns
;;    Execution time lower quantile : 54.631312 µs ( 2.5%)
;;    Execution time upper quantile : 56.571675 µs (97.5%)
;;                    Overhead used : 9.988271 ns

;; Found 1 outliers in 6 samples (16.6667 %)
;; 	low-severe	 1 (16.6667 %)
;;  Variance from outliers : 13.8889 % Variance is moderately inflated by outliers
;; ---
;; [3 1]
;; :editscript-quick
;; Evaluation count : 11076 in 6 samples of 1846 calls.
;;              Execution time mean : 55.576152 µs
;;     Execution time std-deviation : 1.292946 µs
;;    Execution time lower quantile : 54.721433 µs ( 2.5%)
;;    Execution time upper quantile : 57.786691 µs (97.5%)
;;                    Overhead used : 9.988271 ns

;; Found 1 outliers in 6 samples (16.6667 %)
;; 	low-severe	 1 (16.6667 %)
;;  Variance from outliers : 13.8889 % Variance is moderately inflated by outliers
;; ---
;; [1 4]
;; :editscript-quick
;; Evaluation count : 4350 in 6 samples of 725 calls.
;;              Execution time mean : 139.561261 µs
;;     Execution time std-deviation : 2.795468 µs
;;    Execution time lower quantile : 137.387817 µs ( 2.5%)
;;    Execution time upper quantile : 144.186031 µs (97.5%)
;;                    Overhead used : 9.988271 ns

;; Found 1 outliers in 6 samples (16.6667 %)
;; 	low-severe	 1 (16.6667 %)
;;  Variance from outliers : 13.8889 % Variance is moderately inflated by outliers
;; ---
;; [4 1]
;; :editscript-quick
;; Evaluation count : 3282 in 6 samples of 547 calls.
;;              Execution time mean : 183.343872 µs
;;     Execution time std-deviation : 1.696776 µs
;;    Execution time lower quantile : 181.024102 µs ( 2.5%)
;;    Execution time upper quantile : 184.941905 µs (97.5%)
;;                    Overhead used : 9.988271 ns
;; ---
;; [2 3]
;; :editscript-quick
;; Evaluation count : 11418 in 6 samples of 1903 calls.
;;              Execution time mean : 56.795265 µs
;;     Execution time std-deviation : 7.242604 µs
;;    Execution time lower quantile : 53.319841 µs ( 2.5%)
;;    Execution time upper quantile : 69.372004 µs (97.5%)
;;                    Overhead used : 9.988271 ns

;; Found 1 outliers in 6 samples (16.6667 %)
;; 	low-severe	 1 (16.6667 %)
;;  Variance from outliers : 31.5540 % Variance is moderately inflated by outliers
;; ---
;; [3 2]
;; :editscript-quick
;; Evaluation count : 10482 in 6 samples of 1747 calls.
;;              Execution time mean : 74.568509 µs
;;     Execution time std-deviation : 31.899902 µs
;;    Execution time lower quantile : 53.618510 µs ( 2.5%)
;;    Execution time upper quantile : 124.741128 µs (97.5%)
;;                    Overhead used : 9.988271 ns

;; Found 1 outliers in 6 samples (16.6667 %)
;; 	low-severe	 1 (16.6667 %)
;;  Variance from outliers : 82.3451 % Variance is severely inflated by outliers
;; ---
;; [2 4]
;; :editscript-quick
;; Evaluation count : 4296 in 6 samples of 716 calls.
;;              Execution time mean : 143.639635 µs
;;     Execution time std-deviation : 5.969411 µs
;;    Execution time lower quantile : 139.534599 µs ( 2.5%)
;;    Execution time upper quantile : 153.403762 µs (97.5%)
;;                    Overhead used : 9.988271 ns

;; Found 1 outliers in 6 samples (16.6667 %)
;; 	low-severe	 1 (16.6667 %)
;;  Variance from outliers : 13.8889 % Variance is moderately inflated by outliers
;; ---
;; [4 2]
;; :editscript-quick
;; Evaluation count : 3066 in 6 samples of 511 calls.
;;              Execution time mean : 209.415281 µs
;;     Execution time std-deviation : 29.293659 µs
;;    Execution time lower quantile : 183.456509 µs ( 2.5%)
;;    Execution time upper quantile : 245.505596 µs (97.5%)
;;                    Overhead used : 9.988271 ns
;; ---
;; [3 4]
;; :editscript-quick
;; Evaluation count : 4326 in 6 samples of 721 calls.
;;              Execution time mean : 145.617676 µs
;;     Execution time std-deviation : 8.661694 µs
;;    Execution time lower quantile : 139.752408 µs ( 2.5%)
;;    Execution time upper quantile : 157.127105 µs (97.5%)
;;                    Overhead used : 9.988271 ns
;; ---
;; [4 3]
;; :editscript-quick
;; Evaluation count : 3186 in 6 samples of 531 calls.
;;              Execution time mean : 199.678507 µs
;;     Execution time std-deviation : 19.054265 µs
;;    Execution time lower quantile : 182.325932 µs ( 2.5%)
;;    Execution time upper quantile : 222.002183 µs (97.5%)
;;                    Overhead used : 9.988271 ns
;; ---
;; [1 2]
;; :differ
;; Evaluation count : 4716 in 6 samples of 786 calls.
;;              Execution time mean : 138.948845 µs
;;     Execution time std-deviation : 5.668986 µs
;;    Execution time lower quantile : 134.791452 µs ( 2.5%)
;;    Execution time upper quantile : 148.412578 µs (97.5%)
;;                    Overhead used : 9.988271 ns

;; Found 1 outliers in 6 samples (16.6667 %)
;; 	low-severe	 1 (16.6667 %)
;;  Variance from outliers : 13.8889 % Variance is moderately inflated by outliers
;; ---
;; [2 1]
;; :differ
;; Evaluation count : 4680 in 6 samples of 780 calls.
;;              Execution time mean : 131.097384 µs
;;     Execution time std-deviation : 6.116087 µs
;;    Execution time lower quantile : 126.758644 µs ( 2.5%)
;;    Execution time upper quantile : 141.460937 µs (97.5%)
;;                    Overhead used : 9.988271 ns

;; Found 1 outliers in 6 samples (16.6667 %)
;; 	low-severe	 1 (16.6667 %)
;;  Variance from outliers : 13.8889 % Variance is moderately inflated by outliers
;; ---
;; [1 3]
;; :differ
;; Evaluation count : 3576 in 6 samples of 596 calls.
;;              Execution time mean : 132.335986 µs
;;     Execution time std-deviation : 3.887387 µs
;;    Execution time lower quantile : 127.553804 µs ( 2.5%)
;;    Execution time upper quantile : 137.207439 µs (97.5%)
;;                    Overhead used : 9.988271 ns
;; ---
;; [3 1]
;; :differ
;; Evaluation count : 4770 in 6 samples of 795 calls.
;;              Execution time mean : 130.150859 µs
;;     Execution time std-deviation : 6.866008 µs
;;    Execution time lower quantile : 126.697326 µs ( 2.5%)
;;    Execution time upper quantile : 142.033095 µs (97.5%)
;;                    Overhead used : 9.988271 ns

;; Found 1 outliers in 6 samples (16.6667 %)
;; 	low-severe	 1 (16.6667 %)
;;  Variance from outliers : 14.0674 % Variance is moderately inflated by outliers
;; ---
;; [1 4]
;; :differ
;; Evaluation count : 11334 in 6 samples of 1889 calls.
;;              Execution time mean : 54.335042 µs
;;     Execution time std-deviation : 2.226279 µs
;;    Execution time lower quantile : 52.520999 µs ( 2.5%)
;;    Execution time upper quantile : 56.929882 µs (97.5%)
;;                    Overhead used : 9.988271 ns
;; ---
;; [4 1]
;; :differ
;; Evaluation count : 11772 in 6 samples of 1962 calls.
;;              Execution time mean : 52.204266 µs
;;     Execution time std-deviation : 1.398859 µs
;;    Execution time lower quantile : 51.096505 µs ( 2.5%)
;;    Execution time upper quantile : 54.224774 µs (97.5%)
;;                    Overhead used : 9.988271 ns
;; ---
;; [2 3]
;; :differ
;; Evaluation count : 4734 in 6 samples of 789 calls.
;;              Execution time mean : 129.885246 µs
;;     Execution time std-deviation : 6.790292 µs
;;    Execution time lower quantile : 126.569757 µs ( 2.5%)
;;    Execution time upper quantile : 141.456785 µs (97.5%)
;;                    Overhead used : 9.988271 ns

;; Found 1 outliers in 6 samples (16.6667 %)
;; 	low-severe	 1 (16.6667 %)
;;  Variance from outliers : 14.0199 % Variance is moderately inflated by outliers
;; ---
;; [3 2]
;; :differ
;; Evaluation count : 4680 in 6 samples of 780 calls.
;;              Execution time mean : 132.598568 µs
;;     Execution time std-deviation : 4.470462 µs
;;    Execution time lower quantile : 127.526269 µs ( 2.5%)
;;    Execution time upper quantile : 137.464902 µs (97.5%)
;;                    Overhead used : 9.988271 ns
;; ---
;; [2 4]
;; :differ
;; Evaluation count : 11244 in 6 samples of 1874 calls.
;;              Execution time mean : 55.653493 µs
;;     Execution time std-deviation : 2.852291 µs
;;    Execution time lower quantile : 52.920391 µs ( 2.5%)
;;    Execution time upper quantile : 59.919993 µs (97.5%)
;;                    Overhead used : 9.988271 ns
;; ---
;; [4 2]
;; :differ
;; Evaluation count : 11466 in 6 samples of 1911 calls.
;;              Execution time mean : 52.126760 µs
;;     Execution time std-deviation : 855.932882 ns
;;    Execution time lower quantile : 51.259826 µs ( 2.5%)
;;    Execution time upper quantile : 53.537360 µs (97.5%)
;;                    Overhead used : 9.988271 ns

;; Found 2 outliers in 6 samples (33.3333 %)
;; 	low-severe	 1 (16.6667 %)
;; 	low-mild	 1 (16.6667 %)
;;  Variance from outliers : 13.8889 % Variance is moderately inflated by outliers
;; ---
;; [3 4]
;; :differ
;; Evaluation count : 11418 in 6 samples of 1903 calls.
;;              Execution time mean : 54.286322 µs
;;     Execution time std-deviation : 2.230475 µs
;;    Execution time lower quantile : 52.849030 µs ( 2.5%)
;;    Execution time upper quantile : 57.663538 µs (97.5%)
;;                    Overhead used : 9.988271 ns
;; ---
;; [4 3]
;; :differ
;; Evaluation count : 11628 in 6 samples of 1938 calls.
;;              Execution time mean : 53.632614 µs
;;     Execution time std-deviation : 2.858306 µs
;;    Execution time lower quantile : 51.258380 µs ( 2.5%)
;;    Execution time upper quantile : 57.321995 µs (97.5%)
;;                    Overhead used : 9.988271 ns
;; ---

;; property based testing

(def compound (fn [inner-gen]
                (gen/one-of [(gen/list inner-gen)
                             (gen/vector inner-gen)
                             (gen/set inner-gen)
                             (gen/map inner-gen inner-gen)])))

(def compound-wo-set (fn [inner-gen]
                       (gen/one-of [(gen/list inner-gen)
                                    (gen/vector inner-gen)
                                    (gen/map inner-gen inner-gen)])))

(def compound-vec-map-only (fn [inner-gen]
                             (gen/one-of [(gen/vector inner-gen)
                                          (gen/map inner-gen inner-gen)])))

(def scalars (gen/frequency [[19 (gen/one-of [gen/int
                                              gen/string])]
                             [1 (gen/return nil)]]))

(test/defspec differ-roundtrip-generative-test
  2000
  (prop/for-all [a (gen/recursive-gen compound scalars)
                 b (gen/recursive-gen compound scalars)]
                (let [d (differ/diff a b)]
                  (= b (differ/patch a d)))))
;;==> fail

(test/defspec differ-roundtrip-wo-set-generative-test
  2000
  (prop/for-all [a (gen/recursive-gen compound-wo-set scalars)
                 b (gen/recursive-gen compound-wo-set scalars)]
                (let [d (differ/diff a b)]
                  (= b (differ/patch a d)))))
;;==> fail

(test/defspec differ-roundtrip-vec-map-only-generative-test
  2000
  (prop/for-all [a (gen/recursive-gen compound-vec-map-only scalars)
                 b (gen/recursive-gen compound-vec-map-only scalars)]
                (let [d (differ/diff a b)]
                  (= b (differ/patch a d)))))
;;==> fail

(test/defspec editscript-quick-roundtrip-generative-test
  2000
  (prop/for-all [a (gen/recursive-gen compound scalars)
                 b (gen/recursive-gen compound scalars)]
                (let [d (editscript/diff a b {:algo :quick})]
                  (= b (editscript/patch a d)))))
;;==> success

(test/defspec editscript-a*-roundtrip-generative-test
  2000
  (prop/for-all [a (gen/recursive-gen compound scalars)
                 b (gen/recursive-gen compound scalars)]
                (let [d (editscript/diff a b)]
                  (= b (editscript/patch a d)))))
;;==> success

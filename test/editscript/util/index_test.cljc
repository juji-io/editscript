(ns editscript.util.index-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.clojure-test :as test
             #?@(:cljs [:refer-macros [defspec] :include-macros true])]
            [clojure.test.check.properties :as prop
             #?@(:cljs [:include-macros true])]
            [editscript.edit :as e]
            [editscript.util.index :as i]))

(defn- child-specs
  [value]
  (case (e/get-type value)
    :map (vec value)
    :set (mapv #(vector % %) value)
    (:vec :lst) (mapv vector (range) value)
    []))

(defn- sibling-chain
  [node limit]
  (loop [child (i/get-first node)
         left  (inc limit)
         nodes []]
    (cond
      (nil? child) {:nodes nodes :terminated? true}
      (zero? left) {:nodes nodes :terminated? false}
      :else        (recur (i/get-next child) (dec left) (conj nodes child)))))

(defn- valid-index?
  [data]
  (let [root   (i/index data)
        orders (volatile! [])]
    (letfn [(valid-node? [node parent key value path]
              (let [type       (e/get-type value)
                    specs      (child-specs value)
                    children   (i/get-children node)
                    sequence?  (contains? #{:vec :lst} type)
                    collection? (contains? #{:map :set :vec :lst} type)
                    {:keys [nodes terminated?]}
                    (sibling-chain node (count specs))
                    child-nodes (mapv (fn [[child-key _]]
                                        (get children child-key))
                                      specs)]
                (vswap! orders conj (i/get-order node))
                (and (= value (i/get-value node))
                     (= key (i/get-key node))
                     (= path (i/get-path node))
                     (identical? parent (i/get-parent node))
                     (= collection? (some? children))
                     (or (not collection?)
                         (= sequence? (vector? children)))
                     (= (count specs) (count children))
                     terminated?
                     (= (count specs) (count nodes))
                     (= (mapv i/get-key nodes) (mapv first specs))
                     (if (seq nodes)
                       (identical? (last nodes) (i/get-last node))
                       (nil? (i/get-last node)))
                     (every? some? child-nodes)
                     (every? true?
                             (map identical? nodes child-nodes))
                     (= (i/get-size node)
                        (inc (reduce + (map i/get-size child-nodes))))
                     (every? true?
                             (map (fn [[child-key child-value] child]
                                    (valid-node? child node child-key child-value
                                                 (conj path child-key)))
                                  specs
                                  child-nodes)))))]
      (let [valid? (valid-node? root nil nil data [])]
        (and valid?
             (= (count @orders) (count (set @orders))))))))

(deftest compact-index-shape-test
  (let [data       {:vector [0 {:leaf 1}]
                    :list   '(2 3)
                    :map    {:nested 4}
                    :set    #{5 6}}
        root       (i/index data)
        root-kids  (i/get-children root)
        vector-node (get root-kids :vector)
        list-node   (get root-kids :list)
        leaf-node   (-> vector-node i/get-children (get 1)
                        i/get-children (get :leaf))]
    (testing "sequence children use dense vectors"
      (is (vector? (i/get-children vector-node)))
      (is (vector? (i/get-children list-node))))
    (testing "associative children retain keyed lookup"
      (is (map? root-kids))
      (is (map? (i/get-children (get root-kids :map))))
      (is (map? (i/get-children (get root-kids :set)))))
    (testing "paths are reconstructed from parent/key links"
      (is (= [:vector 1 :leaf] (i/get-path leaf-node)))
      (is (= [] (i/get-path root)))
      (is (nil? (i/get-parent root))))
    (is (valid-index? data))))

(deftest lazy-child-realization-test
  (let [root  (i/index [[{:deep [1 2]}]
                        [{:untouched [3 4]}]])
        empty (i/index [])]
    (testing "indexing computes metadata without constructing children"
      (is (= 11 (i/get-size root)))
      (is (= [] (i/get-path root)))
      (is (not (i/children-realized? root)))
      (is (not (i/children-realized? empty))))
    (testing "access realizes one immediate level only"
      (let [first-branch  (i/get-first root)
            second-branch (i/get-next first-branch)]
        (is (i/children-realized? root))
        (is (not (i/children-realized? first-branch)))
        (is (not (i/children-realized? second-branch)))
        (is (= [1] (i/get-path second-branch)))
        (is (= 5 (i/get-size second-branch)))
        (let [map-node (i/get-first first-branch)]
          (is (i/children-realized? first-branch))
          (is (not (i/children-realized? map-node)))
          (is (not (i/children-realized? second-branch)))
          (let [deep-node (i/get-first map-node)]
            (is (i/children-realized? map-node))
            (is (not (i/children-realized? deep-node)))
            (is (= [0 0 :deep] (i/get-path deep-node)))))))
    (testing "even an empty collection records realization"
      (is (nil? (i/get-first empty)))
      (is (i/children-realized? empty)))))

(deftest traversal-order-compatibility-test
  (let [root     (i/index [[:a :b]
                           {:x [:c] :y :d}
                           '(:e :f)])
        expected [[[] 18 11]
                  [[0] 2 3]
                  [[0 0] 0 1]
                  [[0 1] 1 1]
                  [[1] 9 4]
                  [[1 :x] 6 2]
                  [[1 :x 0] 5 1]
                  [[1 :y] 8 1]
                  [[2] 15 3]
                  [[2 0] 13 1]
                  [[2 1] 14 1]]]
    (testing "lazy nodes retain the eager index's size-weighted order"
      (is (= expected
             (mapv (fn [[path _ _]]
                     (let [node (reduce (fn [parent child-key]
                                          (get (i/get-children parent) child-key))
                                        root
                                        path)]
                       [path (i/get-order node) (i/get-size node)]))
                   expected))))))

(def scalars
  (gen/one-of [gen/int gen/string gen/boolean (gen/return nil)]))

(defn- compound
  [inner]
  (gen/one-of [(gen/list inner)
               (gen/vector inner)
               (gen/set inner)
               (gen/map inner inner)]))

(def indexed-data
  (gen/recursive-gen compound scalars))

(test/defspec index-invariants-generative-test
  #?(:cljs 100 :cljr 100 :default 500)
  (prop/for-all [data indexed-data]
                (valid-index? data)))

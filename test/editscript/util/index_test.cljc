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

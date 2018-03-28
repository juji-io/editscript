(ns editscript.core
  (:require [clojure.set :as set])
  (:import [clojure.lang Seqable PersistentVector IPersistentCollection]))

(set! *warn-on-reflection* true)

(defprotocol IEdit
  (add-data [this path value])
  (delete-data [this path])
  (replace-data [this path value]))

(defprotocol IEditScript
  (edit-distance [this] "Report the edit instance")
  (get-edits [this] "Report the edits as a vector")
  (get-adds-num [this] "Report the number of additions")
  (get-dels-num [this] "Report the number of deletions")
  (get-reps-num [this] "Report the number of replacements"))

(deftype EditScript [original
                     ^:volatile-mutable ^PersistentVector edits
                     ^:volatile-mutable ^long adds-num
                     ^:volatile-mutable ^long dels-num
                     ^:volatile-mutable ^long reps-num]
  :load-ns true

  IEdit
  (add-data [this path value]
    (locking this
      (set! adds-num (inc adds-num))
      (set! edits (conj edits [path ::+ value]))))
  (delete-data [this path]
    (locking this
      (set! dels-num (inc dels-num))
      (set! edits (conj edits [path ::-]))))
  (replace-data [this path value]
    (locking this
      (set! reps-num (inc reps-num))
      (set! edits (conj edits [path ::r value]))))

  IEditScript
  (get-edits [this] edits)
  (get-adds-num [this] adds-num)
  (get-dels-num [this] dels-num)
  (get-reps-num [this] reps-num)
  (edit-distance [this] (+ adds-num dels-num reps-num))

  Seqable
  (seq [this]
    (.seq edits))

  IPersistentCollection
  (count [this]
    (.count edits))
  (cons [this o]
    (.cons edits o))
  (empty [this]
    (EditScript. original [] 0 0 0))
  (equiv [this o]
    (.equiv edits o)))

(defmethod print-method EditScript [x ^java.io.Writer writer]
  (print-method (get-edits x) writer))

(declare diff*)

(defn- diff-map [script path a b]
  (reduce-kv
   (fn [_ ka va]
     (let [path' (conj path ka)]
       (if (contains? b ka)
        (diff* script path' va (get b ka))
        (diff* script path' va ::nil))))
   nil
   a)
  (reduce-kv
   (fn [_ kb vb]
     (when-not (contains? a kb)
       (diff* script (conj path kb) ::nil vb)))
   nil
   b))

(defn- vec-edits*
  "Based on 'Wu, S. et al., 1990, An O(NP) Sequence Comparison Algorithm,
  Information Processing Letters, 35:6, p317-23.'"
  [a b n m]
  (let [delta (- n m)
        snake (fn [k x]
                (loop [x x y (- x k)]
                  (let [ax (get a x) by (get b y)]
                    (if (and (< x n)
                             (< y m)
                             (= (type ax) (type by))
                             (= ax by))
                      (recur (inc x) (inc y))
                      x))))
        fp-fn (fn [fp k]
                (let [[dk-1 vk-1] (get fp (dec k) [-1 []])
                      dk-1        (inc dk-1)
                      [dk+1 vk+1] (get fp (inc k) [-1 []])
                      x           (max dk-1 dk+1)
                      sk          (snake k x)
                      ops         (let [es (if (> dk-1 dk+1)
                                             (conj vk-1 :-)
                                             (conj vk+1 :+))]
                                    (if (> sk x)
                                      (conj es (- sk x))
                                      es))]
                  (assoc! fp k [sk ops])))
        fp    (loop [p 0 fp (transient {})]
                (let [fp (loop [k (* -1 p) fp fp]
                           (if (< k delta)
                             (recur (inc k) (fp-fn fp k))
                             fp))
                      fp (loop [k (+ delta p) fp fp]
                           (if (< delta k)
                             (recur (dec k) (fp-fn fp k))
                             fp))
                      fp (fp-fn fp delta)]
                  (if-not (= n (first (get fp delta)))
                    (recur (inc p) fp)
                    (persistent! fp))))]
    (-> fp (get delta) second rest)))

(defn- swap-ops [edits] (vec (map (fn [op] (case op :+ :- :- :+ op)) edits)))

(defn min+plus->replace
  "Turn isolated consecutive `:-` `:+` into a `:r`,
  do not convert if there's `:-` in front, as it is ambiguous"
  [v]
  {:pre [(vector? v)]}
  (let [n (count v)]
    (loop [r (transient []) i -1 j 0 k 1]
      (let [ei (get v i) ej (get v j) ek (get v k)]
       (cond
         (and (= ej :-)
              (= ek :+)
              (not= ei :-)) (recur (conj! r :r) (+ i 2) (+ j 2) (+ k 2))
         (>= j n)           (persistent! r)
         :else              (recur (conj! r ej) (inc i) (inc j) (inc k)))))))

(defn vec-edits [a b]
  (let [n (count a)
        m (count b)
        v (if (< n m)
            (swap-ops (vec-edits* b a m n))
            (vec-edits* a b n m))]
    (-> v vec min+plus->replace)))

(defn- diff-vec [script path a b]
  (reduce
   (fn [{:keys [ia ia' ib] :as m} op]
     (case op
       :- (do (diff* script (conj path ia') (get a ia) ::nil)
              (assoc! m :ia (inc ia)))
       :+ (do (diff* script (conj path ia') ::nil (get b ib))
              (assoc! m :ia' (inc ia') :ib (inc ib)))
       :r (do (diff* script (conj path ia') (get a ia) (get b ib))
              (assoc! m :ia (inc ia) :ia' (inc ia') :ib (inc ib)))
       (assoc! m :ia (+ ia op) :ia' (+ ia' op) :ib (+ ib op))))
   (transient {:ia 0 :ia' 0 :ib 0})
   (vec-edits a b)))

(defn- diff-set [script path a b]
  (doseq [va (set/difference a b)]
    (diff* script (conj path va) va ::nil))
  (doseq [vb (set/difference b a)]
    (diff* script (conj path vb) ::nil vb)))

(defn- diff-lst [script path a b]
  (diff-vec script path (vec a) (vec b)))

(defn get-type [v]
  (cond
    (map? v)    :map
    (vector? v) :vec
    (set? v)    :set
    (list? v)   :lst
    (= ::nil v) :nil
    (nil? v)    :val
    :else       :val))

(defn diff* [script path a b]
  (when-not (identical? a b)
    (case (get-type a)
      :nil (add-data script path b)
      :map (case (get-type b)
             :nil (delete-data script path)
             :map (diff-map script path a b)
             (replace-data script path b))
      :vec (case (get-type b)
             :nil (delete-data script path)
             :vec (diff-vec script path a b)
             (replace-data script path b))
      :set (case (get-type b)
             :nil (delete-data script path)
             :set (diff-set script path a b)
             (replace-data script path b))
      :lst (case (get-type b)
             :nil (delete-data script path)
             :lst (diff-lst script path a b)
             (replace-data script path b))
      :val (case (get-type b)
             :nil (delete-data script path)
             (when-not (= a b)
               (replace-data script path b))))))

(defn diff
  "Create an EditScript that represents the difference between `b` and `a`"
  [a b]
  (let [path   ^::path []
        script (->EditScript a path 0 0 0)]
    (diff* script path a b)
    script))

(defn- vget [x p]
  (case (get-type x)
    (:map :vec :set) (get x p)
    :lst             (nth x p)))

(defn- vdelete [x p]
  (case (get-type x)
    ;;NB, there is a special case where dissoc has no effect:
    ;;if p is ##NaN, then p cannot be found in x, for (= ##NaN ##NaN) is false!
    :map (dissoc x p)
    :vec (vec (concat (subvec x 0 p) (subvec x (inc p))))
    :set (set/difference x #{p})
    :lst (->> (split-at p x)
              (#(concat (first %) (next (last %))))
              (apply list))))

(defn- vadd [x p v]
  (case (get-type x)
    :map (assoc x p v)
    :vec (vec (concat (conj (subvec x 0 p) v) (subvec x p)))
    :set (conj x v)
    :lst (->> (split-at p x)
              (#(concat (first %) (conj (last %) v)))
              (apply list))))

(defn- vreplace [x p v]
  (case (get-type x)
    :map (assoc x p v)
    :vec (vec (concat (conj (subvec x 0 p) v) (subvec x (inc p))))
    :set (-> x (set/difference #{p}) (conj v))
    :lst (->> (split-at p x)
              (#(concat (first %) (conj (rest (last %)) v)))
              (apply list))))

(defn- valter [x p o v]
  (case o
    ::- (vdelete x p)
    ::+ (vadd x p v)
    ::r (vreplace x p v)))

(defn patch* [old [path op value]]
  (letfn [(up [x p o v]
            (let [[f & r] p]
              (if r
                (valter x f ::r (up (vget x f) r o v))
                (if (seq p)
                  (valter x f o v)
                  v))))]
    (up old path op value)))

(defn patch
  "Apply the editscript `script` on `a` to produce `b`"
  [a script]
  (reduce
   #(patch* %1 %2)
   a
   (get-edits script)))

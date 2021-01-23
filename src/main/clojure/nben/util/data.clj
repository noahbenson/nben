;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; data.clj, part of nben, a mathematics library for the JVM.
;; This namespace adds types for managing lazy maps and nested collections.
;; 
;; Copyright (C) 2016 Noah C. Benson
;;
;; This file is part of the nben clojure library.
;;
;; The nben clojure library is free software: you can redistribute it and/or modify it under the 
;; terms of the GNU General Public License as published by the Free Software Foundation, either 
;; version 3 of the License, or (at your option) any later version.
;;
;; The nben clojure library is distributed in the hope that it will be useful, but WITHOUT ANY 
;; WARRANTY; without even the implied warranty of  MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License along with the nben clojure
;; library.  If not, see <http:;;www.gnu.org/licenses/>.
;;

(ns nben.util.data
  (:use [nben.util error])
  (:use [nben.util typedef])
  (:use [nben.util misc])
  (:use [nben.util structured])
  (:use [nben.util set])
  (:use [nben.util iterator])
  (:require [clojure.string :refer [join] :rename {join string-join}]))

;; #PFreezable ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmultipro PFreezable
  "
  The PFreezable protocol is implemented by objects that can convert from a mutable (e.g. atom/ref)
  to immutable version. Freezing consists of removing refs and fully evaluating elements in order to
  obtain a fully-evaluated data structure.
  "
  #(cond (derefable? %) :deref, (transient? %) :transient,
         (map? %) :map, (set? %) :set, (vector? %) :vec,
         (seq? %) :seq, :else :obj)
  {:deref     {:freeze #(freeze (deref %))}
   :transient {:freeze #(freeze (persistent! %))}
   :map       {:freeze #(loop [m %, s (seq %)]
                          (if s
                            (let [[k v] (first s), kk (freeze k), vv (freeze v)]
                              (if (and (identical? k kk) (identical? v vv))
                                (recur m (next s))
                                (recur (assoc m (freeze k) (freeze v)) (next s))))
                            m))}
   :set       {:freeze #(loop [q %, s (seq %)]
                          (if s
                            (let [f (first s), ff (freeze f)]
                              (recur (if (identical? f ff) q (conj (disj q f) ff)) (next s)))
                            q))}
   :vec       {:freeze #(loop [q %, s (seq %), ii 0]
                          (if s
                            (let [f (first s), ff (freeze f)]
                              (recur (if (identical? f ff) q (assoc q ii ff)) (next s) (inc ii)))
                            q))}
   :seq       {:freeze #(vec (map freeze (seq %)))}
   :obj       {:freeze identity}}
  (freeze [_] "
  (freeze x) yields a version of x that is persistent and fully reduced in the sense of having all
    mutables such as refs and atoms removed.
  "))
(defn frozen?
  "
  (frozen? x) yields true if (identical? x (freeze x)) and false otherwise.
  "
  [x] (identical? x (freeze x)))

;; #LazyMap ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defprotocol PLazyMap
  "
  PLazyMap is a protocol that is extended by the lazy-map type.
  "
  (get-delay [self k]
    "
  (get-delay m k) yields the delay object associated with the given map m if k is a lazy key in the
    lazy map m and yields nil otherwise.
  ")
  (lazy-map? [self]
    "
  (lazy-map? m) yields true if m is a lazy map and false otherwise.
  "))
(extend-protocol PLazyMap
  nil
  (get-delay [_ k] nil)
  (lazy-map? [_] false)
  Object
  (get-delay [_ k] nil)
  (lazy-map? [_] false))
(defn lazy-val?
  "
  (lazy-val? m k) yields true if k is a lazy value in the given map m. If k is not in m, yields nil.
  "
  [m k]
  (cond (nil? m)              nil ;; empty map contains no values
        (not (map? m))        (arg-err "lazy-val? requested of non-map")
        (not (contains? m k)) nil
        (lazy-map? m)         (delay? (get-delay m k))
        :else                 nil))
(defn normal-val?
  "
  (normal-val? m k) yields true if k is a non-lazy value in the given map m. Note that if m is not
    a lazy-map, then this will always yield true for any k in m. If m does not contain the key k,
    this will also yield nil. An exception is thrown for non-maps.
  "
  [m k]
  (cond (nil? m)              nil
        (not (map? m))        (arg-err "normal-val? requested of non-map")
        (not (contains? m k)) nil
        (lazy-map? m)         (not (delay? (get-delay m k)))
        :else                 nil))
(defn realized-val?
  "
  (realized-val? m k) yields true if k is a lazy value in m that has been cached already or is a
    value that is normal; it yields false if k is not in m or if k is not realized.
  "
  [m k]
  (or (normal-val? m k)
      (and (lazy-val? m k) (realized? (get-delay m k)))))
(defn unrealized-val?
  "
  (unrealized-val? m k) yields true if k is a lazy value in m that has not been cached yet; it
    yields false if k is not in m or if k is a normal or realized value.
  "
  [m k]
  (and (lazy-val? m k) (not (realized? (get-delay m k)))))
  
(defn- LazyMap-hashCode [m]
  (-> (reduce #(assoc! %1 (key %2) (deref (val %2)))
              (transient m)
              (filter (comp delay? val) (seq m)))
      (persistent!)
      (.hashCode)))
(def-map-type
  ^{:private true
    :doc "
  LazyMap is a class that mimics a persistent map but automatically dereferences delay objects.
  LazyMaps should never be implemented by calling the LazyMap constructor directly; instead use the
  lazy-map function or add delay values to the empty lazy-map (obtained by calling (lazy-map)).
  Note that if you request the hashCode of a lazy-map, it must dereference all of its values in
  order to properly mimic a PersistentHashMap. For this reason, it's suggested that you not use 
  lazy maps in sets or as keys in maps.
  "}
  LazyMap [m hash-code]
  (assoc       [_ k v]  (let [m (assoc m k v)]      (LazyMap. m (delay (LazyMap-hashCode m)))))
  (dissoc      [_ k]    (let [m (dissoc m k)]       (LazyMap. m (delay (LazyMap-hashCode m)))))
  (with-meta   [_ meta] (let [m (with-meta m meta)] (LazyMap. m (delay (LazyMap-hashCode m)))))
  (keys        [_]      (keys m))
  (meta        [_]      (meta m))
  (containsKey [_ k]    (contains? m k))
  (get         [_ k df] (let [v (get m k df)]
                          (cond (identical? v df) df
                                (delay? v) @v
                                :else v)))
  (entryAt     [_ k]    (when-let [e (find m k)]
                          (if (delay? (val e)) (clojure.lang.MapEntry. k @(val e)) e)))
  PLazyMap
  (lazy-map? [_]   true)
  (get-delay [_ k] (let [v (get m k)] (when (delay? v) v)))
  Object
  (hashCode [_] @hash-code))
(def empty-lazy-map
  "empty-lazy-map is an empty LazyMap object; see also (doc lazy-map)."
  (LazyMap. {} (delay (LazyMap-hashCode {}))))
(defn lazy-map
  "
  (lazy-map m) yields a lazy-map version of the map m, in which the values of m that are delays are
    automatically dereferenced when requested. Lazy maps behave like persistent maps in virtually
    every way with the above exception. If a delay value is associated into a lazy-map, the new map
    will dereference this delay on request as well.
  (lazy-map) yields the empty lazy-map.
  (lazy-map k1 v1 k2 v2...) is equivalent to (lazy-map (hash-map k1 v1 k2 v2...)).
  "
  ([] empty-lazy-map)
  ([m] (cond (empty? m) empty-lazy-map
             (map? m)   (LazyMap. m (delay (LazyMap-hashCode m)))
             :else      (arg-err "singleton arg of lazy-map must be a persistent map")))
  ([k1 & more] (lazy-map (apply hash-map k1 more))))

;; #VecMap #seqmap ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- vecmap-opt [v m]
  (let [v (if (vector? v) v (vec (seq v))), n (count v)
        m (cond (empty? m) {} (map? m) m :else (arg-err "invalid map arg"))
        v (reduce conj v (map (partial get m)
                              (take-while (partial contains? m) (range n (+ n (count m))))))
        m (reduce dissoc m (range (count v)))]
    [v m]))
(def-map-type VecMap [vec-part map-part]
  (keys        [_]      (concat (range (count vec-part)) (keys map-part)))
  (containsKey [_ k]    (or (contains? vec-part k) (contains? map-part k)))
  (count       [_]      (+ (count vec-part) (count map-part)))
  (get         [_ k nf] (cond (contains? vec-part k) (nth vec-part k nf)
                              (contains? map-part k) (get map-part k nf)
                              :else                  nf))
  (entryAt     [_ k]    (if (contains? vec-part k)
                          (clojure.lang.MapEntry. k (get vec-part k))
                          (find map-part k)))
  (meta        [_]      (let [mm (meta map-part), mv (meta vec-part)]
                          (if (identical? mm mv) mm (merge mv mm))))
  (with-meta   [this d] (let [v (with-meta vec-part d), m (with-meta map-part d)]
                          (if (and (identical? v vec-part) (identical? m map-part))
                            this
                            (VecMap. v m))))
  (assoc       [_ k x]  (let [[v m] (if (and (integer? k) (>= k 0) (<= k (count vec-part)))
                                      [(assoc vec-part k x) (dissoc map-part k)]
                                      [vec-part (assoc map-part k x)])
                              [v m] (vecmap-opt v m)]
                          (VecMap. v m)))
  (dissoc      [this k]
    (let [[v m] (if (contains? vec-part k)
                  (let [n (count vec-part)]
                    (cond
                      (= 0 k)       [(vec (next vec-part)) map-part]
                      (= (dec n) k) [(pop vec-part) map-part]
                      :else         [(subvec vec-part 0 k)
                                     (loop [m map-part, k (inc k), v (seq (subvec vec-part k))]
                                       (if v (recur (assoc m k (first v)) (inc k) (next v)) m))]))
                  [vec-part (dissoc map-part k)])]
      (if-not (and (identical? v vec-part) (identical? m map-part))
        (VecMap. (if (identical? v vec-part) vec-part (with-meta v (meta vec-part)))
                 (if (identical? m map-part) map-part (with-meta m (meta map-part))))
        this)))
  Object
  (toString [_] (let [s1 (string-join " " (map str vec-part))
                      s2 (string-join ", " (map (partial string-join " ") (seq map-part)))]
                  (str "[" s1 " \\ " s2 "]"))))
(defn seqmap?
  "
  (seqmap? u) yields true if u is a seqmap object and false otherwise.
  "
  [u] (instance? VecMap u))
(defn seqmap
  "
  (seqmap u) yields a map object that is equivalent, in terms of calls to get, to the vector, map,
    or seq u.
  (seqmap u m) yields a map object that is equivalent to (into m (map vector (range) (seq u))).
  (seqmap u m meta) additionally gives the returned map the given meta-data map.

  Note that (seqmap u) is equivalent to (seqmap (seq u)) if u is not a map; ergo sets are not
  respected by this function.
 
  Examples:
    (seqmap [:a :b :c])
    ;==> {0 :a, 1 :b, 2 :c}
    (seqmap [:a :b :c] {:x :y})
    ;==> {0 :a, 1 :b, 2 :c, :x :y}
  "
  ([u]      (cond (seqmap? u) u
                  (vector? u) (VecMap. u (with-meta {} (meta u)))
                  (map? u)    (VecMap. (with-meta [] (meta u)) u)
                  :else       (VecMap. (with-meta (vec (seq u)) (meta u)) (with-meta {} (meta u)))))
  ([v m]    (seqmap v m nil))
  ([v m md] (let [v (if (vector? v) v (vec (seq v))), n (count v)
                  m (cond (empty? m) {} (map? m) m :else (arg-err "invalid map arg"))
                  v (reduce conj v (map (partial get m)
                                        (take-while (partial contains? m)
                                                    (range n (+ n (count m))))))
                  m (reduce dissoc m (range (count v)))]
              (VecMap. (with-meta v md) (with-meta m md)))))

;; #Dictionary ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dictionaries consist of drows and dsets; drows are either map-like or vector-like objects that
;; can be addressed by obejcts (integers in the case of vectors) while dsets are set-like objects
;; that represent conjunctions of items. Dictionaries are capable of the following operations:
;;
;; Example dicts:
;;   * (def abc {:a [1 2 3] :b [4 5 6] :c [7 8 9]})
;;   * (def foo {:name :foo, :alias #{:FOO :Foo},
;;               :position #{ {:x 0 :y 1 :z #{-2 2}]} {:x 0 :y 2 :z #{-4 4}} }})
;;   * (def data {:type #{:unstructured :example}
;;                :rows  [{:a 1 :b 2 :c [3 4]},
;;                        {:a #{1 2 3} :b 1 :c 10}
;;                        {:a 10 :b #{12} :c 10}
;;                        {:a #{1 2 3} :b 14 :c 10}]
;;                :rset #{{:a 1 :b 2 :c [3 4]},
;;                        {:a #{1 2 3} :b 1 :c 10}
;;                        {:a 10 :b #{12} :c 10}
;;                        {:a #{1 2 3} :b 14 :c 10}}})
(declare view edit edits with wout sat?)
(deftype ^:private All [])
(def all
  "
  all is an object that can be used to address all parts of an object in the el and part functions.
  "
  (All.))
(def- missing
  "
  missing is used internally by nben's dictionary system to test for missing values via the
  dict-get function. It should never leave the nben.util.data namespace.
  "
  (Object.))
(deftype ^:private Del [])
(def del
  "
  del is an object that can be used in with-part and with-el calls to indicate that an item should
  be removed from the dictionary instead of replaced with another value.
  "
  (Del.))
(defn- dict-triage [d]
  (cond (set? d)       :set
        (vector? d)    :vec
        (map? d)       :map
        (seq? d)       :seq
        (derefable? d) :deref
        :else          :obj))
(defmultipro PDictRow
  "
  The PDictRow protocol defines basic operations for objects that want to be considered dictionary
  rows. Rows are usually either vectors or maps--objects that have keys/indices mapped to values.
  "
  #(cond (atom? %)      :atom
         (ref? %)       :ref
         (volatile? %)  :vol
         (derefable? %) :deref
         (set? %)       :obj
         :else          (dict-triage %))
  {:atom  {:dict-row?  #(dict-row? (deref %1))
           :dict-get   #(dict-get (deref %1) %2 %3)
           :dict-keys  #(dict-keys (deref %1))
           :dict-set   #(do (swap! %1 dict-set %2 %3) %1)
           :dict-unset #(do (swap! %1 dict-unset %2 %3) %1)
           :dict-key?  #(dict-key? (deref %1) %2)
           :dict-dims  #(dict-dims (deref %1))
           :dict-sub   #(do (swap! %1 dict-sub %2 %3 %4) %1)}
   :ref   {:dict-row?  #(dict-row? (deref %1))
           :dict-get   #(dict-get (deref %1) %2 %3)
           :dict-keys  #(dict-keys (deref %1))
           :dict-set   #(do (alter %1 dict-set %2 %3) %1)
           :dict-unset #(do (alter %1 dict-unset %2 %3) %1)
           :dict-key?  #(dict-key? (deref %1) %2)
           :dict-dims  #(dict-dims (deref %1))
           :dict-sub   #(do (alter %1 dict-sub %2 %3 %4) %1)}
   :vol   {:dict-row?  #(dict-row? (deref %1))
           :dict-get   #(dict-get (deref %1) %2 %3)
           :dict-keys  #(dict-keys (deref %1))
           :dict-set   #(do (swap! %1 dict-set %2 %3) %1)
           :dict-unset #(do (swap! %1 dict-unset %2 %3) %1)
           :dict-key?  #(dict-key? (deref %1) %2)
           :dict-dims  #(dict-dims (deref %1))
           :dict-sub   #(do (swap! %1 dict-sub %2 %3 %4) %1)}
   :deref {:dict-row?  #(dict-row? (deref %1))
           :dict-get   #(dict-get (deref %1) %2 %3)
           :dict-keys  #(dict-keys (deref %1))
           :dict-set   #(dict-set (deref %1) %2 %3)
           :dict-unset #(dict-unset (deref %1) %2)
           :dict-key?  #(dict-key? (deref %1) %2)
           :dict-dims  #(dict-dims (deref %1))
           :dict-sub   #(dict-sub (deref %1) %2 %3 %4)}
   :vec   {:dict-row?  constantly-true
           :dict-get   get
           :dict-keys  #(range (count %1))
           :dict-set   #(if (or (contains? %1 %2) (= %2 (count %1)))
                          (assoc %1 %2 %3)
                          (assoc (seqmap %1) %2 %3))
           :dict-unset #(cond (not (contains? %1 %2)) %1
                              (= (inc %2) (count %1)) (pop %1)
                              :else                   (dissoc (seqmap %1) %2))
           :dict-key?  contains?
           :dict-dims  count
           :dict-sub   (fn [this ii nf f]
                         (let [v (persistent!
                                  (if (identical? ii all)
                                    (reduce #(conj! %1 (f %2)) (transient []) this)
                                    (reduce #(conj! %1 (f (get this %2 nf))) (transient []) ii)))]
                           (if (= v this) this (with-meta v (meta this)))))}
   :map   {:dict-row?  constantly-true
           :dict-get   get
           :dict-keys  keys
           :dict-set   assoc
           :dict-unset dissoc
           :dict-key?  contains?
           :dict-dims  count
           :dict-sub   (fn [d ii nf f]
                         (let [m (persistent!
                                  (if (identical? ii all)
                                    (reduce #(assoc! %1 (key %2) (f (val %2))) (transient {}) d)
                                    (reduce #(assoc! %1 %2 (f (get d %2 nf))) (transient {}) ii)))]
                           (if (= m d) d (with-meta m (meta d)))))}
   :seq   {:dict-row?  constantly-true
           :dict-get   #(dict-get   (vec %1) %2 %3)
           :dict-keys  #(range (count %1))
           :dict-set   #(dict-set   (vec %1) %2 %3)
           :dict-unset #(dict-unset (vec %1) %2)
           :dict-key?  #(dict-key? (vec %1) %2)
           :dict-dims  count
           :dict-sub   #(dict-sub (vec %1) %2 %3 %4)}
   :obj   {:dict-row?  constantly-false
           :dict-get   #(arg-err "attempt to get from non-dict-row")
           :dict-keys  #(arg-err "attempt to keys from non-dict-row")
           :dict-set   #(arg-err "attempt to set non non-dict-row")
           :dict-unset #(arg-err "attempt to unset non non-dict-row")
           :dict-key?  constantly-nil
           :dict-dims  #(arg-err "attempt to find dims of non-dict-row")
           :dict-sub   #(arg-err "attempt to subdict non-dict-row")}}
  (dict-row? [_] "
  (dict-row? r) yields true if r is a valid dict-row object and false otherwise.
  ")
  (dict-get [_ k nf] "
  (dict-get d k nf) yields the value associated with the key k in the dictionary row d or nf if k
    is not in d. If d is not a dict-row object, throws an exception.
  ")
  (dict-keys [_] "
  (dict-keys d) yields a seq of keys in the given dictionary d.
  ")
  (dict-set [_ k v] "
  (dict-set d k v) yields a dictionary equivalent to d but with the given key k associated with the
    given value v. If d is not a dict-row object, yields nil.

  Note that dict-set may require converting the return-value type; for example:
    (dict-set [:x :y :z] :a :b)
    ;==> {0 :x, 1 :y, 2 :z, :a :b} 
  ")
  (dict-unset [_ k] "
  (dict-unset d k) yields a dictionary equivalent to d but with the given key dissociated; if d is
    not a dict-row object, yields nil.

  Notes:
    * dict-unset preserves keys over type, for example:
        [(dict-unset [:x :y :z] 2), (dict-unset [:x :y :z] 1)]
        ;==> [[:x :y], {0 :x, 2 :z}]
    * dict-unset does not raise exceptions when a key is not in the dictionary; it merely returns
      the identical object.
  ")
  (dict-key? [_ k] "
  (dict-key? d k) yields true if k is a key in the dictionary row d; otherwise yields false. If d is
    not a dictionary row, yields nil.
  ")
  (dict-dims [_] "
  (dict-dims d) yields the number of dimensions in the dictionary d assuming that dictionary d is a
    dict-row object.
  ")
  (dict-sub [_ ii not-found subfn] "
  (dict-sub d ii not-found subfn) yields a sub-dictionary like d but with only the key or keys found
    in ii; missing keys will be replaced with not-found, and subfn will be called on each key below
    the given level.
  "))
(extend-protocol PDictRow
  nil
  (dict-row?  [_]         false)
  (dict-get   [_ k nf]    nf)
  (dict-set   [_ k v]     (if (= k 0) (seqmap [v] {}) (seqmap [] {k v})))
  (dict-unset [_ k]       nil)
  (dict-keys  [_]         nil)
  (dict-key?  [_ k]       false)
  (dict-dims  [_]         0)
  (dict-sub   [_ ii nf f] (when-not (identical? ii all)
                            (seqmap (apply hash-map (interleave ii (repeat (f nf))))))))
(defmultipro PDictSet
  "
  PDictSet is the protocol for sets of things in dictionaries. It mostly should just wrap clojure's
  set types.
  "
  #(cond (ref? %) :ref (atom? %) :atom (volatile? %) :vol (derefable? %) :deref
         (set? %) :set :else :obj)
  {:set   {:dict-set? constantly-true
           :dict-has? contains?
           :dict-seq  seq
           :dict-drop disj
           :dict-add  conj}
   :ref   {:dict-set? #(dict-set? (deref %1))
           :dict-has? #(dict-has? (deref %1) %2)
           :dict-seq  #(dict-seq (deref %1))
           :dict-add  #(do (alter %1 dict-add %2) %1)
           :dict-drop #(do (alter %1 dict-drop %2) %1)}
   :atom  {:dict-set? #(dict-set? (deref %1))
           :dict-has? #(dict-has? (deref %1) %2)
           :dict-seq  #(dict-seq (deref %1))
           :dict-add  #(do (swap! %1 dict-add %2) %1)
           :dict-drop #(do (swap! %1 dict-drop %2) %1)}
   :vol   {:dict-set? #(dict-set? (deref %1))
           :dict-has? #(dict-has? (deref %1) %2)
           :dict-seq  #(dict-seq (deref %1))
           :dict-add  #(do (vswap! %1 dict-add %2) %1)
           :dict-drop #(do (vswap! %1 dict-drop %2) %1)}
   :deref {:dict-set? #(dict-set? (deref %1))
           :dict-has? #(dict-has? (deref %1) %2)
           :dict-seq  #(dict-seq (deref %1))
           :dict-add  #(dict-add (deref %1) %2)
           :dict-drop #(dict-drop (deref %1) %2)}
   :obj   {:dict-set? constantly-false
           :dict-has? =
           :dict-seq  list
           :dict-drop #(if (= %1 %2) #{} %1)
           :dict-add  #(-> #{%1 %2})}}
  (dict-set? [_] "
  (dict-set? d) yields true if the object d is a dict-set and false otherwise.
  ")
  (dict-has? [_ k] "
  (dict-has? d k) yields true if the given object k is in the given dict-set d else false.
  ")
  (dict-seq [_] "
  (dict-seq d) yields a seq of elements in the given dict-set.
  ")
  (dict-drop [_ k] "
  (dict-drop d x) yields a copy of the dictionary-set d with the object x dropped.
  ")
  (dict-add [_ k] "
  (dict-add d k) yields a copy of the dictionary-set d with the object k added.
  "))
(extend-protocol PDictSet
  nil
  (dict-set? [_]   false)
  (dict-has? [_ k] false)
  (dict-seq  [_]   nil)
  (dict-drop [_]   nil))
(defn- dict-key-err
  ([k]   (arg-err "Key <" k "> not found"))
  ([d k] (arg-err "Key <" k "> not found in dict <" d ">")))
(defn- dgeterr [d k]
  (let [v (dict-get d k missing)] (if (identical? v missing) (dict-key-err k) v)))
(defn dict?
  "
  (dict? d) yields true if d is a valid dict object and false otherwise.
  "
  [d] (or (dict-row? d) (dict-set? d)))

;; #PDictLens ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- map-view [d f ks vs]
  (loop [k ks, v vs, r (transient {})]
    (if k
      (recur (next k) (next v) (assoc! r (first k) (f (dgeterr d (first v)))))
      (persistent! r))))
(defn- set-view [d f ks] (map-view d f ks ks))
(defn- seq-view [d f ks]
  (loop [k ks, r (transient [])]
    (if k
      (recur (next k) (conj! r (f (dgeterr d (first k)))))
      (persistent! r))))
(defmultipro PDictLens
  "
  PDictLens is a protocol for objects that can construct a view onto a dictionary.
  "
  #(cond (set? %) :set, (map? %) :map, (vector? %) :seq, (seq? %) :seq,
         (derefable? %) :deref (dict-row? %) :drow, (dict-set? %) :dset,
         :else :obj)
  {:set  {:dict-lens?   constantly-true
          :dict-lens-kv (fn [k d] (map vector (seq k) (seq k)))
          :dict-view    #(set-view %2 %3 (seq %1))
          :dict-lens-of get
          :lens-single? constantly-false
          :dict-ldrop   disj}
   :map  {:dict-lens?   constantly-true
          :dict-lens-kv (fn [k d] (seq k))
          :dict-view    #(map-view %2 %3 (keys %1) (vals %1))
          :lens-single? constantly-false
          :dict-lens-of get
          :dict-ldrop   dissoc}
   :seq  {:dict-lens?   constantly-true
          :dict-lens-kv (fn [k d] (map vector (range) (seq k)))
          :dict-view    #(seq-view %2 %3 (seq %1))
          :lens-single? constantly-false
          :dict-lens-of nth
          :dict-ldrop   #(dissoc (seqmap %1) %2)}
   :drow {:dict-lens?   constantly-true
          :dict-lens-kv (fn [k d] (for [kk (dict-keys k)] [kk (dgeterr k kk)]))
          :dict-view    #(let [ks (dict-keys %1)] (map-view %2 %3 ks (map (partial dgeterr %1) ks)))
          :lens-single? constantly-false
          :dict-lens-of dict-get
          :dict-ldrop   dict-unset}
   :dset {:dict-lens?   constantly-true
          :dict-lens-kv (fn [k d] (map vector (dict-seq k) (dict-seq k)))
          :dict-view    #(set-view %2 %3 (dict-seq %1))
          :lens-single? constantly-false
          :dict-lens-of #(if (dict-has? %1 %2) %2 %3)
          :dict-ldrop   dict-drop}
   :obj  {:dict-lens?   constantly-false
          :dict-lens-kv (fn [k d] (cons [k k] nil))
          :dict-view    #(%3 (dgeterr %2 %1))
          :lens-single? constantly-true
          :dict-lens-of #(if (= %1 %2) %2 %3)
          :dict-ldrop   #(arg-err "lens-drop requested of non-lens object " %1 " (" %2 ")")}}
  (dict-lens? [_] "
  (dict-lens? l) yields true if l is a valid dictionary lens object and false otherwise.
  ")
  (dict-lens-kv [_ d] "
  (dict-lens kv l d) yields a seq of map-entries or 2-vectors [k v] where k is the key transformed
    by the lens l into the key v when applied to the dictionary d. For most lenses, the d may be
    replace with any value, but for lenses that depend on their dictionary's contents, such as all,
    the d is required to be correct.
  ")
  (dict-view [_ d f] "
  (dict-view l d f) yields a view of the given dictionary d through the given lens l with the
    function f applied to sub-elements.
  ")
  (lens-single? [_] "
  (lens-single? l) yields true if l is a single lens such as a keyword and false if it is not.
  ")
  (dict-lens-of [_ k nf] "
  (dict-lens-of l k nf) yields the key into which the lens l transforms key k or nf if the lens l
    does not include key k.
  ")
  (dict-ldrop [_ k] "
  (dict-lrdrop l k) drops the key k from the dict lens l and returns the new dictionary lens.
  "))
(extend-protocol PDictLens
  nil
  (dict-lens? [_] false)
  (dict-lens-kv [_ d] (cons [nil nil] nil))
  (dict-view [_ d f] (dgeterr d nil))
  (lens-single? [_] false)
  (dict-lens-of [_ k nf] nf)
  (dict-ldrop [_ k] nil)
  All
  (dict-lens? [_] true)
  (dict-lens-kv [_ d] (if (dict-row? d)
                        (map vector (dict-keys d) (dict-keys d))
                        (arg-err "cannot take all dict-keys of non dict-row")))
  (dict-view [_ d f] (dict-sub d all nil f))
  (lens-single? [_] false)
  (dict-lens-of [_ k nf] k)
  (dict-ldrop [_ k] (arg-err "Cannot drop from an all index")))

;; #PPattern ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- patt-conditions [obj]
  "Given an object with meta-data, extracts a seq of conditions from that meta-data."
  (when-let [m (meta obj)]
    (filter #(let [k (key %)] (and (vector? k) (every? keyword? k)))
            (seq m))))
(defn- patt-params
  "Given an object with meta-data, extracts pattern parameters from that meta-data."
  [obj]
  (loop [r (transient #{}), s (filter keyword? (apply concat (map first (patt-conditions obj))))]
    (if s (recur (conj! r (first s)) (next s)) (persistent! r))))
(defn- patt-allow
  "Given an object with meta-data extracts the allowed pattern keys from that meta-data."
  [obj]
  (when-let [m (and (meta? obj) (meta obj))]
    (when-let [a (get m :allow)]
      (cond (coll? a)    (set a)
            (= a :any)   :any
            (= a :all)   :any
            (= a :none)  nil
            (keyword? a) #{a}
            :else        (arg-err "Invalid :allow tag in pattern meta-data")))))
(defn- patt-then
  "Given an object with meta-data extracts the asserted pattern assignments from that meta-data."
  [obj]
  (when-let [m (and (meta? obj) (meta obj))]
    (when-let [a (get m :then)]
      (cond (map? a)     a
            (= a :none)  nil
            (coll? a)    (apply hash-map a)
            :else        (arg-err "Invalid :then tag in pattern meta-data")))))
(defn- pattget [d k]
  (if-let [me (find d k)] (val me) (arg-err "condition requires " k " not matched in params")))
(defn- patt-check
  "
  Given an object with meta-data and a parameter-map satisfying it, ensures that all conditions are
  met for the pattern object.
  "
  [obj params]
  (let [cs (patt-conditions obj), getf (partial pattget params)]
    (if (empty? cs)
      true
      (loop [[[args f] & more] cs]
        (cond (and f (not (apply f (map getf args)))) false
              more                                    (recur more)
              :else                                   true)))))
(defn- match-merge
  "
  (match-merge a b) yields a merged version of maps a and b if for every key k in a associated
    with a value v, the map b either does not contain the key k or the key k is associated with
    value v in b also.
  "
  [a b]
  (let [[a b] (if (< (dict-dims a) (dict-dims b)) [a b] [b a])
        aks (dict-keys a), bks (dict-keys b)]
    (cond (empty? aks) b, (empty? bks) a,
          :else (loop [aks aks, b (reduce #(assoc! %1 %2 (dict-get b %2 nil)) (transient {}) bks)]
                  (if aks
                      (let [k (first aks), v (dict-get a k nil), bv (get b k missing)]
                        (cond (identical? bv missing) (recur (next a) (assoc! b k v))
                              (= bv v)                (recur (next a) b)
                              :else                   nil))
                      (persistent! b))))))
(defn- basic-join [a b]
  (let [a (filter dict-row? (seq a)), sb (filter dict-row? (seq b))]
    (when-not (and (empty? a) (empty? b))
      (let [r (loop [q (first a), a (next a), b sb, r (transient #{})]
                (cond b (recur q a (next b) (if-let [m (match-merge q (first b))] (conj! r m) r))
                      a (recur (first a) (next a) sb r)
                      :else (persistent! r)))]
        (when-not (empty? r) r)))))
(defn- add-patt-meta [p2 p1] ;; from p1 to p2
  (let [ps      (vec (patt-params p1))
        [p2 m2] (cond (with-meta? p2) [p2 (meta p2)]
                      (meta? p2)      [#{p2} (meta p2)]
                      (empty? ps)     [p2 nil]
                      :else           [#{p2} nil])
        m2 (if (or (empty? ps) (contains? m2 ps)) m2 (assoc m2 ps nil))]
    (if m2 (with-meta p2 m2) p2)))
(defprotocol PPattern
  "
  The PPattern protocol is extended by patterns and used in matching.
  "
  (satisfiers [_ d] "
  (satisfiers p d) yields the set of matches that satisfy the pattern p in the dictionary d. Each
    match is represented as a map of parameters to matched values. If the set of satisfiers is
    empty, nil is yielded. If the pattern has no parameters but there is a match, then the '#{{}} is
    yielded.
  "))
(defn- dict-match [prow d]
  (let [params (patt-params prow)
        ss (cond
             ;; if this is a dict-set, we match anything inside of it
             (dict-set? d)
             (apply concat (filter identity (map (partial satisfiers prow) (dict-seq d))))
             ;; if prow is a dict-set, we need to find matches to all of its elements
             (dict-set? prow)
             (let [s (dict-seq prow)]
               (cond (empty? s)      #{{}}
                     (nil? (next s)) (let [f (first s)]
                                       (cond (contains? params f) #{{f d}}
                                             (dict-row? f) (dict-match (add-patt-meta f prow) d)
                                             :else (satisfiers f d)))
                     :else (reduce basic-join
                                   (map #(satisfiers (add-patt-meta % prow) d) (dict-seq prow)))))
             ;; if d is a not a dict, (and prow is not a set), there can't be a match
             (not (dict? d)) nil
             ;; if empty, simple:
             (empty? d) (when (and (empty? prow) (patt-check prow {})) #{{}})
             ;; if this is a dict-row, we match keys plus meta-data instructions:
             (dict-row? d)
             (let [{allow :allow, sarg :then} (meta prow)
                   allow (cond (or (not allow) (= allow :none)) constantly-false
                               (contains? #{:any :all} allow)   constantly-true
                               :else                            allow)
                   ss (loop [ks (dict-keys d), ss nil]
                        (if ks
                          (let [[k & ks] ks, v (dict-get d k d), pv (dict-get prow k missing)]
                            (cond
                              ;; could be missing from the pattern
                              (identical? pv missing) (when (allow k) (recur ks ss))
                              ;; could be that the pattern allows a wildcard
                              (contains? params pv) (recur ks (basic-join ss [{pv v}]))
                              ;; could be that they match... if not, no match here either...
                              :else (when-let [ms (satisfiers (add-patt-meta pv prow) v)]
                                      (recur ks (basic-join ss ms)))))
                          ss))]
               (when-not (empty? ss) ss))
             ;; otherwise we don't match...
             :else nil)
        sarg (patt-then prow)]
    (when-let [ss (filter (partial patt-check prow)
                          (if sarg (map (partial basic-join [sarg]) ss) ss))]
      (when-not (empty? ss) (set ss)))))
(extend-protocol PPattern
  nil    (satisfiers [_ x] (when (empty? x) #{{}}))
  Object (satisfiers [o x]
           (if (identical? (class x) Object)
             (when (= o x) #{{}})
             (do (extend (class o)
                   PPattern {:satisfiers (if (or (map? o) (set? o) (vector? o) (seq? o))
                                     dict-match
                                     #(when (= %1 %2) #{{}}))})
                 (satisfiers o x)))))
(defn sat?
  "
  (sat? p d) yields true if the given pattern p is satisfied by the dictionary d and false
    otherwise.
  "
  [p d] (not (empty? (satisfiers p d))))
(deftype ^:private Wildcard []
  PPattern
  (satisfiers [_ d] #{{}}))
(def ??
  "
  ?? is a wildcard pattern; when used as a pattern, it matches against anything.
  "
  (Wildcard.))
(def- patt-options [:allow :then])
(defn patt
  "
  (patt form [p1 p2...] condition-fn) yields a patterm with the given form, the given set of
    parameters [p1 p2...], and the given condition function (which must accept the arguments that
    match the parameters, in the given order). It is recommended that the parameters be keywords.
  (patt form [p1 p2...]) yields a pattern without an explicit condition.
  (patt form) yields a pattern without parameters or a condition; note that this may be useful for
    passing optional pattern settings.

  Note that patterns are expressed in meta-data, thus the returned pattern is likely to be form or
  a set containing form with additional meta-data.

  The following optional arguments are accepted:
    * :allow [keys] specifies that the pattern should allow the additional keys listed to appear in
      the matched form; the keys vector may alternately be :any or :all to specify that any
      additional keys may appear.
    * :then {k1 v1, k2 v2...} specifies that the named parameters given by k1, k2, etc. (which
      should not listed in params) should also be set to the values v1, v2, etc. in the resulting
      satisfier map when matched.
  "
  [form & more]
  (let [form                 (if (with-meta? form) form #{form})
        [params & more]      more
        [params condfn more] (if (or (coll? params) (nil? params))
                               [params (first more) (next more)]
                               [nil params more])
        [condfn more]        (if (contains? patt-options condfn)
                               [nil (cons condfn more)]
                               [condfn more])
        {a :allow t :then}   more
        meta                 (merge (when (or params condfn) {(vec params) condfn})
                                    (when a {:allow (vec a)})
                                    (when t {:then  (if (map? t) t (apply hash-map t))}))]
    (if-not (empty? meta) (with-meta form meta) form)))
(defmacro pattern
  "
  (pattern form [p1 p2...] condition) yields a patterm with the given form, the given set of
    parameters [p1 p2...], and the given condition expression (which may refer to the parameters).
  (pattern form [p1 p2...]) yields a pattern without an explicit condition.

  Note that patterns are expressed in meta-data, thus the returned pattern is likely to be form or
  a set containing form with additional meta-data.

  The following optional arguments are accepted:
    * :allow [keys] specifies that the pattern should allow the additional keys listed to appear in
      the matched form; the keys vector may alternately be :any or :all to specify that any
      additional keys may appear.
    * :then {k1 v1, k2 v2...} specifies that the named parameters given by k1, k2, etc. (which
      should not listed in params) should also be set to the values v1, v2, etc. in the resulting
      satisfier map when matched.
  "
  [form & more]
  (let [[params condfn a t]
        (loop [s (seq more), dat (transient {})]
          (if s
            (let [f (first s), n (next s)]
              (cond (or (= f :allow) (= f :then))
                    (if (nil? n)
                      (arg-err "no value given for " f)
                      (recur (next n) (assoc! dat f (first n))))
                    (identical? (get dat :params missing) missing)
                    (recur (next s) (assoc! dat :params f))
                    (identical? (get dat :condfn missing) missing)
                    (recur (next s) (assoc! dat :condfn f))
                    :else (arg-err "unrecognized argument: " f)))
            [(get dat :params) (get dat :condfn missing) (get dat :allow) (get dat :then)]))
        fnargs (vec params)
        kwargs (vec (map keyword params))
        meta   (merge {kwargs (if (or (identical? condfn missing) (true? condfn))
                                `(fn [& more#] true)
                                `(fn ~fnargs ~condfn))}
                      (when a {:allow (if (contains? #{:any :all} a) :any (vec a))})
                      (when t {:then  (if (map? t) t (apply hash-map t))}))]
    (if-not (empty? meta)
      `(let [f# ~form] (with-meta (if (with-meta? f#) f# #{f#}) ~meta))
      form)))
(defn sats
  "
  (sats dict patt) is equivalent to (satisfiers patt dict). This form is intended to be used along
    with the functions pick, view, part, crop, with, wout, and edit and the macro -> in that the
    functions all transform a dictionary, which is their first argument.
  (sats dict form [params...]) is equivalent to (sats dict (patt form [params...])).
  (sats dict form [params...] cond-fn) is equivalent to (sats dict (patt form [params...] cond-fn)).
  "
  ([d p] (satisfiers p d))
  ([d p params] (satisfiers (patt p params) d))
  ([d p params condfn] (satisfiers (patt p params condfn) d)))

;; #DerefView ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defrecord DerefView [derefable lens-path]
  clojure.lang.IDeref
  (deref [_] (apply view (deref derefable) lens-path))
  ;; a View may be either a set or a row, depending on what it holds...
  PDictRow
  (dict-row? [this]         (dict-row? (deref this)))
  (dict-get  [this k nf]    (dict-get  (deref this) k nf))
  (dict-keys [this]         (dict-keys (deref this)))
  (dict-key? [this k]       (dict-key? (deref this) k))
  (dict-sub  [this ks nf f] (dict-sub  (deref this) ks nf f))
  (dict-set  [this k v] 
    (let [lp (filter (comp not lens-single?) lens-path)
          ss (take-while lens-single? lens-path)
          l0 (if (empty? lp) missing (first lp))
          kk (if (identical? l0 missing) k (dict-lens-of l0 k missing))]
      (if (identical? kk missing)
        (arg-err "attempt to set unrecognized key " kk " in derefable view")
        (let [newderef (edits derefable (concat ss [kk]) v)]
          (if (identical? newderef derefable) this (apply view newderef lens-path))))))
  (dict-unset [this k]
    (let [l0 (first lens-path), kk (dict-lens-of l0 k missing)]
      (when-not (identical? kk missing)
        (let [dnew (dict-unset derefable kk), lens-path (cons (dict-ldrop l0 kk) (next lens-path))]
          (if (identical? derefable dnew)
            (DerefView. dnew lens-path)
            (apply view dnew lens-path))))))
  PDictSet
  (dict-set? [this]   (dict-set? (deref this)))
  (dict-has? [this k] (dict-has? (deref this) k))
  (dict-seq  [this]   (dict-seq  (deref this)))
  (dict-drop [this k] (let [x (dict-drop derefable k)]
                        (if (identical? derefable x) this (apply view x lens-path))))
  (dict-add  [this k] (let [x (dict-add derefable k)]
                        (if (identical? derefable x) this (apply view x lens-path)))))
(defmethod print-method DerefView [v ^java.io.Writer w]
  (.write w (str "#view[" (.derefable v) (apply str (interleave (.lens-path v) (repeat " ")) "]"))))

;; #IndexedSet ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;#TODO
(def- indexed-set hash-set)
(defn- indexed-set? [_] false)

;; #view ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn view
  "
  (view d) yields d.
  (view d k) is equivalent to (dict-get d k) except that it throws an exception if the value is not
    found.
  (view d k1 k2...) is roughly equivalent to (reduce dict-get d [k1 k2...]) except that it leaves
    any dict-set type that it encounters as a set.
  "
  [d & more]
  (cond (empty? more)  d
        (derefable? d) (DerefView. d more)
        (dict-set? d)  (apply indexed-set (map #(apply view % more) (dict-seq d)))
        (dict-row? d)  (let [k (first more), nk (next more)]
                         (if (dict-lens? k)
                           (dict-view k d (if nk #(apply view % nk) identity))
                           (let [v (dict-get d k missing)]
                             (if (identical? v missing)
                               (arg-err "Key not found in dict: " k)
                               (apply view v nk)))))
        :else          (arg-err "Cannot access element of non-dict")))
;; #view ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn part
  "
  (part args...) yields (freeze (view args...)).
  "
  [& args] (freeze (apply view args)))

;; #edit and #edits ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- dicts-union [seq-of-vals]
  (apply union (for [u (seq seq-of-vals)]
                 (cond (set? u)      u
                       (dict-set? u) (set (dict-seq u))
                       :else         #{u}))))
(defn- edit-vget
  "
  For a call like (edit d k1 k2 v), (edit-vget v k1) yields the value obtained by descending into
  the value with key k1; if v is, for example, a string, this will yield v no matter what k1 is; if
  k1 is a non-negative integer and v is a dict whose only key is 0, yields the value in v at 0 no
  matter what value k has; otherwise, v must have a value at the given key k1."
  [v k]
  (cond
    (dict-set? v)             (dicts-union (for [u (dict-seq v)] (edit-vget u k)))
    (not (dict-row? v))       v
    (dict-key? v k)           (dgeterr v k)
    (and (dict-key? v 0)
         (= 1 (dict-dims v))) (dgeterr v 0)
    :else                     (arg-err "edit key mismatch at " v " with key " k)))
(defn edits
  "
  (edits d keys1 dval1 keys2 dval2...) yields a duplicate of the dictionary d but with the
    dict-subionaries specified by the given key-vectors (keys1, keys2, etc.) replaced with the
    dict-sub dvals. The dval dictionaries must have a matching shape as the selected subdict, which
    will have a shape equivalent to (view d keys...). Note that the dvals may not fully specify
    all elements but instead specify all elements below the given level (i.e., this will thread over
    elements if possible).
  (edits d) yields d.

  Note that any dval may be set to the special object del, which indicates that the key should be
  dissociated from the dict.

  Examples:
    (def data {:a {:b [4 6 8] :c [3 5 7]} :x {:y 1 :z 4}})
    (edits data [:a :c [0 2]] [:x :y])
       ; ==> {:a {:b [4 6 8] :c [:x 5 y]} :x {:y 1 :z 4}}
    (edits data [:a [:c :b] all] [[0 1 2] [3 4 5]])
       ; ==> {:a {:b [3 4 5] :c [0 1 2]} :x {:y 1 :z 4}}
    (edits data :a [[:c :b] all] {:b [0 1 2], :c [3 4 5]})
       ; ==> {:a {:b [0 1 2] :c [3 4 5]} :x {:y 1 :z 4}}
  "
  ([d] d)
  ([d ks v]
   (cond (empty? ks)   v
         (dict-set? d) (dicts-union (for [u (dict-seq d)] (edits u ks v)))
         (dict-row? d) (let [[k & ks] ks]
                         (build d d [[kv kd] (dict-lens-kv k d)
                                     :let    [vv (if (lens-single? k) v (edit-vget v kv))
                                              vd (dict-get d kd {})]]
                           (cond ks                  (dict-set d kd (edits vd ks vv))
                                 (identical? vv del) (dict-unset d kd)
                                 :else               (dict-set d kd vv))))
         :else         (arg-err "cannot edit parts " ks " of non-dict object")))
  ([d k1 v1 & more]
   (loop [s (seq more), d (edits d k1 v1)]
     (if s
       (if-let [ss (next s)]
         (recur (next ss) (edits d (first s) (first ss)))
         (arg-err "edits: even number of key/value arguments required"))
       d))))
(defn edit
  "
  (edit d keys... dval) yields a duplicate of the dictionary d but with the dict-subionary specified
    by the given keys replaced with the dict-sub dval. The dval dictionary must have a matching
    shape as the selected subdict, which will have a shape equivalent to (part d keys...). Note that
    dval may not fully specify all elements but instead specify all elements below the given level.

  Examples:
    (def data {:a {:b [4 6 8] :c [3 5 7]} :x {:y 1 :z 4}})
    (edit data :a :c [0 2] [:x :y])
       ; ==> {:a {:b [4 6 8] :c [:x 5 y]} :x {:y 1 :z 4}}
    (edit data :a [:c :b] all [[0 1 2] [3 4 5]])
       ; ==> {:a {:b [3 4 5] :c [0 1 2]} :x {:y 1 :z 4}}
    (edit data :a [:c :b] all {:b [0 1 2], :c [3 4 5]})
       ; ==> {:a {:b [0 1 2] :c [3 4 5]} :x {:y 1 :z 4}}
  "
  [d k1 & more]
  (let [[ks v] (most-last (cons k1 more))]
    (if (empty? ks) v (edits d ks v))))

;; #with ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn withs
  "
  (withs d [keys...] dval) yields a duplicate of the dictionary d but with the dict-subionary
    specified by the given keys conjoined with the dict-sub dval. The dval dictionary must have a
    matching shape as the selected subdict, which will have a shape equivalent to (part d keys...).
    Note that dval may not fully specify all elements but instead specify all elements below the
    given level.
  "
  ([d ks v]
   (cond (empty? ks)   (if (dict-set? d) (dict-add d v) #{d v})
         (dict-set? d) (build d d [x (dict-seq d)] (dict-add (dict-drop d x) (apply withs x ks v)))
         (dict-row? d) (let [[k & ks] ks]
                         (build d d [[kv kd] (dict-lens-kv k d)
                                     :let    [vv (if (lens-single? k) v (edit-vget v kv))
                                              vd (dict-get d kd {})]]
                           (dict-set d kd (if ks (withs vd ks vv) (dict-add vd vv)))))
         :else         (arg-err "with cannot add into non-dict-row")))
  ([d k1 v1 k2 v2 & more]
   (loop [s (seq more), d (withs (withs d k1 v1) k2 v2)]
     (if s
       (if-let [ss (next s)]
         (recur (next ss) (withs d (first s) (first ss)))
         (arg-err "withs: even number of key/value arguments required"))
       d))))
(defn with
  "
  (with d keys... dval) yields a duplicate of the dictionary d but with the dict-subionary specified
    by the given keys conjoined with the dict-sub dval. The dval dictionary must have a matching
    shape as the selected subdict, which will have a shape equivalent to (part d keys...). Note that
    dval may not fully specify all elements but instead specify all elements below the given level.
  "
  [d & more]
  (if more 
    (let [[ks v] (most-last more)]
      (if (empty? ks) (dict-add d v) (withs d ks v)))
    d))

;; #wout ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn wouts
  "
  (wouts d [keys...] v) yields a duplicate of the dictionary d but with the dict-subionary v at the
    position specified by the given keys disjoined. The dictionary v must have a matching shape to
    the selected subdict, which will have a shape equivalent to (part d keys...). Note that v may
    not fully specify all elements but instead specify all elements below the given level.
  "
  ([d ks v]
   (cond (empty? ks)   (cond (dict-set? d) (dict-drop d v) (= d v) #{} :else d)
         (dict-set? d) (build d d [x (dict-seq d)] (dict-add (dict-drop d x) (apply wouts x ks v)))
         (dict-row? d) (let [[k & ks] ks]
                         (build d d [[kv kd] (dict-lens-kv k d)
                                     :let    [vv (if (lens-single? k) v (edit-vget v kv))
                                              vd (dict-get d kd {})]]
                           (dict-set d kd (if ks (wouts vd ks vv) (dict-drop vd vv)))))
         :else         (arg-err "wout cannot drop from non-dict-row")))
  ([d k1 v1 k2 v2 & more]
   (loop [s (seq more), d (wouts (wouts d k1 v1) k2 v2)]
     (if s
       (if-let [ss (next s)]
         (recur (next ss) (wouts d (first s) (first ss)))
         (arg-err "withs: even number of key/value arguments required"))
       d))))
(defn wout
  "
  (wout d keys... v) yields a duplicate of the dictionary d but with the dict-subionary v at the
    position specified by the given keys disjoined. The dictionary v must have a matching shape to
    the selected subdict, which will have a shape equivalent to (part d keys...). Note that v may
    not fully specify all elements but instead specify all elements below the given level.
  "
  [d & more]
  (if more 
    (let [[ks v] (most-last more)]
      (if (empty? ks) (dict-drop d v) (wouts d ks v)))
    d))
;; #pick ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn pick
  "
  (pick dict f) yields a duplicate of the given dictionary dict containing only the elements for
    which the predicate f yields a true-like value. If there are no matches, yields nil.
  "
  [d f]
  (cond (dict-set? d) (loop [r #{}, s (dict-seq d)]
                        (cond (not (empty? s)) (let [el (first s)]
                                                 (recur (if (f el) (conj r el) r)
                                                        (next s)))
                              (empty? r) nil
                              :else r))
        (f d) d
        :else nil))

;; #JSON ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(declare to-json-struct from-json-struct)
(defn to-json-string
  "
  (to-json-string obj) yields a JSON-compatible string that represents the given object obj; the
    type of the object is indicated by the first character of the string.

  Strings, symbols, keywords and numbers can all be converted into strings.
  "
  [obj]
  (cond (string? obj)  (str \" obj)
        (symbol? obj)  (str \' obj)
        (keyword? obj) (str obj)
        (integer? obj) (str \# obj)
        (number? obj)  (str \. (Float/floatToRawIntBits (.doubleValue (float obj))))
        :else (arg-err "Object cannot be converted to JSON-string: " obj)))
(def- json-string-translators
  {\" identity
   \' symbol
   \: keyword
   \# #(Integer/parseInt %)
   \. #(Float/intBitsToFloat (.longValue (Integer/parseInt %)))})
(defn from-json-string
  "
  (from-json-string s) yields a JSON-string compatible object from the given JSON-string s. This is
    the inverse of to-json-string.
  "
  [s]
  (if-let [f (get json-string-translators (first s))]
    (f (subs s 1))
    (arg-err "Unrecognized string-object type: " (first s))))
(defn to-json-list
  "
  (to-json-list obj) yields a JSON-compatible vector that represents the given object obj. The obj
    must be a set, seq, or vector.
  "
  [obj]
  (cond (set? obj)    (vec (cons "set" (map to-json-struct obj)))
        (vector? obj) (vec (cons "vec" (map to-json-struct obj)))
        (seq? obj)    (vec (cons "seq" (map to-json-struct obj)))
        :else         (arg-err "Given object cannot be converted to JSON-compatible list: " obj)))
(def- json-list-translators
  {"set" #(set (map from-json-struct %))
   "vec" #(vec (map from-json-struct %))
   "seq" #(seq (map from-json-struct %))})
(defn from-json-list
  "
  (from-json-list obj) yields an object encoded as a JSON-compatible vector. This is the inverse of
    to-json-list.
  "
  [obj]
  (if-let [f (get json-list-translators (first obj))]
    (f (next obj))
    (arg-err "Unrecognized JSON-list type: " (first obj))))
(defn to-json-map
  "
  (to-json-map obj) yields a JSON-compatible map that represents the given object obj. The obj must
    be a map type.
  "
  [obj]
  (if (map? obj)
    (loop [ks (map to-json-string (keys obj)), vs (map to-json-struct (vals obj)), r (transient {})]
      (if ks
        (recur (next ks) (next vs) (assoc! r (first ks) (first vs)))
        (persistent! r)))
    (arg-err "Given object cannot be converted to JSON-compatible map: " obj)))
(defn from-json-map
  "
  (from-json-map m) yields a map that is a decoded copy of the JSON-compatible map m. This is the
    inverse of to-json-map.
  "
  [m]
  (if (map? m)
    (loop [ks (map from-json-string (keys m)), vs (map from-json-struct (vals m)), r (transient {})]
      (if ks
        (recur (next ks) (next vs) (assoc! r (first ks) (first vs)))
        (persistent! r)))
    (arg-err "Given object cannot be converted to JSON-compatible map: " m)))
(defn from-json-struct
  "
  (from-json-struct data) yields a decoded copy of the given JSON-structure data. This is the
    inverse of to-json-struct.
  "
  [obj]
  (cond
    ;; numbers get left as they are
    (number? obj) obj
    ;; strings...
    (string? obj) (from-json-string obj)
    ;; maps...
    (map? obj) (from-json-map obj)
    ;; vectors/lists
    (or (seq? obj) (vector? obj)) (from-json-list obj)
    ;; otherwise, ?
    :else (arg-err "Given object is not a valid JSON-compatible structure: " obj)))
(defn to-json-struct
  "
  (to-json-struct data) yields a copy of data that is compatible for conversion to JSON and back.
  "
  [obj]
  (cond
    ;; numbers get left as they are
    (number? obj) obj
    ;; anything that is always a string should be encoded such:
    (or (string? obj) (symbol? obj) (keyword? obj)) (to-json-string obj)
    ;; if we have a set, seq, or pure-vector, that get's encoded as a list-type
    (or (set? obj) (vector? obj) (seq? obj)) (to-json-list obj)
    ;; if we have a map, encode it...
    (map? obj) (to-json-map obj)
    ;; otherwise, not sure what this is...
    :else (arg-err "Given object cannot be converted to JSON-compatible structure: " obj)))

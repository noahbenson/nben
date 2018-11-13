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
  (:use [nben.util structured]))

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
        this))))
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
;; part:
;;  (part data :rows 0 :c)
;;   ;==> [3 4]
;;  (part foo :position :z)
;;   ;==> #{-4 -2 2 4}
;;  (part foo :position :y)
;;   ;==> #{1 2}
;;  (part data :rows all :a)
;;   ;==> [1 #{1 2 3} 10 #{1 2 3}]
;;  (part data :rows all [:a :b])
;;   ;==> [[1 2] [#{1 2 3} 1] [10 12] [#{1 2 3} 14]]
;;  (part data :rset :a)
;;   ;==> #{1 2 3 10}
;;  (part data :rset [:a :b])
;;   ;==> #{[1  2]  [1 1]  [2 1]  [3 1]
;;   ;      [10 12] [1 14] [2 14] [3 14]}
;;  (part data :rset #{:a :b})
;;   ;==> #{{:a 1  :b 2}  {:a 1 :b 1}  {:a 2 :b 1}  {:a 3 :b 1}
;;   ;      {:a 10 :b 12} {:a 1 :b 14} {:a 2 :b 14} {:a 3 :b 14}}
;;  (part data :rset {:a :c, :b :d})
;;   ;==> #{{:c 1  :d 2}  {:c 1 :d 1}  {:c 2 :d 1}  {:c 3 :d 1}
;;   ;      {:c 10 :d 12} {:c 1 :d 14} {:c 2 :d 14} {:c 3 :d 14}}
;;
;; subd:
;;  (subd data :rows 0 :c)
;;   ;==> [3 4]
;;  (subd foo :position :z)
;;   ;==> #{#{-2 2} #{-4 4}}
;;  (subd foo :position :y)
;;   ;==> #{1 2}
;;  (subd data :rows all :a)
;;   ;==> [1 #{1 2 3} 10 #{1 2 3}]
;;  (subd data :rows all [:a :b])
;;   ;==> [[1 2] [#{1 2 3} 1] [10 12] [#{1 2 3} 14]]
;;  (subd data :rset :a)
;;   ;==> #{1 #{1 2 3} 10}
;;  (subd data :rset [:a :b])
;;   ;==> #{{:a 1 :b 2} {:a #{1 2 3} :b 1} {:a 10 :b #{12}} {:a #{1 2 3} :b 14}}
;;  (subd data :rset #{:a :b})
;;   ;==> #{{:a 1 :b 2} {:a #{1 2 3} :b 1} {:a 10 :b #{12}} {:a #{1 2 3} :b 14}}
;;
;; dict-reduce:
;;  (dict-reduce
(defrecord ^:private All [])
(def all
  "
  all is an object that can be used to address all parts of an object in the el and part functions.
  "
  (All.))
(defrecord ^:private Nought [])
(def nought
  "
  nought is an object that is used by nben's dictionary system to indicate that something is not
  found or should be deleted.
  "
  (Nought.))
(def- missing
  "
  missing is used internally by nben's dictionary system to test for missing values via the
  dict-get function. It should never leave the nben.util.data namespace.
  "
  (Object.))
(defrecord ^:private Del [])
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
  {:atom  {:dict-row?  (comp dict-row? deref)
           :dict-get   #(dict-get (deref %1) %2 %3)
           :dict-keys  #(dict-keys (deref %1))
           :dict-set   #(do (swap! %1 dict-set %2 %3) %1)
           :dict-unset #(do (swap! %1 dict-unset %2 %3) %1)
           :dict-key?  #(dict-key? (deref %1) %2)
           :dict-sub   #(do (swap! %1 dict-sub %2 %3 %4) %1)}
   :ref   {:dict-row?  (comp dict-row? deref)
           :dict-get   #(dict-get (deref %1) %2 %3)
           :dict-keys  #(dict-keys (deref %1))
           :dict-set   #(do (alter %1 dict-set %2 %3) %1)
           :dict-unset #(do (alter %1 dict-unset %2 %3) %1)
           :dict-key?  #(dict-key? (deref %1) %2)
           :dict-sub   #(do (alter %1 dict-sub %2 %3 %4) %1)}
   :vol   {:dict-row?  (comp dict-row? deref)
           :dict-get   #(dict-get (deref %1) %2 %3)
           :dict-keys  #(dict-keys (deref %1))
           :dict-set   #(do (swap! %1 dict-set %2 %3) %1)
           :dict-unset #(do (swap! %1 dict-unset %2 %3) %1)
           :dict-key?  #(dict-key? (deref %1) %2)
           :dict-sub   #(do (swap! %1 dict-sub %2 %3 %4) %1)}
   :deref {:dict-row?  (comp dict-row? deref)
           :dict-get   #(dict-get (deref %1) %2 %3)
           :dict-keys  #(dict-keys (deref %1))
           :dict-set   #(dict-set (deref %1) %2 %3)
           :dict-unset #(dict-unset (deref %1) %2)
           :dict-key?  #(dict-key? (deref %1) %2)
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
           :dict-sub   #(dict-sub (vec %1) %2 %3 %4)}
   :obj   {:dict-row?  constantly-false
           :dict-get   #(arg-err "attempt to get from non-dict-row")
           :dict-keys  #(arg-err "attempt to keys from non-dict-row")
           :dict-set   #(arg-err "attempt to set non non-dict-row")
           :dict-unset #(arg-err "attempt to unset non non-dict-row")
           :dict-key?  constantly-nil
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
  (dict-key?  [_ k]       false)
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
           :doct-add  conj}
   :ref   {:dict-set? (comp dict-set deref)
           :dict-has? #(dict-has? (deref %1) %2)
           :dict-seq  #(dict-seq (deref %1))
           :dict-add  #(do (alter %1 dict-add %2) %1)
           :dict-drop #(do (alter %1 dict-drop %2) %1)}
   :atom  {:dict-set? (comp dict-set deref)
           :dict-has? #(dict-has? (deref %1) %2)
           :dict-seq  #(dict-seq (deref %1))
           :dict-add  #(do (swap! %1 dict-add %2) %1)
           :dict-drop #(do (swap! %1 dict-drop %2) %1)}
   :vol   {:dict-set? (comp dict-set deref)
           :dict-has? #(dict-has? (deref %1) %2)
           :dict-seq  #(dict-seq (deref %1))
           :dict-add  #(do (vswap! %1 dict-add %2) %1)
           :dict-drop #(do (vswap! %1 dict-drop %2) %1)}
   :deref {:dict-set? (comp dict-set deref)
           :dict-has? #(dict-has? (deref %1) %2)
           :dict-seq  #(dict-seq (deref %1))
           :dict-add  #(dict-add (deref %1) %2)
           :dict-drop #(dict-drop (deref %1) %2)}
   :obj   {:dict-set? constantly-false
           :dict-has? #(arg-err "object is not a dictionary set")
           :dict-seq  #(arg-err "object is not a dictionary set")
           :dict-drop #(arg-err "object is not a dictionary set")
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
          :dict-view    #(set-view %2 %3 (seq %1))
          :dict-lens-of get
          :dict-ldrop   disj}
   :map  {:dict-lens?   constantly-true
          :dict-view    #(map-view %2 %3 (keys %1) (vals %1))
          :dict-lens-of get
          :dict-ldrop   dissoc}
   :seq  {:dict-lens?   constantly-true
          :dict-view    #(seq-view %2 %3 (seq %1))
          :dict-lens-of nth
          :dict-ldrop   #(dissoc (seqmap %1) %2)}
   :drow {:dict-lens?   constantly-true
          :dict-view    #(let [ks (dict-keys %1)] (map-view %2 %3 ks (map (partial dgeterr %1) ks)))
          :dict-lens-of dict-get
          :dict-ldrop   dict-unset}
   :dset {:dict-lens?   constantly-true
          :dict-view    #(set-view %2 %3 (dict-seq %1))
          :dict-lens-of #(if (dict-has? %1 %2) %2 %3)
          :dict-ldrop   dict-drop}
   :obj  {:dict-lens?   constantly-false
          :dict-view    #(%3 (dgeterr %2 %1))
          :dict-lens-of #(arg-err "lens-of queried of non-lens object " %1)
          :dict-ldrop   #(arg-err "lens-drop requested of non-lens object" %1)}}
  (dict-lens? [_] "
  (dict-lens? l) yields true if l is a valid dictionary lens object and false otherwise.
  ")
  (dict-view [_ d f] "
  (dict-view l d f) yields a view of the given dictionary d through the given lens l with the
    function f applied to sub-elements.
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
  (dict-view [_ d f] (dgeterr d nil))
  (dict-lens-of [_ k nf] nf)
  (dict-ldrop [_ k] nil)
  All
  (dict-lens? [_] true)
  (dict-view [_ d f] (dict-sub d all nil f))
  (dict-lens-of [_ k nf] k)
  (dict-ldrop [_ k] (arg-err "Cannot drop from an all index")))

;; #DerefView ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(declare at)
(defrecord DerefView [derefable lens-path]
  clojure.lang.IDeref
  (deref [_] (apply at (deref derefable) lens-path))
  ;; a View may be either a set or a row, depending on what it holds...
  PDictRow
  (dict-row? [this]         (dict-row? (deref this)))
  (dict-get  [this k nf]    (dict-get  (deref this) k nf))
  (dict-keys [this]         (dict-keys (deref this)))
  (dict-key? [this k]       (dict-key? (deref this) k))
  (dict-sub  [this ks nf f] (dict-sub (deref this) ks nf f))
  (dict-set  [this k v]
    (let [l0 (first lens-path), kk (dict-lens-of l0 k missing)]
      (if (identical? kk missing)
        (dict-key-err k)
        (let [newderef (dict-set derefable kk v)]
          (if (identical? newderef derefable) this (apply at newderef lens-path))))))
  (dict-unset [this k]
    (let [l0 (first lens-path), kk (dict-lens-of l0 k missing)]
      (when-not (identical? kk missing)
        (let [dnew (dict-unset derefable kk), lens-path (cons (dict-ldrop l0 kk) (next lens-path))]
          (if (identical? derefable dnew)
            (DerefView. dnew lens-path)
            (apply at dnew lens-path))))))
  PDictSet
  (dict-set? [this]   (dict-set? (deref this)))
  (dict-has? [this k] (dict-has? (deref this) k))
  (dict-seq  [this]   (dict-seq  (deref this)))
  (dict-drop [this k] (let [x (dict-drop derefable k)]
                        (if (identical? derefable x) this (apply at x lens-path))))
  (dict-add  [this k] (let [x (dict-add derefable k)]
                        (if (identical? derefable x) this (apply at x lens-path)))))
(defmethod print-method DerefView [v ^java.io.Writer w]
  (.write w (str "#at[" (.derefable v) (apply str (interleave (.lens-path v) (repeat " ")) "]"))))

;; #IndexedSet ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;#TODO
(def- indexed-set hash-set)
(defn- indexed-set? [_] false)

;; #at ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn at
  "
  (at d) yields d.
  (at d k) is equivalent to (dict-get d k) except that it throws an exception if the value is not
    found.
  (at d k1 k2...) is roughly equivalent to (reduce dict-get d [k1 k2...]) except that it leaves any
    dict-set type that it encounters as a set.
  "
  ([d] d)
  ([d & more]
   (if (derefable? d)
     (DerefView. d more)
     (let [k (first more), nk (next more)]
       (dict-view k d (if nk #(apply at % nk) identity))))))

;; the with function
(defn with-part
  "
  (with-part d keys... dval) yields a duplicate of the dictionary d but with the dict-subionary
    specified by the given keys replaced with the dict-sub dval. The dval dictionary must have a
    matching shape as the selected subdict, which will have a shape equivalent to (part d keys...).
    Note that dval may not fully specify all elements but instead specify all elements below the
    given level.

  Examples:
    (def data {:a {:b [4 6 8] :c [3 5 7]} :x {:y 1 :z 4}})
    (with-part data :a :c [0 2] [:x :y])
       ; ==> {:a {:b [4 6 8] :c [:x 5 y]} :x {:y 1 :z 4}}
    (with-part data :a [:c :b] all [[0 1 2] [3 4 5]])
       ; ==> {:a {:b [3 4 5] :c [0 1 2]} :x {:y 1 :z 4}}
    (with-part data :a [:c :b] all {:b [0 1 2], :c [3 4 5]})
       ; ==> {:a {:b [0 1 2] :c [3 4 5]} :x {:y 1 :z 4}}
  "
  [d k1 & more]
  nil)

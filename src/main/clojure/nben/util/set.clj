;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; set.clj, part of nben, a mathematics library for the JVM.
;; The nben.util.set namespace provides a few set-specific operations that provide small
;; optimizations on existing clojure ways of doing things.
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

(ns nben.util.set
  (:use nben.util.error)
  (:use nben.util.typedef))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Handy types for making sets out of keys of maps...
(def-set-type MapKeySet [map]
  (contains? [_ o] (contains? map o))
  (get [_ o] (let [me (find map o)] (when me (key me))))
  (count [_] (count map))
  (empty [_] (MapKeySet. {}))
  (seq [_] (keys map))
  (conj [_ o] (MapKeySet. (assoc map o o)))
  (disj [_ o] (MapKeySet. (dissoc map o)))
  (meta [_] (meta map))
  (with-meta [_ mta] (MapKeySet. (with-meta map mta))))
(def-set-type MapEntrySet [map]
  (contains? [_ o] (if (map-entry? o)
                     (let [v (get map o map)]
                       (cond (identical? v map) false
                             (not= v (val o)) false
                             :else true))
                     false))
  (get [_ o] (when (map-entry? o)
               (let [me (find map (key o))]
                 (when (and me (= (val me) (val o))) me))))
  (count [_] (count map))
  (empty [_] (MapEntrySet. {}))
  (seq [_] (seq map))
  (conj [_ o] (conj (set (seq map)) o))
  (disj [_ o] (disj (set (seq map)) o))
  (meta [_] (meta map))
  (with-meta [_ mta] (MapEntrySet. (with-meta map mta))))
(def-set-type RangeSet [start end inc meta-data]
  (contains? [_ o] (and (number? o) (< o end) (>= o start) (== 0 (mod (- o start) inc))))
  (get [self o] (when (contains? self o) o))
  (count [_] (+ (int (/ (- end start) inc)) (if (== 0 (mod (- end start) inc)) 0 1)))
  (empty [_] #{})
  (seq [_] (range start end inc))
  (conj [self o] (if (contains? self o) self (conj (set (range start end inc)))))
  (disj [self o] (if (contains? self o)
                   (cond (= o end) (RangeSet. start (- end inc) inc meta-data)
                         (= o start) (RangeSet. (+ start inc) end inc meta-data)
                         :else (disj (set (range start end inc)) o))
                   self))
  (meta [_] meta-data)
  (with-meta [_ mta] (RangeSet. start end inc mta)))
(defn range-set
  "(range-set start end inc) is equivalent to (set (range start end inc)) but runs in constant time
     and has constant-time lookup. If items are conj'ed to or disj'ed from the set such that it can
     no longer be represented as a simple range, then the return value is constructed from a
     reified version of the range.
   (range-set start end) is equivalent to (range-set start end 1).
   (range-set end) is equivalent to (range-set 0 end 1)."
  ([end] (RangeSet. 0 end 1 nil))
  ([start end] (RangeSet. start end 1 nil))
  ([start end inc] (RangeSet. start end inc nil)))
(defn key-set
  "(key-set map) yields a set of the keys in map. This is performed in constant time. The map
     parameter may be a map (in which case the keys are used) or a vector (in which case a range-set
     ise used). If map is neither of these, then nil is yielded. The object returned is of type
     MapKeySet or of type RangeSet."
  [map]
  (cond (map? map) (->MapKeySet map)
        (vector? map) (->RangeSet (count map))
        :else nil))
(defn entry-set
  "(entry-set map) yields a set of the entries in the given map. This is performed in constant time.
     The map parameter may be a map or it may be a vector. The object returned is of type
     MapEntrySet."
  [map]
  (->MapEntrySet map))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set functions for fast intersection, union, etc.
(defn set-isect
  "(set-isect a b...) yields the intersection of all of the sets given. This function should be
     faster than clojure's intersection function because it sorts the sets and performs the
     intersection optimally."
  [& sets]
  (let [sorted-sets (seq (sort-by count sets))]
    (reduce (fn [smaller larger]
              (loop [s (seq smaller), res smaller]
                (if s
                  (recur (next s)
                         (if (contains? larger (first s)) res (disj res (first s))))
                  res)))
            (first sorted-sets)
            (next sorted-sets))))

(defn set-union
  "(set-isect a b...) yields the intersection of all of the sets given. This function should be
     faster than clojure's intersection function because it sorts the sets and performs the
     intersection optimally."
  [& sets]
  (let [sorted-sets (seq (sort-by count sets))]
    (reduce (fn [smaller larger]
              (loop [s (seq smaller), res larger]
                (if s
                  (recur (next s) (conj res (first s)))
                  res)))
            (first sorted-sets)
            (next sorted-sets))))

(defn map-isect
  "(map-isect f a b...) yields the intersection of all of the maps (a, b...) given, using the value
     of (f u v) to determine the value of matching keys with values u and v. This function should be
     faster than clojure's merge function because it sorts the sets and performs the intersection
     optimally.
   (map-isect f) yields a curried function that "
  [fun & maps]
  (let [sorted-maps (seq (sort-by count maps))]
    (reduce (fn [smaller larger]
              (loop [s (seq smaller), res smaller]
                (if s
                  (recur (next s)
                         (let [[k v] (first s)
                               lval (get larger k larger)]
                           (if (identical? larger lval)
                             (dissoc res k)
                             (assoc res k (fun v lval)))))
                  res)))
            (first sorted-maps)
            (next sorted-maps))))

(defn map-union [fun & maps]
  (let [sorted-maps (seq (sort-by count maps))]
    (reduce (fn [smaller larger]
              (loop [s (seq smaller), res larger]
                (if s
                  (let [[k v] (first s)
                        lval (get res k res)]
                    (recur (next s)
                           (if (identical? res lval)
                             (assoc res k v)
                             (assoc res k (fun v lval)))))
                  res)))
            (first sorted-maps)
            (next sorted-maps))))

(defn isect
  "(isect set, set...) yields the intersection of all the given sets.
   (isect map1, map2...) yields the intersection of all the given maps by keys, with the final value
     of any collision taken from an unspecified map.
   (isect fn map1, map2...) yields the intersection of all the given maps by keys, with the final
     value of an collision taken from (fn value1 value2). Note that fn may not be a map for this
     to work.
   (isect fn) yields a curried version of isect that will automatically use fn for the first
     argument to map-isect."
  [& args]
  (when (not (empty? args))
    (let [f (first args)
          n (next args)]
      (cond (map? f) (apply map-isect (fn [x y] x) args)
            (set? f) (apply set-isect (map set args))
            (ifn? f) (if n
                       (if (map? (first n)) (apply map-isect args) (apply set-isect n))
                       (partial map-isect f))
            :else (apply set-isect (map set args))))))

(defn union
  "(union set, set...) yields the union of all the given sets.
   (union map1, map2...) yields the union of all the given maps by keys, with the final value
     of any collision taken from an unspecified map.
   (union fn map1, map2...) yields the union of all the given maps by keys, with the final
     value of an collision taken from (fn value1 value2). Note that fn may not be a map for this
     to work.
   (union fn) yields a curried version of union that will automatically use fn as the first argument
     to map-union."
  [& args]
  (when (not (empty? args))
    (let [f (first args)
          n (next args)]
      (cond (map? f) (apply map-union (fn [x y] x) args)
            (set? f) (apply set-union args)
            (ifn? f) (if n
                       (if (map? (first n)) (apply map-union args) (apply set-union n))
                       (partial map-union f))
            :else (apply set-union (map set args))))))

(defn outer
  "(outer f args...) yields a lazy seq of the function f applied to the individual members
     of the cartesian product of the collections passed as args."
  ([f] nil)
  ([f coll0 & colls]
     (if colls
       (for [s0 (seq coll0), s (apply outer list colls)]
         (apply f s0 s))
       (map f coll0))))

    
  

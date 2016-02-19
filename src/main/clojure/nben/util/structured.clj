;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; structured.clj, part of nben, a mathematics library for the JVM.
;; A few utilities for querying and modifying structured data such as nested maps.
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

(ns nben.util.structured
  (:use nben.util.error)
  (:use nben.util.set)
  (:require [clojure.core.incubator :refer [seqable? dissoc-in]]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Some handy functions for dealing with associative datasets

;; #ref? ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn ref?
  "(ref? r) yields true if r is an instance of clojure.lang.IRef and false otherwise."
  [m]
  (instance? clojure.lang.IRef m))

;; #named? ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn named?
  "(named? u) yields true if u is an instance of Named."
  [u]
  (instance? clojure.lang.Named u))


;; Accessor Functions Here! ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- get-one-el [m key nf]
  (let [el (if (coll? m) (get m key m) (arg-err "cannot get element " key " of object " m))]
    (if (identical? el m) nf el)))
(defn- part-one-key [start-res add-val descend m key nf]
  (loop [s (seq key), res (transient start-res)]
    (if s
      (recur (next s)
             (let [f (first s), el (get-one-el m f nf)]
               (add-val res f (descend el))))
      (persistent! res))))
(defn- part-key [m key rest nf]
  (if (ref? m)
    (part-key (deref m) key rest nf)
    (let [descend (if rest
                    (fn [mm] (if (identical? mm nf)
                               nf
                               (part-key mm (first rest) (next rest) nf)))
                    identity)]
      (if (and (coll? key) (not (set? key)) (not (map? key)))
        (part-one-key [] #(conj! %1 %3) descend m key nf)
        (descend (get-one-el m key nf))))))


(defn part-or
  "(part-or m not-found keys...) is identical to (part m keys...) except that instead of throwing an
     exception when a set of keys is not found in the data structure m, not-found is yielded.
   (part-or m not-found) yields a function f such that (f keys...) is equivalent to
     (part-or m not-found keys...).
   (part-or not-found) yields a function f such that (f m keys...) is equivalent to
     (part-or m not-found keys...) and (f m) is equivalent to (part-or m not-found)."
  ([m not-found & keys] (if keys (part-key m (first keys) (next keys) not-found) m))
  ([m not-found]        (fn [& keys] (apply part-or m not-found keys)))
  ([not-found]          (fn ([m & keys] (apply part-or m not-found keys))
                            ([m] (part-or m not-found)))))

(defn part
  "(part m keys...) yields the data element at the indices given by the sequence of keys.
   Keys may be individual keys, which will be looped up in the appropriate level usina assoc, or
   they may be sets, maps, or collections. Maps may be of the form {k1 v1, k2 v2, ...} such that
   each key ki is looked up using get at the appropriate level and mapped to the key vi in the
   returned map. A set key is interpreted as a map in which each key is mapped to itself. Any other
   collection yields a vector of the values referened by the keys in the collection, in seq order.

   Examples:
     (def data {:a {:b [4 6 8] :c [3 5 7]} :x {:y 1 :z 4}})
     (part data :a :* [0 1])
        ==> {:b [4 6] :c [3 5]}
     (part data :x [:z :y])
        ==> [4 1]"
  [m & keys]
  (if keys 
    (let [el (part-key m (first keys) (next keys) m)]
      (if (identical? m el)
        (throw (java.util.NoSuchElementException. "part: requested element address not found"))
        el))
    m))

(defn- edit-one-el [descend m key val]
  (assoc m key (descend (get m key) val)))
(defn- edit-one-key [descend m keys trs val]
  (loop [k keys, tr trs, res m]
    (if k
      (let [f (first k), t (first tr)]
        (recur (next k) (next tr) (edit-one-el descend res f (get val t))))
      res)))
(defn- edit-key [m key val krest]
  (if (ref? m)
    (let [mm (deref m), mmm (edit-key mm key val krest)]
      (if-not (identical? mm mmm) (ref-set m mmm))
      m)
    (let [descend (if krest
                    (fn [m v] (edit-key m (first krest) v (next krest)))
                    (fn [m v] (if (ref? m) (do (ref-set m v) m) v)))]
      (if (and (coll? key) (not (set? key)) (not (map? key)))
        (edit-one-key descend m (seq key) (range) val)
        (edit-one-el descend m key val)))))
(defn edit
  "(edit m keys... val) yields the data structure m after having been modified by updating the
     elements addressed by the sequence of keys to match the values in the val data.
   (edit m val) yields val if m is not a ref, or ref-set's m to val then yields m if m is a ref."
  [m key0 & more]
  (if more
    (edit-key m key0 (last more) (butlast more))
    (if (ref? m) (do (ref-set m key0) m) key0)))

(declare crop)
(defn- crop-map [m key nk]
  (let [el (get m key m)]
    (if (identical? m el)
      m
      (let [u (apply crop el nk)]
        (cond (identical? el u) m
              (and (map? el) (empty? u)) (dissoc m key)
              :else (assoc m key u))))))
(defn- crop-set [m key nk]
  (if-not (contains? m key)
    m
    (let [u (apply crop key nk)]
      (cond (identical? u key) m
            (and (map? key) (empty? u)) (disj m key)
            :else (conj (disj m key) u)))))
(defn- crop-seq [m key nk]
  (let [m (vec m), el (get m key m)]
    (if (identical? m el)
      m
      (let [u (apply crop el nk)]
        (if (identical? u key) m (assoc m key u))))))
(defn crop
  "(crop m keys...) crops all of the given keys out of the data structure m and yeilds the updated
     structure."
  [m & keys]
  (cond (nil? keys) m
        (ref? m) (let [mm (deref m), mmm (apply crop mm keys)]
                   (if (not (identical? mm mmm)) (ref-set m mmm))
                   m)
        :else
        (let [key (first keys), nk (next keys)]
          (if (and (seqable? key)
                   (not (or (nil? key) (map? key) (set? key))))
            (cond (map? m)     (reduce #(crop-map %1 %2 nk) m key)
                  (set? m)     (reduce #(crop-set %1 %2 nk) m key)
                  (seqable? m) (reduce #(crop-seq %1 %2 nk) m key)
                  :else (arg-err "Cannot crop nonexistant part of argument " m))
            (if nk
              (cond (map? m)     (crop-map m key nk)
                    (set? m)     (crop-set m key nk)
                    (seqable? m) (crop-seq m key nk)
                    :else (arg-err "Cannot crop nonexistant part of argument " m))
              (cond (map? m) (dissoc m key)
                    (set? m) (disj m key)
                    (nil? m) m
                    (vector? m) (assoc m key nil)
                    (seqable? m) (assoc (vec (seq m)) key nil)
                    :else (arg-err "Cannot crop nonexistant part of argument " m)))))))



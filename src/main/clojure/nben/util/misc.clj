;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; misc.clj, part of nben, a mathematics library for the JVM.
;; A collection of miscellaneous utilities.
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

(ns nben.util.misc
  (:use potemkin))

;; #def- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro def-
  "(def- args...) is equivalent to (def ^:private args...); similar to defn- but for def."
  [symbol & args]
  `(def ^:private ~symbol ~@args))

;; #import-all-vars ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro import-all-vars
  "(import-all-vars namespace args...) imports all variables from the given namespace into the
     current namespace then yields nil. See also import-vars."
  [namespace & args]
  (let [{:keys [exclude only]} args
        exclude (set (map clojure.core/name exclude))
        only (set (map clojure.core/name only))
        filt (cond (and (empty? exclude) (empty? only)) (constantly true)
                   (empty? exclude) only
                   (empty? only) (complement exclude)
                   :else #(and (only %) (not (exclude %))))]
  `(potemkin/import-vars [~namespace ~@(filter (comp filt clojure.core/name)
                                               (map key (ns-publics (the-ns namespace))))])))

;; #string-drop ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn string-drop
  "
  (string-drop s i) drops the i'th character from the string s.
  (string-drop s i j) drops the substring from i to j in string s.
  "
  ([s i]   (str (subs s 0 i) (subs s (inc i))))
  ([s i j] (str (subs s 0 (dec i)) (subs s j))))

;; #string-insert ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn string-insert
  "(string-insert s i t) inserts the string t starting at position i."
  [s i t]   (str (subs s 0 i) t (subs s i)))

;; utility functions that are occasionally useful
(def constantly-true      (constantly true))
(def constantly-false     (constantly false))
(def constantly-nil       (constantly nil))
(def constantly-1         (constantly 1))
(def constantly-0         (constantly 0))
(def constantly-empty-set (constantly #{}))
(def constantly-empty-map (constantly {}))
(def constantly-empty-vec (constantly []))
(defn yield-first-argument [arg & more] arg)
(defn yield-last-argument  [& args] (last args))

;; #most ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn most
  "
  (most x) is equivalent to (butlast x) with the following exceptions:
    * If x is a non-empty vector, yields (pop x)
    * If x is a non-empty set or map, equivalent to (dissoc x (last x)) or (disj x (last x))--i.e.,
      it yields an object of the same type as x.
    * If x is a list or other seq'able item, this is equivalent to (butlast x) except that it yields
      a lazy seq instead of a realized seq.
    * Note that (most [y]) yields [], (most {a b}) yields {}, and (most #{y}) yields #{}; otherwise,
        if x has only one element and is neither a vector, map, or set (therefore must be a seqable
        object) nil is yielded.
  "
  [x]
  (cond (empty? x)  nil
        (vector? x) (pop x)
        (map? x)    (dissoc x (last (keys x)))
        (set? x)    (disj x (last x))
        :else       (let [x (seq x), fx (first x), nx (next x)]
                      (when nx (lazy-seq (cons fx (most nx)))))))

;; #most-last ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn most-last
  "
  (most-last x) is equivalent to [(most x) (last x)] except  that it is never lazy. If x is empty,
    yields nil.
  "
  [x]
  (cond (empty? x)  nil
        (vector? x) [(pop x) (last x)]
        (map? x)    (let [l (last x)] [(dissoc x (key l)) l])
        (set? x)    (let [l (last x)] [(disj x l) l])
        :else       (when-let [x (seq x)]
                      (let [v (volatile! nil)]
                        (letfn [(op [x] (let [fx (first x), x (next x)]
                                          (if x
                                            (lazy-seq (cons fx (op x)))
                                            (do (vreset! v fx) nil))))]
                          (let [x (op (seq x))]
                            (doall x)
                            [x @v]))))))

;; #>> ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn >>
  "
  (>> f x) is equivalent to (f x).
  (>> f g x) is equivalent to (f (g x)).
  (>> f g h... x) is equivalent to ((comp f g h...) x).
  (>> [f g h...] x y z...) is equivalent to ((comp f g h...) x y z...).
  "
  ([f1 f2 & more]
   (cond (or (seq? f1) (vector? f1)) (apply (apply comp f1) (cons f2 more))
         (empty? more)               (f1 f2)
         :else                       (let [[f x] (most-last (list* f1 f2 more)), f (apply comp f)]
                                       (f x)))))

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




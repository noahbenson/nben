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
  (:use potemkin)
  (:require [clojure.core.incubator :refer [seqable? dissoc-in]]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Some handy functions for dealing with associative datasets

;; #ref? ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn ref?
  "(ref? r) yields true if r is an instance of clojure.lang.Ref and false otherwise."
  [m]
  (instance? clojure.lang.Ref m))

;; #iref? ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn iref?
  "(iref? r) yields true if r is an instance of clojure.lang.IRef and false otherwise."
  [m]
  (instance? clojure.lang.IRef m))

;; #atom? ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn atom?
  "
  (atom? x) yields true if x is of type clojure.lang.Atom and false otherwise.
  "
  [x] (instance? clojure.lang.Atom x))

;; #agent? ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn agent?
  "(agent? r) yields true if r is an instance of clojure.lang.Agent and false otherwise."
  [m]
  (instance? clojure.lang.Agent m))

;; #named? ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn named?
  "(named? u) yields true if u is an instance of Named."
  [u]
  (instance? clojure.lang.Named u))

;; #derefable? ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn derefable?
  "
  (derefable? obj) yields true if obj is a type that can be safely dereferenced (i.e., a promise,
    ref, agent, delay, atom, or future).
  "
  [obj]
  (instance? clojure.lang.IDeref obj))

;; #transient? ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn transient?
  "
  (transient? x) yields true if x is an instance of the ITransientCollection interface and false
    otherwise.
  "
  [x]
  (instance? clojure.lang.ITransientCollection x))

;; #meta-data? ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn meta?
  "
  (meta? x) yields true if x is an instance of IMeta and false otherwise. See also with-meta?.
  "
  [x]
  (instance? clojure.lang.IMeta x))

;; #meta? ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn with-meta?
  "
  (with-meta? x) yields true if x is an instance of IObj and false otherwise.
  "
  [x]
  (instance? clojure.lang.IObj x))


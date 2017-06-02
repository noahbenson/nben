;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; util.clj, part of nben, a mathematics library for the JVM.
;; This namespace imports a large set of useful functions from various libraries and source files
;; and gives them all unique names that are conveniently non-conflicting with clojure's core.
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

(ns nben.util
  (:refer-clojure :exclude [second parents extend name minus])
  
  (:use [potemkin :exclude [def-map-type import-fn]])

  (:require nben.util.error)
  (:require nben.util.typedef)
  (:require nben.util.set)
  (:require nben.util.iterator)
  (:require nben.util.structured)
  (:require nben.util.misc)

  (:require clojure.string)
  (:require clojure.data.priority-map)
  (:require clojure.data.finger-tree)
  (:require jordanlewis.data.union-find))


;; Import the misc. utilities so that we have import-all-vars.
(nben.util.misc/import-all-vars nben.util.misc)

;; --> potemkin ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(import-vars [potemkin def-map-type import-fn])

;; --> incubator ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(import-vars [clojure.core.incubator seqable? dissoc-in])

;; --> string ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(import-vars [clojure.string capitalize lower-case re-quote-replacement split-lines trim
                             trim-newline triml trimr upper-case ends-with? starts-with?])
(import-fn clojure.string/blank?        whitespace?)
(import-fn clojure.string/escape        string-escape)
(import-fn clojure.string/join          string-join)
(import-fn clojure.string/replace       string-replace)
(import-fn clojure.string/replace-first string-replace-first)
(import-fn clojure.string/reverse       string-reverse)
(import-fn clojure.string/split         string-split)


;; --> priority-map ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(import-vars [clojure.data.priority-map priority-map priority-map-by])

;; --> finger-trees ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(import-vars [clojure.data.finger-tree
              double-list counted-double-list conjl
              counted-sorted-set
              finger-tree meter measured
              ft-concat ft-split-at])

;; --> int-map ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(import-vars [clojure.data.int-map int-map int-set dense-int-map dense-int-set])

;; --> union-find ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(import-fn jordanlewis.data.union-find/union-find clusters)
(import-fn jordanlewis.data.union-find/get-canonical clusters-find)
(import-fn jordanlewis.data.union-find/union clusters-union)

;; --> error ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(import-vars [nben.util.error arg-err state-err arithmetic-err unsupported-err arity-err check])

;; --> typedef ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(import-vars [nben.util.typedef def-set-type def-vec-type defmultipro])

;; --> set ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(import-vars [nben.util.set key-set entry-set range-set union isect map-union map-isect set-union 
                            set-isect outer])
(import-vars [clojure.set map-invert])

;; --> iterator ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(import-vars [nben.util.iterator iterator for-all? for-any? for-none? for-not-all? build sum
                                 product])

;; --> structured ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(import-all-vars nben.util.structured)


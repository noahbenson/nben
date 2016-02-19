;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; error.clj, part of nben, a mathematics library for the JVM.
;; This namespace provides a few handy functions for throwing simple errors smoothly.
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

(ns nben.util.error)

(defn arg-err
  "(arg-err text...) is equivalent to (throw (IllegalArgumentException. (apply str text)))."
  [& text]
  (throw (IllegalArgumentException. (apply str text))))

(defn state-err
  "(state-err text...) is equivalent to (throw (IllegalStateException. (apply str text)))."
  [& text]
  (throw (IllegalStateException. (apply str text))))

(defn arithmetic-err
  "(arithmetic-err text...) is equivalent to (throw (ArithmeticException. (apply str text)))."
  [& text]
  (throw (ArithmeticException. (apply str text))))

(defn unsupported-err
  "(unsupported-err text...) is equivalent to
     (throw (UnsupportedOperationException. (apply str text)))."
  [& text]
  (throw (UnsupportedOperationException. (apply str text))))

(defn arity-err
  "(arity-err obj n) throws an arity exception for an incorrect call of n arguments to the function
   object obj."
  [obj n]
  (throw (clojure.lang.ArityException. n (str (class obj)))))

(defmacro check
  "(check value tests...) yields value if all of the given tests are true when the symbol _ is
     assigned its value. This is a macro that essentially expands to:
     (let [_ value] (if (and tests...) value error)).
   The following options may also be given:
     :yield <v> instructs the macro to yield the given value v instead of value
     :symbol <s> instructs the macto to use the symbol <s> instead of _ to hold the value
     :with <sym> <val> instructs the macro to additionally bind the given symbol to the given value
       when evaluating the tests and yields clauses; multiple of these may be given and are bound in
       the order given
   Examples:
     * (/ x (check (cos y) (not= _ 0)))"
  [value & more]
  ;; first check for args
  (loop [sym '_, yie value, withs [], tests [], s more]
    (if s
      (let [f (first s), n (next s)]
        (cond (= f :symbol) (if n
                              (recur (first n) yie withs tests (next n))
                              (arg-err "check given a trailing :symbol option"))
              (= f :yield)  (if n
                              (recur sym (first n) withs tests (next n))
                              (arg-err "check given a trailing :yield option"))
              (= f :with)   (if n
                              (let [fn (first n), nn (next n)]
                                (if nn
                                  (recur sym yie (-> withs (conj fn) (conj (first nn))) tests (next nn))
                                  (arg-err "check given a :with with only one arg")))
                              (arg-err "check given a trailing :with option"))
              :else         (recur sym yie withs (conj tests f) n)))
      `(let [~sym ~value, ~@withs]
         (and ~@(map (fn [test] `(or ~test (throw (AssertionError. (quote ~test))))) tests)
              ~(if (identical? yie value) sym yie))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; iterator.clj, part of nben, a mathematics library for the JVM.
;; This namespace provides a versatile alternative to range and a few handy for-like macros.
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

(ns nben.util.iterator
  (use nben.util.error)
  (use nben.util.typedef)
  (use clojure.core.incubator))

;; #iterator ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn iterator
  "(iterator) yields a lazy seq of the whole numbers.
   (iterator max) yields a lazy seq of the whole numbers that stops before reaching max. If max is
     less than or equal to zero, then yields nil.
   (iterator min max) yields a lazy seq of numbers starting with min and ending before reaching max.
     If max is less than or equal to min, yields nil.
   (iterator min max step-size) yields a lazy seq starting at min and proceeding by steps of
     step-size and halting before reaching max. If max is :infinity or :-infinity, these values are
     accepted to create infinite sequences. If min + k * step-size == max such that k is less than
     or equal to zero, then yields nil.
   (iterator [a b ...]) yields (iterator a b ...).

   In addition to being able to specify iterators using the above syntax, the following keys may be
   placed anywhere in the argument list and will cause the remaining arguments to be interpreted as
   if their argument was specified in its correct place:
     min:
      :from min is equivalen to specifying the min
     max:
      :to max is equivalent to specifying the max
      :through max is equivalent to specifying that the seq should stop after max but include max,
        assuming that max falls in the series.
     step-size
      :by step-size is equivalent to specifying the step-size.
      :in steps guarantees that it will traverse from min to max in steps even steps. If this is
        used with a :to specifier, an exception is thrown; otherwise, the max is taken to be an
        inclusive max.

   Examples:
     (iterator :by 2 14) => (0 2 4 6 8 10 12)
     (iterator 1 2 :through 11) => (1 3 5 7 9 11)
     (iterator 1 :in 5) => (0 0.25 0.5 0.75 1)"
  [& args]
  (loop [s args, min nil, max nil, step-size nil, inclusive nil, in-steps false, rest []]
    (if s
      (let [f (first s), n (next s)]
        (cond (= f :from)
              (cond (nil? n) (arg-err ":from found but no argument given")
                    min (arg-err "minimum specified more than once in iterator")
                    :else (recur (next n) (first n) max step-size inclusive in-steps rest))
              (= f :to)
              (cond (nil? n) (arg-err ":to found but no argument given")
                    max (arg-err "max specified more than once in iterator")
                    :else (recur (next n) min (first n) step-size false in-steps rest))
              (= f :through)
              (cond (nil? n) (arg-err ":through found but no argument given")
                    max (arg-err "max specified more than once in iterator")
                    :else (recur (next n) min (first n) step-size true in-steps rest))
              (= f :by)
              (cond (nil? n) (arg-err ":by found but no argument given")
                    step-size (arg-err "step-size specified more than once in iterator")
                    :else (recur (next n) min max (first n) inclusive false rest))
              (= f :in)
              (cond (nil? n) (arg-err ":in found but no argument given")
                    step-size (arg-err "step-size specified more than once in iterator")
                    :else (recur (next n) min max (first n) inclusive true rest))
              :else (recur n min max step-size inclusive in-steps (conj rest f))))
      (let [specified (+ (count rest) (if min 1 0) (if max 1 0) (if step-size 1 0))
            implicit (count rest)]
        (if (> specified 3)
          (arg-err "too many arguments given to iterator")
          (let [start    (cond min min
                               (empty? rest) 0
                               max (first rest)
                               (> implicit 1) (fnext rest)
                               :else 0)
                end      (cond max max
                               (empty? rest) :infinity
                               :else (first rest))
                step-arg (cond step-size step-size
                               (> implicit (+ (if max 1 0) (if min 1 0))) (last rest)
                               :else 1)]
            (cond
             (== step-arg 0) (arg-err "cannot iterate in or by 0")
             in-steps (cond (= end :infinity) (arg-err "Cannot iterate to infinity in finite steps")
                            (false? inclusive) (arg-err "Cannot evenly iterate exclusive interval")
                            (not (integer? step-arg)) (arg-err "number of steps must be an int")
                            (< step-arg 1) (arg-err "cannot traverse two bounds in negative steps")
                            :else (let [stepsz (/ (- end start) (dec step-arg))]
                                    (map (fn [i] (+ start (* stepsz i))) (range step-arg))))
             :else (cond (= end :infinity) (cond
                                            inclusive (arg-err "iterator cannot include infinity")
                                            (== step-arg 1) (range start)
                                            (< step-arg 0) nil
                                            :else (map #(+ start (* step-arg %)) (range)))
                         (= end :-infinity) (cond
                                             inclusive (arg-err "iterator cannot include -infinity")
                                             (> step-arg 0) nil
                                             :else (map #(+ start (* step-arg %)) (range)))
                         inclusive (let [steps (/ (- end start) step-arg)]
                                     (if (== steps (int steps))
                                       (map #(+ start (* step-arg %)) (range (inc steps)))
                                       (range start end step-arg)))
                         :else (range start end step-arg)))))))))

(defmacro for-all?
  "(for-all? [bindings...] expression) yields true if expression yields a truthy value for each
     binding. The bindings... and syntax generally are identical to that of a for macro. As soon as
     a false or nil is yielded by a bound expression, the for-all yields false; otherwise, it
     evaluates all such bindings and yields true.
   (for-all? list) yields true if each element of the list is truthy; consumes only as much of the
     list as is necessary. Note that (for-all? nil) and (for-all? []) both yield true.
   (for-all?) yields true."
  ([] true)
  ([sequence]
     `(loop [s# (seq ~sequence)]
        (cond (nil? s#) true
              (first s#) (recur (next s#))
              :else false)))
  ([bindings expression]
     `(loop [s# (seq (for ~bindings ~expression))]
        (cond (nil? s#) true
              (first s#) (recur (next s#))
              :else false))))

(defmacro for-any?
  "(for-any? [bindings...] expression) yields true if any expression yields a truthy value for all
     bindings. The bindings... and syntax generally are identical to that of a for macro. As soon as
     a truthy value is yielded by a bound expression, the forany yields true; otherwise, it
     evaluates all such bindings and yields false.
   (for-any? s) yields true if any element of (seq s) is truthy; consumes only as much of the
     seq as is necessary. Note that (for-any? nil) and (for-any? []) both yield false.
   (for-any?) yields false."
  ([] false)
  ([sequence]
     `(loop [s# (seq ~sequence)]
        (cond (nil? s#) false
              (first s#) true
              :else (recur (next s#)))))
  ([bindings expression]
     `(loop [s# (seq (for ~bindings ~expression))]
        (cond (nil? s#) false
              (first s#) true
              :else (recur (next s#))))))

(defmacro for-none?
  "(for-none? [bindings...] expression) yields true if no expression yields a truthy value for all
     bindings. The bindings... and syntax generally are identical to that of a for macro. As soon as
     a truthy value is yielded by a bound expression, the fornone yields false; otherwise, it
     evaluates all such bindings and yields true.
   (for-none? s) yields true if no element of (seq s) is truthy; consumes only as much of the
     seq as is necessary. Note that (for-none? nil) and (for-none? []) both yield true.
   (for-none?) yields true."
  ([] true)
  ([sequence]
     `(loop [s# (seq ~sequence)]
        (cond (nil? s#) true
              (first s#) false
              :else (recur (next s#)))))
  ([bindings expression]
     `(loop [s# (seq (for ~bindings ~expression))]
        (cond (nil? s#) true
              (first s#) false
              :else (recur (next s#))))))

(defmacro for-not-all?
  "(for-not-all? [bindings...] expression) yields true if at least one expression does not yields a
     truthy value over all bindings. The bindings... and syntax generally are identical to that of a
     for macro. As soon as a non-truthy value is yielded by a bound expression, the for-not-all yields
     true; otherwise, it evaluates all such bindings and yields false.
   (for-not-all? s) yields true if at least one expression of (seq s) is not truthy; consumes only as
     much of the seq as is necessary. Note that (for-not-all? nil) and (for-not-all? []) both yield
     false.
   (for-not-all?) yields false."
  ([] false)
  ([sequence]
     `(loop [s# (seq ~sequence)]
        (cond (nil? s#) false
              (first s#) (recur (next s#))
              :else true)))
  ([bindings expression]
     `(loop [s# (seq (for ~bindings ~expression))]
        (cond (nil? s#) false
              (first s#) (recur (next s#))
              :else true))))

(defn- invoke-reversed
  ([f] (f))
  ([x f] (f x))
  ([x1 x2 & more] (let [r (reverse (concat [x1 x2] more))] (apply (first r) (rest r)))))
(defmacro build
  "(build sym init-val [bindings...] expr) yields the result of calling expr with the given
     bindings (expressed as in a for statement); after each evaluation of expr, the result is bound
     to the symbol sym, which is initially bound to init-val. The build macro is to the reduce
     function as the for macro is to the map function.
   (build sym [bindings...] expr) is equivalent to (build sym nil [bindings...] expr)."
  ([sym init-val bindings expr]
     `(let [box# (volatile! ~init-val)]
        (doseq ~bindings (vreset! box# (let [~sym @box#] ~expr)))
        @box#))
     ;`(reduce invoke-reversed ~init-val (for ~bindings (fn [~sym] ~expr))))
  ([sym bindings expr] `(build ~sym nil ~bindings ~expr)))

(defmacro sum
  "(sum [bindings...] expr) yields the sum of the expr expression when bound over the given
     bindings. The (sum args...) macro is essentially equivalent to (reduce + 0 (for args...)).
   (sum s) is equivalent to (reduce + 0 s)."
  ([bindings expr] `(reduce + 0 (for ~bindings ~expr)))
  ([s] `(reduce + 0 (seq ~s))))

(defmacro product
  "(product [bindings...] expr) yields the product of the expr expression when bound over the given
     bindings. The (expr args...) macro is essentially equivalent to (reduce * 1 (for args...)).
   (product s) is equivalent to (reduce * 1 s)."
  ([bindings expr] `(reduce * 1 (for ~bindings ~expr)))
  ([s] `(reduce * 1 (seq ~s))))

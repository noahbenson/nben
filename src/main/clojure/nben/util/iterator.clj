;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; iterator.clj, part of nben, a mathematics library for clojure.
;; This file defines the clojure functions for systematic handling of the generation and parsing of f
;; iterator forms given to functions like sum.
;; 
;; Copyright (C) 2012 Noah C. Benson
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

(ns nben.util.iterator)

(defn- arg-err [& msgs] (throw (IllegalArgumentException. (apply str msgs))))

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
      (let [specified (+ (count rest) (if min 1 0) (if max 1 0) (if step-size 1 0))]
        (if (> specified 3)
          (arg-err "too many arguments given to iterator")
          (let [start (or min (if (> specified 1) (first rest) 0))
                end (or max (case specified 0 :infinity 1 (nth rest 0)
                                  (if min (first rest) (nth rest 1))))
                step-arg (or step-size (if (< specified 3) 1 (last rest)))]
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



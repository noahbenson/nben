;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; math.clj, part of nben, a mathematics library for clojure.
;; This file defines the clojure functions for handling scalars and tensors; these are intended as a 
;; set of minimal functions that can be used to operate efficiently with any tensor format. Ideally, 
;; any format can extend this protocol and have a ready tensor class that will operate optimally 
;; with all nben math functions.
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

(ns nben.math
  (:use nben.util.iterator))

;;
;; These are just for internal error use
;;
(defn- arg-err [txt] (throw (IllegalArgumentException. txt)))
(defn- not-tensor-err [&{:keys [arg-name fn-name] :or {arg-name "<unknown>" fn-name "<unknown>"}}]
  (throw (IllegalArgumentException.
          (str "Argument " arg-name " to function " fn-name " is not a tensor"))))
(defn- not-complex-err [&{:keys [arg-name fn-name] :or {arg-name "<unknown>" fn-name "<unknown>"}}]
  (throw (IllegalArgumentException.
          (str "Argument " arg-name " to function " fn-name " is not a complex number"))))
(defn- not-real-err [&{:keys [arg-name fn-name] :or {arg-name "<unknown>" fn-name "<unknown>"}}]
  (throw (IllegalArgumentException.
          (str "Argument " arg-name " to function " fn-name " is not a real number"))))
(defn- not-rational-err [&{:keys [arg-name fn-name] :or {arg-name "<unknown>" fn-name "<unknown>"}}]
  (throw (IllegalArgumentException.
          (str "Argument " arg-name " to function " fn-name " is not a rational number"))))
(defn- not-integer-err [arg-name fn-name]
  (throw (IllegalArgumentException.
          (str "Argument " arg-name " to function " fn-name " is not an integer"))))
(defn- not-natural-err [arg-name fn-name]
  (throw (IllegalArgumentException.
          (str "Argument " arg-name " to function " fn-name " is not a natural number"))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Section 1. Definitions of the Protocols that Define the Mathematical Types
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Section 1.1. The tensor protocol
;; #tensor
(defprotocol tensor
  "The tensor is a protocol requires a minimal number of functions for performing tensor operations
   in conjunction with the nben clojure library. Anything that implements all of these functions
   will work optimally with the library."

  ;; First: the functions that determine what this is and how well it runs
  (tensor? [self]
    "(tensor? u) yields true if u is a tensor and false if it is not.")
  (tensor-time-profile [self]
    "(tensor-time-profile u yields the performance time profile of the tensor u. This profile must
       be a map of the time requirements for each operation it supports. Any operation that is not a
       key in the map or that maps to nil is assumed by the nben library to be unsupported. The keys
       to the profile should be the symbols of the tensor functions themselves, e.g.
       'nben.math.tensor.el or 'nben.math.tensor.normal. The values may be:
         :constant => a single call runs in O(1) time
         :logarithmic => a single call runs in O(log n) time
         :linear => a single call runs in O(n) time
         :quadratic => a single call runs in O(n^2) time
         :polynomial => a single call runs in O(n^a) such that a > 2
         :exponential => a single call runs in O(2^n) time
         nil => operation not supported
       As a general rule, the nben clojure math library will consider anything worse than
       quadratic unsuported.")
  
  ;; Next: functions for querying the data in the tensor
  (length [self]
    "(length u) yields the length of the tensor u.")  
  (normal [self]
    "(normal u) yields a seq of the elements of u in order.")
  (el [self k]
    "(el u k) yields the kth element of tensor u if k is an integer greater than 0 and less than 
       length u).")

  ;; Last: the functions for mutating the tensor
  (prepend [self q]
    "(prepend u q) yields a tensor of u with q prepended to it.")
  (append [self q]
    "(append u q) yields a tensor of u with q appended to it.")
  (replace-el [self k q]
    "(replace-el u k q) yields a tensor identical to u but with element k replaced by q."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Section 1.2. The complex protocol
;; #complex
(defprotocol complex
  "The complex-number protocol defines the functions for dealing with complex numbers."
  (complex? [self] "(complex? x) yields true if and only if x is a complex number.")
  (re [self] "(re z) yields the real part of the complex number z.")
  (im [self] "(im z) yields the imaginary part of the complex number z."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Section 1.3. The real protocol
(defprotocol real
  "The real protocol defines the functions for dealing with real numbers. This protocol requires
   only that real numbers ever be resolved to a finite precision."
  (real? [self] "(real? x) yields true if and only if x is a real number; otherwise false.")
  (estimate [self precision]
    "(estimate x precision) yields a Double or BigDecimal estimate of x whose error is less than
       precision, which must be positive."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Section 1.4. The rational protocol
;; #rational
(defprotocol rational
  "The rational protocol defines only the function for testing whether an object is a rational
   number or not. It may be extended to assert that a particular type is a rational number."
  (quotient? [self]
    "(quotient? x) yields true if and only if x is a rational number; otherwise it yields false."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Section 1.5. The integer protocol
(defprotocol integer
  "The integer protocol defines only the function for testing whether an object is an integer
   number or not. It may be extended to assert that a particular type is an integer."
  (int? [self]
    "(int? x) yields true if and only if x is an integer; otherwise it yields false."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Section 1.6. The natural protocol
;; #natural
(defprotocol natural
  "The natural protocol defines only the function for testing whether an object is a natural
   number or not. It may be extended to assert that a particular type is a natural number."
  (natural? [self]
    "(natural? x) yields true if and only if x is a natural number; otherwise it yields false."))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Section 2. Extensions of Protocols to Basic Types
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Section 2.1. Extension of the tensor protocol
;; #tensor
(extend-protocol tensor

  ;; Objects are not tensors unless they are complex numbers or one of the below types
  Object
  (tensor? [o] (complex? o))
  (tensor-time-profile [o]
    (when (complex? o) 
      {'length :constant 'normal :constant 'el :constant
       'prepend :constant 'append :constant 'replace-el :constant}))
  (length [o] (if (complex? o) nil (not-tensor-err o 'length)))
  (normal [o] (if (complex? o)
                (arg-err "Cannot normal a rank zero tensor")
                (not-tensor-err o 'normal)))
  (el [o k]
    (if (complex? o) 
      (arg-err "Cannot get element of rank zero tensor")
      (not-tensor-err o 'el)))
  (prepend [o q] (if (complex? o)
                   (arg-err "Cannot prepend to rank-zero tensor")
                   (not-tensor-err o 'prepend)))
  (append [o q] (if (complex? o)
                   (arg-err "Cannot append to rank-zero tensor")
                   (not-tensor-err o 'append)))
  (replace-el [o k q] (if (complex? o)
                        (arg-err "Cannot replace element of rank zero tensor")
                        (not-tensor-err o 'replace-el)))

  ;; But vectors are...
  clojure.lang.IPersistentVector
  (tensor? [o] true)
  (tensor-time-profile [o]
    {'length :constant
     'normal :constant
     'el :constant
     'prepend :constant
     'append :constant
     'replace-el :constant})
  (length [u] (.count u))
  (normal [u] (.seq u))
  (el [u k] (.nth u k))
  (prepend [u q] (.cons q u))
  (append [u q] (.conj u q))
  (replace-el [u k q] (.assocN u k q))

  ;; IPersistentCollections are mostly tensors
  clojure.lang.IPersistentCollection
  (tensor? [o] true)
  (tensor-time-profile [o]
    {'length :linear
     'normal :constant
     'el :linear
     'prepend :constant
     'append :linear
     'replace-el :linear})
  (length [u] (.count u))
  (normal [u] u)
  (el [u k] (nth u k))
  (prepend [u q] (.cons q u))
  (append [u q] (conj u q))
  (replace-el [u k q] (.assocN (vec u) k q))
  
  ;; Anything that implements Seqable is equally a tensor
  clojure.lang.Seqable
  (tensor? [o] true)
  (tensor-time-profile [o]
    {'length :linear
     'normal :constant
     'el :linear
     'prepend :constant
     'append :linear
     'replace-el :linear})
  (length [u] (length (.seq u)))
  (normal [u] (normal (.seq u)))
  (el [u k] (el (.seq u) k))
  (prepend [u q] (prepend (.seq u) q))
  (append [u q] (append (.seq u) q))
  (replace-el [u k q] (replace-el (.seq u) k q))

  ;; IPersistentSet's, however, are not tensors
  clojure.lang.IPersistentSet
  (tensor? [o] false)
  (tensor-time-profile [o] nil)
  (length [o] (not-tensor-err o 'length))
  (normal [o] (not-tensor-err o 'normal))
  (el [o k] (not-tensor-err o 'el))
  (prepend [o q] (not-tensor-err o 'prepend))
  (append [o q] (not-tensor-err o 'append))
  (replace-el [o k q] (not-tensor-err o 'replace-el))

  ;; IPersistentMap's may be (sparse) tensors, if properly formatted
  ;; Note that IPersistentMap's may also be complex numbers, thus must be checked as such
  clojure.lang.IPersistentMap
  (tensor? [o]
    (if-let [len (.valAt o :length)]
      (and (integer? len) (>= len 0))
      (complex? o)))
  (tensor-time-profile [o]
    (cond (tensor? o)
          {'length :logarithmic
           'normal :logarithmic
           'el :logarithmic
           'prepend :linear
           'append :logarithmic
           'replace-el :logarithmic}
          (complex? o)
          {'length :constant
           'normal :constant
           'el :constant
           'prepend :constant
           'append :constant
           'replace-el :constant}
          :else nil))
  (length [o]
    (let [len (.valAt o :length)]
      (if (and (integer? len) (>= len 0))
        len
        (if (complex? o) nil (not-tensor-err o 'length)))))
  (normal [o]
    (let [len (.valAt o :length)]
      (if (and (integer? len) (>= len 0))
        (let [dflt (.valAt o :default)]
          (letfn [(loopfn
                   ([k keys] (when (< k len)
                               (let [f (first keys)]
                                 (if (= k f)
                                   (cons (.valAt o k)
                                         (lazy-seq (loopfn (inc k) (next keys))))
                                   (cons dflt
                                         (lazy-seq (loopfn (inc k) keys))))))))]
            (loopfn 0 (sort (filter #(and (integer? %) (>= % 0)) (keys o))))))
        (if (complex? o)
          (arg-err "Cannot normal a rank zero tensor")
          (not-tensor-err o 'normal)))))
  (el [o k]
    (let [len (.valAt o :length)]
      (cond
       (or (not (integer? len)) (< len 0))
       (if (complex? o)
         (arg-err "Cannot get element of rank zero tensor")
         (not-tensor-err o 'el))
       (or (not (integer? k)) (< k 0)) (arg-err "argument k to el must be an integer > 0")
       (>= k len) (arg-err "argument " k " is greater than the length of tensor")
       :else (let [val (.valAt o k o)]
               (if (= o val) (.valAt o :default) val)))))
  (prepend [o q]
    (let [len (.valAt o :length)]
      (if (or (not (integer? len)) (< len 0))
        (if (complex? o) (arg-err "Cannot prepend to rank zero tensor") (not-tensor-err o 'prepend))
        (let [dflt (.valAt o :default)
              start {:default dflt :length (inc len)}]
          (loop [s (seq o),
                 res (transient (if (= q dflt) start (assoc start 0 q)))]
            (if (nil? s)
              (persistent! res)
              (let [[key val] (first s)]
                (if (and (integer? key) (>= key 0) (< key len))
                  (recur (next s) (assoc! res (inc key) val))
                  (recur (next s) res)))))))))
  (append [o q]
    (let [len (.valAt o :length)]
      (if (or (not (integer? len)) (< len 0))
        (if (complex? o) (arg-err "Cannot append to rank zero tensor") (not-tensor-err o 'append))
        (if (= (.valAt o :default) q)
          (assoc o :length (inc len))
          (assoc o :length (inc len) len q)))))
  (replace-el [o k q]
    (let [len (.valAt o :length)]
      (if (or (not (integer? len)) (< len 0))
        (if (complex? o)
          (arg-err "Cannot replace non-zeroth element of rank zero tensor")
          (not-tensor-err o 'replace-el))
        (if (= (.valAt o :default) q)
          (dissoc o k)
          (assoc o k q))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Section 2.2. Extension of the complex protocol
;; #complex
(extend-protocol complex

  ;; Objects are not complex numbers...
  Object
  (complex? [z] (real? z))
  (re [z] (if (real? z) z (not-complex-err z 're)))
  (im [z] (if (real? z) 0 (not-complex-err z 'im)))

  ;; All number types are complex numbers
  Number
  (complex? [x] true)
  (re [x] x)
  (im [x] 0)

  ;; Doubles and Floats need special attention for NaN's
  Double
  (complex? [x] (not (or (.isNaN x) (.isInfinite x))))
  (re [x] (if (or (.isNaN x) (.isInfinite x)) (arg-err "NaN argument is not a complex number") x))
  (im [x] (if (or (.isNaN x) (.isInfinite x)) (arg-err "NaN argument is not a complex number") 0))
  Float
  (complex? [x] (not (or (.isNaN x) (.isInfinite x))))
  (re [x] (if (or (.isNaN x) (.isInfinite x)) (arg-err "NaN argument is not a complex number") x))
  (im [x] (if (or (.isNaN x) (.isInfinite x)) (arg-err "NaN argument is not a complex number") 0))

  ;; The big types are also complex numbers
  java.math.BigInteger
  (complex? [x] true)
  (re [x] x)
  (im [x] 0)
  java.math.BigDecimal
  (complex? [x] true)
  (re [x] x)
  (im [x] 0)
  clojure.lang.BigInt
  (complex? [x] true)
  (re [x] x)
  (im [x] 0)

  ;; the real numbers are complex de facto
  nben.jvm.IRealNumber
  (complex? [r] true)
  (re [r] r)
  (im [r] 0)
  
  ;; Some maps are in fact complex numbers as well
  clojure.lang.IPersistentMap
  (complex? [m] (let [x (.valAt m :re), y (.valAt m :im)]
                  (and (real? x) (real? y)))
  (re [m] (let [x (.valAt m :re), y (.valAt m :im)]
            (if (and (real? x) (real? y)) x (not-complex-err m 're))))
  (im [m] (let [x (.valAt m :re), y (.valAt m :im)]
            (if (and (real? x) (real? y)) y (not-complex-err m 'im))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Section 2.3. Extension of the real protocol
;; #real
(extend-protocol real

  ;; not all objects are real...
  Object
  (real? [o] (quotient? o))
  (estimate [o p] (if (quotient? o) o (not-real-err o 'estimate)))

  ;; but all the numbers are...
  Number
  (real? [x] true)
  (estimate [x precision] x)

  ;; as well as the big numbers
  clojure.lang.BigInt
  (real? [x] true)
  (estimate [x precision] x)
  java.math.BigInteger
  (real? [x] true)
  (estimate [x precision] x)
  java.math.BigDecimal
  (real? [x] true)
  (estimate [x precision] x)

  ;; but not the nans...
  Double
  (real? [x] (not (or (.isNaN x) (.isInfinite x))))
  (estimate [x precision] (if (or (.isNaN x) (.isInfinite x))
                            (arg-err "NaN argument is not a real number")
                            x))
  Float
  (real? [x] (not (or (.isNaN x) (.isInfinite x))))
  (estimate [x precision] (if (or (.isNaN x) (.isInfinite x))
                            (arg-err "NaN argument is not a real number")
                            x))
  
  ;; Also, anything that implements IRealNumber is a real number...
  nben.jvm.IRealNumber
  (real? [r] true)
  (estimate [r precision] (.estimate r precision)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Section 2.4. Extension of the rational protocol
;; #rational
(extend-protocol rational

  ;; Objects may or may not be rational numbers
  Object (quotient? [o] (int? o))

  ;; All the number types are rational...
  Number (quotient? [q] true)
  ;; ...and the big numbers...
  clojure.lang.BigInt (quotient? [q] true)
  java.math.BigInteger (quotient? [q] true)
  java.math.BigDecimal (quotient? [q] true)
  ;; ...but not the nans
  Double (quotient? [q] (not (or (.isNan q) (.isInfinite q))))
  Float (quotient? [q] (not (or (.isNan q) (.isInfinite q))))

  ;; Also, the ratios are rational, as long as they aren't anything strange
  clojure.lang.Ratio (quotient? [q] (not (== (.demoninator q) 0))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Section 2.5. Extension of the integer protocol
;; #integer
(extend-protocol integer

  ;; Objects are integers if they're natural numbers...
  Object (int? [o] (natural? o))

  ;; Numbers we can check with clojure's mod
  Number (int? [k] (== (mod k 1) 0))
  ;; Integer types we can answer with more confidence though...
  Integer (int? [k] true)
  Short (int? [k] true)
  Long (int? [k] true)
  BigInteger (int? [k] true)
  Byte (int? [k] true)
  java.math.BigInteger (int? [k] true)
  clojure.lang.BigInt (int? [k] true)
  ;; big decimals we can also work with...
  java.math.BigDecimal (int? [k] (== (mod k 1) 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Section 2.6. Extension of the natural protocol
;; #natural
(extend-protocol natural

  ;; Objects are not natural numbers, as a general rule
  Object (natural? [n] false)

  ;; Numbers we can check with clojure's mod
  Number (int? [k] (and (> k 0) (== (mod k 1) 0)))
  ;; Integer types we can answer with more confidence though...
  Integer (int? [k] (> k 0))
  Short (int? [k] (> k 0))
  Long (int? [k] (> k 0))
  BigInteger (int? [k] (> k 0))
  Byte (int? [k] (> k 0))
  java.math.BigInteger (int? [k] (> k 0))
  clojure.lang.BigInt (int? [k] (> k 0))
  ;; big decimals we can also work with...
  java.math.BigDecimal (int? [k] (and (> k 0) (== (mod k 1) 0))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Section 3. Definition of the math type hierarchy
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def math-type-hierarchy
  "math-type-hierarchy is a hierarchy of the mathematical types supported by the core nben.math
   library. Generally speaking, the mathematical multimethods in nben.math dispatch on the math-type
   value of each argument (though this is not always true)."
  (-> (make-hierarchy)
      (derive ::tensor ::complex)
      (derive ::complex ::real)
      (derive ::real ::rational)
      (derive ::rational ::integer)
      (derive ::integer ::natural)))

(defprotocol supported-math-type
  "The supported-math-type protocol is a protocol that is designed to give the nben.math library a
   dispatch option for all types supported by the library. The only function, math-type, allows one
   to specify the lowest-level mathematical type for a particular class, thus slightly optimizing
   the dispatch of various of the library's multimethods."

  (math-type [self]
    "(math-type u) yields the scoped keyword corresponding to u's mathematical type, according to 
       the nben.math library. If u has no type, yields nil. This function will work correctly for 
       objects that have overloaded their lowest-level protocol without overloading the
       supported-math-type protocol, but will run more optimally if the supported-math-type
       protocol is also overloaded."))

(extend-protocol supported-math-type

  ;; Objects may be supported by virtue of extending another protocol, but we must check manually
  Object
  (math-type [o] (when (tensor? o)
                   (if (complex? o)
                     (if (real? o)
                       (if (quotient? o)
                         (if (int? o)
                           (if (natural? o)
                             ::natural
                             ::integer)
                           ::rational)
                         ::real)
                       ::complex)
                     ::tensor)))

  ;; Numbers we know much more about implicitly...
  Number
  (math-type [o] (if (int? o)
                   (if (natural? o) ::natural ::integer)
                   ::rational))
  ;; Especially integers and such
  Integer (math-type [k] (if (natural? k) ::natural ::integer))
  Short (math-type [k] (if (natural? k) ::natural ::integer))
  Long (math-type [k] (if (natural? k) ::natural ::integer))
  Byte (math-type [k] (if (natural? k) ::natural ::integer))
  java.math.BigInteger (math-type [k] (if (natural? k) ::natural ::integer))
  clojure.lang.BigInt (math-type [k] (if (natural? k) ::natural ::integer))
  ;; but we must avoid nan's still
  Double (math-type [q] (when (not (or (.isNan q) (.isInfinite q)))
                          (if (int? q) (if (natural? q) ::natural ::integer) ::rational)))
  Float (math-type [q] (when (not (or (.isNan q) (.isInfinite q)))
                         (if (int? q) (if (natural? q) ::natural ::integer) ::rational)))
  ;; and we can still support the big decimals
  java.math.BigDecimal (math-type [q] (if (int? q)
                                        (if (natural? q) ::natural ::integer)
                                        ::rational)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Section 4. General mathematical functions that use the above protocols
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Section 4.1. The Real Number functions
;; #real
(defmacro infinite-sum
  "(infinite-sum iterator term) yields a real number that is estimated by summing the terms formed
     by the term-form when bound under iterator. For information on the format of iterators see the
     documentation for nben.util.parse-iterator. In both the term-form and the error-form, the
     iterator variable is bound.

   Options:
     :error (default: :automatic) If :automatic, uses the absolute value of the term as the error
       and requires that all terms decrease monotonically in size. Otherwise, may be a function of
       three arguments, the iterator value, its new term, and the new sum including the new term; it
       must yield a positive rational number that decreases monotonically as the sequence proceeds.

   Examples:
     (infinite-sum [k :from 0] (/ 1 (factorial k))) => e"
  [[iterator-sym &{:keys [from by] :or {from 0 by 1}}] term &{:keys [error] :or {error :automatic}}]
  `(let [by-arg# ~by
         iter# (iterator :from ~from
                         :to (if (< by-arg# 0) :-infinity :infinity)
                         :by by-arg#)]
     (letfn [(next-step-fn# [~iterator-sym] (let [term# ~term] (cons )
        
   
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Section 4.1. Functions for arithmetic.
;; #arithmetic 

(defn plus
  "(plus) yields 0.
   (plus a) yields a.
   (plus a b) yields (+ a b) if a and b are not tensors, and the element-wise tensor sum of a and b
     if they are tensors of the same length. If a or b is a tensor, but not both, then yields the
     tensor whose elements are the sum of the scalar with each element of the tensor.
   (plus a1 a2 ... an) yields the sum of all items or tensors reduced using plus."
  ([] 0)
  ([a] a)
  ([a b] (cond (quotient? a)
               (cond (quotient? b) (+ a b)
                     (real? b) (
           (if (tensor? b)
             (if (= (length a) (length b))
               (map plus (normal a) (normal b))
               (arg-err "Cannot add tensors of unequal length (" (length a) " and " (lenth b)")"))
             (map #(plus % b) (normal a)))
           (if (tensor? b)
             (map #(plus % a) (normal b))
             (+ a b))))
  ([a b & more] (reduce plus (plus a b) more)))
(defn pplus
  "pplus is identical to plus except that it maps over tensor addition in parallel using pmap."
  ([a] a)
  ([a b] (if (tensor? a)
           (if (tensor? b)
             (if (= (length a) (length b))
               (pmap pplus (normal a) (normal b))
               (arg-err "Cannot add tensors of unequal length (" (length a) " and " (lenth b)")"))
             (pmap #(pplus % b) (normal a)))
           (if (tensor? b)
             (pmap #(pplus % a) (normal b))
             (+ a b))))
  ([a b & more] (reduce pplus (pplus a b) more)))

(defn subtract
  "(subtract a) yields a.
   (subtract a b) yields (- a b) if a and b are not tensors, and the element-wise tensor subtraction
     of a - b if they are tensors of the same length. If a or b is a tensor, but not both, then
     yields the tensor whose elements are the subtraction of the scalar and each element of the
     tensor.
   (subtract a1 a2 ... an) is equivalent to (subtract (subtract ... (subtract a1 a2) ...) an)."
  ([a] a)
  ([a b] (if (tensor? a)
           (if (tensor? b)
             (if (= (length a) (length b))
               (map subtract (normal a) (normal b))
               (arg-err "Cannot subtract tensors of unequal length ("
                        (length a) " and " (lenth b)")"))
             (map #(subtract % b) (normal a)))
           (if (tensor? b)
             (map #(subtract % a) (normal b))
             (- a b))))
  ([a b & more] (reduce subtract (subtract a b) more)))
(defn psubtract
  "psubtract is identical to plus except that it maps over tensor subtraction in parallel using pmap."
  ([a] a)
  ([a b] (if (tensor? a)
           (if (tensor? b)
             (if (= (length a) (length b))
               (pmap psubtract (normal a) (normal b))
               (arg-err "Cannot subtract tensors of unequal length ("
                        (length a) " and " (lenth b)")"))
             (pmap #(psubtract % b) (normal a)))
           (if (tensor? b)
             (pmap #(psubtract % a) (normal b))
             (- a b))))
  ([a b & more] (reduce psubtract (psubtract a b) more)))

(defn minus
  "(minus u) yields the opposite of tensor or number u."
  [u] (cond (number? u) (- u)
            (tensor? u) (map minus (normal u))
            :else (arg-err "Cannot calculate minus of non-tensor and non-number")))
(defn pminus
  "(pminus u) yields the opposite of tensor or number u, but maps over tensors in parallel."
  [u] (cond (number? u) (- u)
            (tensor? u) (pmap minus (normal u))
            :else (arg-err "Cannot calculate minus of non-tensor and non-number")))

(defn times
  "(times u) yields u.
   (times u v) yields the element-wise multiplication of tensors u and v if they are both tensors
     and the same length; if one is a scalar, yields the element-wise product of the scalar with
     the elements of the tensor; otherwise, yields (* u v).
   (times u1 u2 ... un) yields the product of all the given tensors or numbers."
  ([u] u)
  ([u v] (if (tensor? u)
           (if (tensor? v)
             (if (= (length u) (length v))
               (map times (normal u) (normal v))
               (arg-err "Cannot multiply tensors of unequal length (" (length u)
                        " and " (lenth v)")"))
             (map #(times % v) (normal u)))
           (if (tensor? v)
             (map #(times % u) (normal v))
             (* u v))))
  ([u v & more] (reduce times (times u v) more)))
(defn ptimes
  "ptimes is identical to times except that it maps in parallel over tensors using pmap."
  ([u] u)
  ([u v] (if (tensor? u)
           (if (tensor? v)
             (if (= (length u) (length v))
               (pmap ptimes (normal u) (normal v))
               (arg-err "Cannot multiply tensors of unequal length (" (length u)
                        " and " (lenth v)")"))
             (pmap #(ptimes % v) (normal u)))
           (if (tensor? v)
             (pmap #(ptimes % u) (normal v))
             (* u v))))
  ([u v & more] (reduce ptimes (ptimes u v) more)))

(defn divide
  "(divide u v) yields the element-wise division of tensors u and v if they are both tensors
     and the same length; if one is a scalar, yields the element-wise division of the scalar with
     the elements of the tensor; otherwise, yields (/ u v)."
  [u v] (if (tensor? u)
          (if (tensor? v)
            (if (= (length u) (length v))
              (map divide (normal u) (normal v))
              (arg-err "Cannot divide tensors of unequal length (" (length u)
                       " and " (lenth v)")"))
            (map #(divide % v) (normal u)))
          (if (tensor? v)
            (map #(divide % u) (normal v))
            (/ u v))))
(defn pdivide
  "pdivide is identical to divide except that it maps over tensors in parallel using pmap."
  [u v] (if (tensor? u)
          (if (tensor? v)
            (if (= (length u) (length v))
              (pmap pdivide (normal u) (normal v))
              (arg-err "Cannot divide tensors of unequal length (" (length u)
                       " and " (lenth v)")"))
            (pmap #(pdivide % v) (normal u)))
          (if (tensor? v)
            (pmap #(pdivide % u) (normal v))
            (/ u v))))

(defn power
  "(power u c) yields the number or tensor u raised (element-wise for tensors) to the c power."
  [u c] (if (tensor? u)
          (map #(power % c) (normal u))
          (Math/pow u c)))
(defn ppower
  "ppower is identical to power except that it maps over tensors in parallel."
  [u c] (if (tensor? u)
          (pmap #(ppower % c) (normal u))
          (Math/pow u c)))

(defn exp
  "(exp u) yields E raised to the the number or tensor (element-wise) u."
  [u] (if (tensor? u)
        (map exp (normal u))
        (Math/exp u)))
(defn pexp
  "pexp is identical to exp except that it maps in parallel over tensors using pmap."
  [u] (if (tensor? u)
        (pmap pexp (normal u))
        (Math/exp u)))

(defn sqrt
  "(sqrt u) yields the square root of the number u, or the tensor whose elements are the square
     roots of the elements in tensor u."
  [u] (if (tensor? u)
        (map sqrt (normal u))
        (Math/sqrt u)))
(defn psqrt
  "pexp is identical to exp except that it maps in parallel over tensors using pmap."
  [u] (if (tensor? u)
        (pmap pexp (normal u))
        (Math/sqrt u)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Section 4.2. Multimethods for low-level tensor operations.

;; #rank ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmulti rank
  "(rank u) yields the rank of tensor u."
  math-type :hierarchy math-types-hierarchy)



;; #index ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmulti index
  "(index u) yields a map of elements mapped to a set of all indices at which that element occurs
     in the tensor u.
   (index u label-fn) yields a map of elements labels mapped to a set of all indices at which 
     those elements occur. The element labels are determined by (label-fn element)
   (index u test-fn val) yields a lazy sequence of all indices for which (test-fn value) yields
     val. (index u = val) can be used to find a seq of all values that equal val.
   (indec u test-fn val1 val2 ...) yields a map of the val1...valn mapped to the indices at the
     elements at which (test-fn element) yields one of the vals."
  (fn [u & more] (math-type u))
  :hierarchy math-types-hierarchy)






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Section 5. General mathematicam multimethods that use the above protocols; defmethods
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; #rank ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod rank :default [u] (not-tensor-err u 'rank))
(defmethod rank ::complex [u] 0)
(defmethod rank ::tensor [u] (count (dimensions u)))

;; #index ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(letfn [(index-seq [s i test-fn val]
          (when s
            (let [f (test-fn (first s))]
              (if (= f val)
                (cons i (lazy-seq (index-seq (next s) (inc i) test-fn val)))
                (recur (next s) (inc i) test-fn val)))))]
  (defn index
    "(index u) yields a map of elements mapped to a set of all indices at which that element occurs
       in the tensor u.
     (index u label-fn) yields a map of elements labels mapped to a set of all indices at which 
       those elements occur. The element labels are determined by (label-fn element)
     (index u test-fn val) yields a lazy sequence of all indices for which (test-fn value) yields
       val. (index u = val) can be used to find a seq of all values that equal val.
     (indec u test-fn val1 val2 ...) yields a map of the val1...valn mapped to the indices at the
       elements at which (test-fn element) yields one of the vals."
    ([u]
       (loop [i 0, s (normal u), res (transient {})]
         (if s
           (let [f (first s)
                 cur (.valAt res f)]
             (recur (inc i) (next s) (assoc! res f (if cur (conj cur i) [i]))))
           (persistent! res))))
    ([u label-fn]
       (loop [i 0, s (normal u), res (transient {})]
         (if s
           (let [f (label-fn (first s))
                 cur (.valAt res f)]
             (recur (inc i) (next s) (assoc! res f (if cur (conj cur i) [i]))))
           (persistent! res))))
    ([u label-fn val] (index-seq (normal u) 0 label-fn val))
    ([u label-fn val1 & rest]
       (loop [i 0, s (normal u), res (reduce #(assoc! %1 %2 []) (transient {}) (cons val1 rest))]
         (if s
           (let [f (label-fn (first s))
                 cur (.valAt res f)]
             (if cur 
               (recur (inc i) (next s) (assoc! res f (if cur (conj cur i) [i])))
               (recur (inc i) (next s) res)))
           (persistent! res))))))

(defn tally
  "(tally u) yields a map of the elements of u to the number of times each element occurs in the
     tensor u. (tally u) is equivalent to (tally u =).
   (tally u test-fn) yields a map of, for each element e, (test-fn e) mapped to the number of times
     (test-fn e) occurs in the tensor u."
  ([u test-fn]
     (loop [s (normal u), res (transient {})]
       (if s
         (recur (next s) (let [f (first s)] (assoc! res f (inc (.valAt res f 0)))))
         (persistent! res))))
  ([u] (tally u =)))

(defn precedes?
  "(precedes? a b) yields true if and only if a comes before b in canonical order; for tensors,
     precedes? will traverse the tensors down to any non-tensor element and compare there.
     (precedes? a b) is equivalent to (precedes? < a b). All tensors are considered to come after
     all atoms in canonical order.
   (precedes? test-fn a b) performs precedes? using the ordering function test-fn."
  ([test-fn a b]
     (cond
      (not (tensor? a)) (if (tensor? b) true (test-fn a b))
      (not (tensor? b)) false
      :else (loop [sa (normal a), sb (normal b)]
              (cond (nil? sa) (not (nil? sb))
                    (nil? sb) false
                    :else (let [fa (first sa), fb (first sb)]
                            (if (tensor? fa)
                              (if (tensor? fb)
                                (cond (precedes? fa fb test-fn) true
                                      (precedes? fb fa test-fn) false
                                      :else (recur (next sa) (next sb)))
                                false)
                              (if (tensor? fb)
                                true
                                (cond (test-fn fa fb) true
                                      (test-fn fb fa) false
                                      :else (recur (next sa) (next sb))))))))))
  ([a b] (precedes? < a b)))

(defn eq?
  "(eq? u v) yields true if and only if either (= u v) or u and v are tensors the same length
     n such that for 0 <= i < n, (eq? (el u i) (el v i)).
   (eq? condition u v) yields true if and only if either (condition u v) or u and v are tensors the
     same length n such that for 0 <= i < n, (eq? condition (el u i) (el v i))."
  ([u v]
     (cond (= u v) true
           (not (tensor? u)) false
           (not (tensor? v)) false
           (not= (length u) (length v)) false
           :else (every? true? (map = (normal u) (normal v)))))
  ([condition u v]
     (cond (condition u v) true
           (not (tensor? u)) false
           (not (tensor? v)) false
           (not= (length u) (length v)) false
           :else (every? true? (map condition (normal u) (normal v))))))
  
(defn ordered?
  "(ordered? u) yields true if the elements of tensor u are in order. (ordered? u) is equivalent
     to (ordered? u precedes?).
   (ordered? u sorg-fn) yields true if the elements of tensor u are ordered according to
     (sort-fn u1 u2)."
  ([u sort-fn]
     (loop [s (next (normal u)), f (el u 0)]
       (if s
         (let [fn (first s)]
           (if (sort-fn f fn)
             (recur (next s) fn)
             false))
         true)))
  ([u] (ordered? u precedes?)))

(defn commonest
  "(commonest u) yields the commonest element of u; if more than one element is commonest, yields
     the one that occurs earliest in u.
   (commonest u n) yields the n commonest elements of u; in the case of ambiguities, the elements
     occuring earliest in the list are returned first."
  ([u] (loop [s (normal u), i 0, tally (transient {}), cur-best nil]
         (if s
           (let [f (first s), cur (.valAt tally f)]
             (if (nil? cur)
               (recur (next s) (inc i) (assoc! tally f [1 i]) (or cur-best [[-1 i] f]))
               (let [[count i0] cur, new-count (inc count),
                     new-el [new-count i0], new-cur [(- new-count) i0]]
                 (recur (next s) (inc i) (assoc! tally f new-el)
                        (if (precedes? (first cur-best) new-cur) cur-best [new-cur f])))))
           (fnext cur-best))))
  ([u n] (loop [s (normal u), i 0, tally (transient {})]
           (if s
             (let [f (first s), cur (.valAt tally f)]
               (if (nil? cur)
                 (recur (next s) (inc i) (assoc! tally f [-1 i]))
                 (let [[count i0] cur, new-count (dec count)]
                   (recur (next s) (inc i) (assoc! tally f [new-count i0])))))
             (map key (take n (sort #(precedes? (val %1) (val %2)) (persistent! tally))))))))

(defn ordering
  "(ordering u) yields a seq of the indices such that (sort u) is equivalent to
     (apply part (cons u (ordering u))).
   (ordering u k) yields the first k elements of (ordering u).
   (ordering comp-fn u) yields the indices as sorted by comp-fn.
   (ordering comp-fn u k) yields the first k indices as sorted by comp-fn."
  ([comp-fn u k] ;; need to find the k lowest elements as ordered by comp-fn
     (loop [s (normal u), i 0,
            r (sorted-set-by #(comp-fn (first %1) (first %2)))]
       (if s
         (recur (next s) (inc i)
                (if (and k (= (count r) k))
                  (disj (conj r [(first s) i]) (first (rseq r)))
                  (conj r [(first s) i])))
         (map fnext (seq r)))))
  ([arg1 arg2]
     (cond (and (tensor? arg1) (integer? arg2) (>= 0 arg2)) (ordering precedes? arg1 arg2)
           (and (ifn? arg1) (tensor? arg2)) (ordering arg1 arg2 nil)
           :else (arg-err "ordering requires a tensor and integer or function and tensor")))
  ([u] ;; full ordering is just a sorting problem
     (map fnext
          (sort #(precedes? (first %1) (first %2))
                (map list (normal u) (range))))))

;; this function performs one layer of the part function by using random access; ideal for vectors
(defn- part-random-access [u indices]
  (when indices
    (cons (el u (first indices))
          (lazy-seq (part-random-access u (next indices))))))

(defn part
  "(part u k) yields the kth element of tensor u.
   (part u k1 k2 ...) yields the k2th element of the k1th element of tensor u, etc.
   The k's may be tensors of indices, in which case the elements are returned as a lazy seq in
     order. E.g., (part u [1 2 3] 9) yields ((part u 1 9) (part u 2 9) (part u 3 9))."
  [u & indices]
  ;; u must be a tensor:
  (or (tensor? u) (not-tensor-err u 'part))
  ;; if there are no more indices, we yield u itself; otherwise, the elements indexed
  (if (nil? indices)
    u
    ;; we need to know how fast the tensor does random access to determine our algorithm
    (let [time-profile (tensor-time-profile u)
          random-access (get time-profile 'el)
          first-index (first indices)
          next-index (next indices)
          parts (cond (not (coll? first-index)) (el u first-index)
                      (or (= :constant random-access) (= :logarithmic random-access))
                      (part-random-access u first-index)
                      :else (part-random-access (vec (normal u)) first-index))]
      (cond (nil? next-index) parts
            (coll? first-index) (map #(apply part (cons % next-index)) parts)
            :else (apply part (cons parts next-index))))))

;; used by flatter and flattest to lazily walk and flatten tensors
(defn- flatten-walk [u k stack level]
  (cond (nil? u) (when stack (recur (first stack) k (next stack) (dec level)))
        (and k (> level k)) (cons (first u)
                                  (lazy-seq (flatten-walk (next u) k stack level)))
        :else
        (let [f (first u)]
          (if (tensor? f)
            (recur (normal f) k (cons (next u) stack) (inc level))
            (cons f (lazy-seq (flatten-walk (next u) k stack level)))))))

(defn flatter
  "(flatter u) yields a lazy seq of the tensor u with one level of its elements flattened.
   (flatter u 0) yields (normal u).
   (flatter u k) yields a lazy seq of the tensor u with k levels of its elements flattened."
  ([u k]
     (or (tensor? u) (not-tensor-err u 'flatter))
     (or (and (integer? k) (>= k 0)) (arg-err "flatter requires a non-negative integer"))
     (if (= k 0) (normal u) (flatten-walk (normal u) k nil 1)))
  ([u] (flatter u 1)))
(defn flattest
  "(flatterst u) yields a lazy seq of the tensor u with all levels of its elements flattened.
   (flatterst u 0) yields (normal u).
   (flatterst u k) yields a lazy seq of the tensor u with k levels of its elements flattened."
  ([u k]
     (or (tensor? u) (not-tensor-err u 'flattest))
     (or (and (integer? k) (>= k 0)) (arg-err "flatter requires a non-negative integer"))
     (if (= k 0) (normal u) (flatten-walk (normal u) k nil 1)))
  ([u]
     (or (tensor? u) (not-tensor-err u 'flattest))
     (flatten-walk u nil nil 1)))

(defmacro reap
  "(reap expr) yields a vector of all items, in order, for which (sow item) was called."
  [& expr]
  (let [result (gensym)]
    `(let [~result (transient [])]
       (letfn [(~'sow [~'item] (conj! ~result ~'item))]
         ~@expr)
       (persistent! ~result))))
(defmacro reap-tags
  "(reap expr tags) yields a map of each tag in the tensor tags mapped to the items, in order,
     that were sown via (sow item tag). Multiple tags may be sown at once with
     (sow item tag1 tag2 ...).
   (reap expr :all) yields a map of all tags sown in expr."
  [tags & expr]
  (let [result (gensym "result"), tag (gensym "tag"), items (gensym "tmp"), pers (gensym "pers")]
    (cond
     (= tags :all)
     `(let [~result (transient {})]
        (letfn [(~'sow [~'item & ~'tag-list]
                       (doseq [~tag ~'tag-list]
                         (if-let [~items (.valAt ~result ~tag)]
                           (conj! ~items ~'item)
                           (assoc! ~result ~tag (transient [~'item])))))]
          ~@expr)
        (let [~pers (persistent! ~result)]
          (persistent!
           (reduce #(assoc! %1 (key %2) (persistent! (val %2)))
                   (transient ~pers)
                   ~pers))))
     (coll? tags)
     `(let [~result (reduce #(assoc! %1 %2 (transient [])) (transient {}) ~tags)]
        (letfn [(~'sow [~'item & ~'tag-list]
                       (doseq [~tag ~'tag-list]
                         (if-let [~items (.valAt ~result ~tag)]
                           (conj! ~items ~'item)
                           (throw (IllegalArgumentException.
                                   (str "sow called with unrecognized tag: " ~tag))))))]
          ~@expr)
        (let [~pers (persistent! ~result)]
          (persistent!
           (reduce #(assoc! %1 (key %2) (persistent! (val %2)))
                   (transient ~pers)
                   ~pers))))
     :else (arg-err "tag must be either :all or a collection of tags"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Convenient redefinitions of basic arithmatic for tensors.

;; used for dimension traversal
(defn- dimensions-walk [u]
  (let [f (first u)
        n (when (tensor? f) (length f))]
    (when (and n (every? #(and (tensor? %) (= (length %) n)) (next u)))
      (cons n (lazy-seq (dimensions-walk (apply concat u)))))))            

(defn dimensions
  "(dimensions) yields nil.
   (dimensions u) yields a lazy seq of the rectangular dimensions of tensor u; dimensions traverses
     the tensor u to the deepest rectangular level. If u is not a tensor, yields nil.
   (dimensions u1 u2 ... un) is equivalent to (next (dimensions [u1 u2 ... un]))."
  [& tensors]
  (when tensors (lazy-seq (dimensions-walk tensors))))

(defn rank
  "(rank u) yields the rank of tensor u. For numbers this is 0, and for non-tensors and non-numbers,
     yields nil."
  [u] (cond (tensor? u) (count (dimensions u))
            (number? u) 0
            :else nil))

(defn thread
  "(thread f 0 u1 ... un) yields (apply f [u1 ... un]).
   (thread f 1 u1 ... un) yields (map f u1 ... un).
   (thread f -1 u1 ... un) yields the function f applied to the elements of the given tensors at
     the lowest depth possible.
   (thread f -2 u1 ... un) yields the function f applied to the elements of the given tensors at
     the second lowest depth possible.
   (thread f depth u1 ... un) yields the tensor constructed by applying f to the depth-level
     elements in turn.
   (thread f u1 ... un) yields (thread f 1 u1 ... un)."
  [f depth & more]
  (cond (tensor? depth) (apply thread (concat [f 1 depth] more))
        (not (integer? depth))) (arg-err "thread depth must be an integer")
        (nil? more) nil
        (< depth 0) (let [max-depth (count (apply dimensions more))]
                      (if (< max-depth (- depth))
                        (arg-err "negative depth is greater than max-depth of tensors")
                        (apply thread (cons f (cons (+ max-depth (inc depth)) more)))))
        (= depth 0) (apply f more)
        (let [n (length (first more))]
          (not (every? #(= (length %) n) more))) (arg-err "Dimension mismatch in thread")
        :else (let [d (dec depth)] (apply map (cons #(thread f d %&) (map normal more)))))
  ([f u] (thread f 1 u)))
(defn pthread
  "pthread behaves identically to thread except that it threads in parallel using pmap."
  [f depth & more]
  (cond (tensor? depth) (apply thread (concat [f 1 depth] more))
        (not (integer? depth)) (arg-err "thread depth must be an integer")
        (nil? more) nil
        (< depth 0) (let [max-depth (count (apply dimensions more))]
                      (if (< max-depth (- depth))
                        (arg-err "negative depth is greater than max-depth of tensors")
                        (apply thread (cons f (cons (+ max-depth (inc depth)) more)))))
        (= depth 0) (apply f more)
        (let [n (length (first more))]
          (not (every? #(= (length %) n) more))) (arg-err "Dimension mismatch in thread")
        :else (let [d (dec depth)] (apply pmap (cons #(thread f d %&) (map normal more)))))
  ([f u] (thread f 1 u)))


(defn inner
  "(inner f u v g) is a generalization of dot that yields the inner product of tensors u and v using
     function f in place of plus and function g in place of times. The resulting tensor will be of
     rank (+ (rank u) (rank v) -2)"
  ([f u v g]
     (cond (or (not (tensor? u)) (not (tensor? v))) (arg-err "inner can only operate over tensors")
     
(defn pinner
  "pinner is identical to inner except that it maps in parallel over tensors using pmap."
  )

(defn dot
  "(dot u v) yields the dot product of tensor u . tensor v.
   (dot u1 u2 ... un) yields the dot product of (((u1 . u2) . u3) ... un)"
  ([u v] (inner plus u v times))
  ([u v & more] (reduce #(inner plus %1 %2 times) (inner plus u v times) more)))
(defn pdot
  "pdot is identical to dot except that it maps over tensors in parallel using pmap."
  ([u v] (pinner plus u v times))
  ([u v & more] (reduce #(pinner plus %1 %2 times) (pinner plus u v times) more)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tensor Multiplication Algorithms

(defn naive-matrix-multiply
  "(naive-matrix-multiply u v) yields the result of ")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The math-type code goes here.

(def math-types
  "The math-types hierarchy includes all types in the math subsystem."
  (-> (make-hierarchy)
      (derive :nben/math/complex :nben/math/tensor)
      (derive :nben/math/real :nben/math/complex)
      (derive :nben/math/rational :nben/math/real)
      (derive :nben/math/integer :nben/math/rational)
      (derive :nben/math/natural :nben/math/integer)))

;; these answer questions about types...
(defn tensor?
  "(tensor? u) yields true if u is a tensor and false otherwise."
  [u] (isa? math-types (math-type u) :nben/math/tensor))
(defn scalar?
  "(scalar? u) yields true if u is a scalar and false otherwise."
  [u] (isa? math-types (math-type u) :nben/math/scalar))
(defn complex?
  "(complex? u) yields true if u is a complex number and false otherwise."
  [u] (isa? math-types (math-type u) :nben/math/complex))
(defn real?
  "(real? u) yields true if u is a real number and false otherwise."
  [u] (isa? math-types (math-type u) :nben/math/real))
(defn rational-number?
  "(rational-number? u) yields true if u is a rational number and false otherwise."
  [u] (isa? math-types (math-type u) :nben/math/rational))
(defn int?
  "(int? u) yields true if u is an integer and false otherwise. Numbers such as 5.0 and 6/6 are
     considered integers for the purpose of this function."
  [u] (isa? math-types (math-type u) :nben/math/integer))
(defn natural?
  "(natural? u) yields true if u is a natural number and false otherwise. Numbers such as 5.0 and
     6/6 are considered natural numbers for the purpose of this function."
  [u] (isa? math-types (math-type u) :nben/math/natural))
(defn math-type?
  "(math-type? u) yields true if and only if u is of a type understood by the nben math system.
     It is equivalent to (not (math-type u))."
  [u] (not (math-type u)))

(defprotocol supported-math-type
  "The supported-type protocol must be defined for any class that can be used with the nben math
   subsystem. The methods must yield elements of the math-types hierarchy."
  (math-type [self] "Yields a keyword scoped to :nben/math that indicates the object's type"))

(extend-protocol supported-math-type

  ;; nil is in fact a tensor of rank 0
  nil (math-type [u] :nben/math/tensor)

  ;; but objects, unless overloaded in another way, do not have math types
  Object (math-type [x] nil)
  
  ;; in case someone overloads a number, we can deal with that as well
  Number (math-type [x] (if (= 0 (mod x 1))
                          (if (> 0 x) :nben/math/natural :nben/math/integer)
                          :nben/math/rational))

  ;; We can improve performance on the integers by skipping the is-integer test...
  Byte (math-type [x] (if (> x 0) :nben/math/natural :nben/math/integer))
  Short (math-type [x] (if (> x 0) :nben/math/natural :nben/math/integer))
  Long (math-type [x] (if (> x 0) :nben/math/natural :nben/math/integer))
  Integer (math-type [x] (if (> x 0) :nben/math/natural :nben/math/integer))
  java.math.BigInteger (math-type [x] (if (> x 0) :nben/math/natural :nben/math/integer))
  java.util.concurrent.atomic.AtomicInteger
  (math-type [x] (if (> x 0) :nben/math/natural :nben/math/integer))

  ;; For the rational numbers, we have to be more careful
  clojure.lang.Ratio (math-type [x] (if (= 0 (mod (.numerator x) (.denominator x)))
                                      (if (> x 0) :nben/math/natural :nben/math/integer)
                                      :nben/math/rational))
  Float (math-type [x] (if (= (.intValue x) (.floatValue x))
                         (if (> 0 x) :nben/math/natural :nben/math/integer)
                         :nben/math/rational))
  Double (math-type [x] (if (= (.intValue x) (.doubleValue x))
                          (if (> 0 x) :nben/math/natural :nben/math/integer)
                          :nben/math/rational))
  java.math.BigDecimal (math-type [x] (if (= 0 (mod x 1))
                                        (if (> 0 x) :nben/math/natural :nben/math/integer)
                                        :nben/math/rational))
  
  ;; There are many collection types that may be tensors
  clojure.lang.IPersistentCollection (math-type [x] :nben/math/tensor)
  clojure.lang.Seqable (math-type [x] :nben/math/tensor)
  clojure.lang.IPersistentSet (math-type [x] nil)
                       
  ;; Maps may be any type, depending on their elements
  clojure.lang.IPersistentMap
  (math-type [m] (cond (or (and (real? (.valAt m :re)) (real? (.valAt m :im)))
                           (and (real? (.valAt m :arg))
                                (let [r (.valAt m :im)] (and (real? r) (>= r 0)))))
                       :nben/math/complex
                       (let [len (.valAt o :length)] (and len (int? len)))
                       :nben/math/tensor
                       :else nil)))



                               
                           
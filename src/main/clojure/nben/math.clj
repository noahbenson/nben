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
(defn- not-traversable-err
  [&{:keys [arg-name fn-name] :or {arg-name "<unknown>" fn-name "<unknown>"}}]
  (throw (IllegalArgumentException.
          (str "Argument " arg-name " to function " fn-name " is not traversable"))))
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
;; Section 1.1. The datum protocol, which defines the basic data type that all math-comatible types
;; must extend.
;; #datum
(defprotocol datum
  "The datum protocol is a low-level type for the nben math library; all types that can interact
   with the library must extend this."
  (datum? [self] "(datum x) yields true if and only if the object x is a math datum.")
  (math-type [self]
    "(math-type x) yields a type, compatible with the math-types hierarchy, of the object x,
       assuming it is a datum."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Section 1.2. The traversable protocol
;; #traversable

;; Section 1.2.1. The protocol itself ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defprotocol traversable
  "The traversable protocol should be implemented by anything that can be seq'ed over; it probably
    shouldn't be necessary in the math library, but it is nice to include for use with some of the
    general math functions."
  (traversable? [self] "(traversable? u) yields true if and only if u is a traversable object.")
  (traverse [self] "(traverse u) yields a seq of the elements of u.")
  (length [self] "(lenght u) yields the number of elements in the traversable u")
  (elements [self indices]
    "(elements u parts) yields a traversable that is a subsequence of u composed of the elements
       specified by parts. The parts argument may be an integer index or a seq of index specifiers.
       An index specifier may be any integer between -n and (n-1) (where n is the length of u);
       negative numbers are traslated into n plus the number."))

;; Section 1.2.2. The functions that require only the above protocol ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn eq?
  "(eq? u v) yields true if and only if either (= u v) or u and v are traversables the same length
     n such that for 0 <= i < n, (eq? (elements u i) (elements v i)).
   (eq? condition u v) yields true if and only if either (condition u v) or u and v are traversables
     the same length n such that for 0 <= i < n, (eq? condition (elelemts u i) (elements v i))."
  ([u v]
     (cond (= u v) true
           (not (traversable? u)) false
           (not (traversable? v)) false
           (== (length u) (length v)) (every? true? (map = (traverse u) (traverse v)))
           :else false))
  ([condition u v]
     (cond (condition u v) true
           (not (traversable? u)) false
           (not (traversable? v)) false
           (== (length u) (length v)) (every? true? (map condition (normal u) (normal v)))
           :else false)))
(defn precedes?
  "(precedes? a b) yields true if and only if a comes before b in canonical order; for traversables,
     precedes? will traverse the argument down to any non-traversable element and compare there.
     (precedes? a b) is equivalent to (precedes? < a b). All tensors are considered to come after
     all atoms in canonical order.
   (precedes? test-fn a b) performs precedes? using the ordering function test-fn."
  ([test-fn a b]
     (cond
      (not (traversable? a)) (if (traversable? b) true (test-fn a b))
      (not (traversable? b)) false
      :else (loop [sa (traverse a), sb (traverse b)]
              (cond (nil? sa) (not (nil? sb))
                    (nil? sb) false
                    :else (let [fa (first sa), fb (first sb)]
                            (if (traversable? fa)
                              (if (traversable? fb)
                                (cond (precedes? test-fn fa fb) true
                                      (precedes? test-fn fb fa) false
                                      :else (recur (next sa) (next sb)))
                                false)
                              (if (tensor? fb)
                                true
                                (cond (test-fn fa fb) true
                                      (test-fn fb fa) false
                                      :else (recur (next sa) (next sb))))))))))
  ([a b] (precedes? < a b)))
(defn ordered?
  "(ordered? u) yields true if the elements of tensor u are in order. (ordered? u) is equivalent
     to (ordered? u precedes?).
   (ordered? u sort-fn) yields true if the elements of tensor u are ordered according to
     (sort-fn u1 u2)."
  ([u sort-fn]
     (loop [s (next (traverse u)), f (elements u 0)]
       (if s
         (let [fn (first s)]
           (if (sort-fn f fn)
             (recur (next s) fn)
             false))
         true)))
  ([u] (ordered? u precedes?)))
(defn- take-while-thread
  [cond-fn travs]
  (when (and (every? seq? travs)
             (apply cond-fn (map first travs)))
    (cons (first (first travs))
          (lazy-seq (take-while-thread cond-fn (map next travs))))))    
(defn take-while
  "(take-while condition-fn u1 u2 ...) yields a lazy-seq of the first k elements of u1 such that k+1
     is the first index for which (condition-fn (elements u1 k+1) (elements u2 k+1) ...) yields nil
     or false."
  [condition-fn & traversables]
  (and traversables
       (or (every? traversable? traversables) (arg-err "take-while requires traversable arguments"))
       (or (ifn? condition-fn) (arg-err "take-while requires the first arg be an ifn"))
       (lazy-seq (take-while-thread cond-fn (map traverse traversables)))))
(defn- dimensions-thread
  "(dimensions-thread u) yields the dimensions of traversable u."
  ([u guide]
     (when (and u guide)
       (loop [s u, g guide, unverified nil]
         (if s
           (when (not (empty? g))
             (let [n (first g)
                   f (first s)
                   dims (:nben/math/dimensions (meta f))]
               (if dims
                 (recur (next s) (take-while == g dims) unverified)
                 (when (traversable? f)
                   (when (== (length f) n)
                     (recur (next s) g (cons f unverified)))))))
           (cons (first g)
                 (lazy-seq (dimensions-thread (apply concat unverified) (next g))))))))
  ([u]
     (when u
       (loop [s u, n nil, unverified nil, guide :none]
         (when guide
           (if s
             (let [f (first s)
                   dims (:nben/math/dimensions (meta f))]
               (if dims
                 (if (not= guide :none)
                   (recur (next s) n unverified (take-while = guide dims))
                   (if n
                     (when (== n (first dims))
                       (recur (next s) n unverified dims))
                     (dimensions-thread (next s) dims)))
                 (when (traversable? f)
                   (if n
                     (when (== n (length f))
                       (recur (next s) n (cons f unverified) guide))
                     (let [n0 (length f)]
                       (when (> 0 n0) (recur (next s) n0 (cons f unverified) guide)))))))
             (when n
               (cons n (if guide
                         (lazy-seq (dimensions-thread unverified guide))
                         (lazy-seq (dimensions-thread unverified)))))))))))
(defn dimensions
  "(dimensions) yields nil.
   (dimensions u) yields a lazy seq of the rectangular dimensions of tensor u; dimensions traverses
     the tensor u to the deepest rectangular level. If u is not traversable, yields nil.
   (dimensions u1 u2 ... un) is equivalent to (next (dimensions [u1 u2 ... un]))."
  [& traversibles]
  (when traversibles (lazy-seq (dimensions-walk traversibles))))
(defn dimensionality
  "(dimensionality) yields nil.
   (dimensionality u) yields the number of rectangular dimensions in the traversable object u. Note
     that for tensors, (rank u) is identical to (dimensionality u), but for non-tensors, (rank u)
     will throw an exception.
   (dimensionality u1 u2 ... un) is equivalent to (dec (dimensionality [u1 u2 ... un]))."
  [& args] (when args (dec (count (dimensions args)))))
(defn thread
  "(thread f 0 u1 ... un) yields (apply f [u1 ... un]).
   (thread f 1 u1 ... un) yields (map f u1 ... un).
   (thread f -1 u1 ... un) yields the function f applied to the elements of the given traversables
     at the lowest depth possible.
   (thread f -2 u1 ... un) yields the function f applied to the elements of the given traversables
     at the second lowest depth possible.
   (thread f depth u1 ... un) yields the traversable constructed by applying f to the depth-level
     elements in turn.
   (thread f u1 ... un) yields (thread f 1 u1 ... un)."
  ([f depth & more]
     (cond (traversable? depth) (apply thread (concat [f 1 depth] more))
           (not (integer? depth)) (arg-err "thread depth must be an integer")
           (nil? more) nil
           (< depth 0) (let [max-depth (count (apply dimensions more))]
                         (if (< max-depth (- depth))
                           (arg-err "negative depth is greater than max-depth of tensors")
                           (apply thread (cons f (cons (+ max-depth (inc depth)) more)))))
           (= depth 0) (apply f more)
           (let [n (length (first more))]
             (not (every? #(= (length %) n) more))) (arg-err "Dimension mismatch in thread")
             :else (let [d (dec depth)] (apply map (cons #(thread f d %&) (map traverse more))))))
  ([f u] (thread f 1 u)))
(defn pthread
  "pthread behaves identically to thread except that it threads in parallel using pmap."
  ([f depth & more]
     (cond (traversable? depth) (apply pthread (concat [f 1 depth] more))
           (not (integer? depth)) (arg-err "pthread depth must be an integer")
           (nil? more) nil
           (< depth 0) (let [max-depth (count (apply dimensions more))]
                         (if (< max-depth (- depth))
                           (arg-err "negative depth is greater than max-depth of tensors")
                           (apply pthread (cons f (cons (+ max-depth (inc depth)) more)))))
           (= depth 0) (apply f more)
           (let [n (length (first more))]
             (not (every? #(= (length %) n) more))) (arg-err "Dimension mismatch in thread")
             :else (let [d (dec depth)] (apply pmap (cons #(pthread f d %&) (map traverse more))))))
  ([f u] (pthread f 1 u)))
(defn- index-walk [s i test-fn val] ;; used by index below
  (when s
    (let [f (test-fn (first s))]
      (if (= f val)
        (cons i (lazy-seq (index-walk (next s) (inc i) test-fn val)))
        (recur (next s) (inc i) test-fn val)))))
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
     (loop [i 0, s (traverse u), res (transient {})]
       (if s
         (let [f (first s)
               cur (.valAt res f)]
           (recur (inc i) (next s) (assoc! res f (if cur (conj cur i) [i]))))
         (persistent! res))))
  ([u label-fn]
     (loop [i 0, s (traverse u), res (transient {})]
       (if s
         (let [f (label-fn (first s))
               cur (.valAt res f)]
           (recur (inc i) (next s) (assoc! res f (if cur (conj cur i) [i]))))
         (persistent! res))))
  ([u label-fn val] (index-walk (traverse u) 0 label-fn val))
  ([u label-fn val1 & rest]
     (loop [i 0, s (traverse u), res (reduce #(assoc! %1 %2 []) (transient {}) (cons val1 rest))]
       (if s
         (let [f (label-fn (first s))
               cur (.valAt res f)]
           (if cur 
             (recur (inc i) (next s) (assoc! res f (if cur (conj cur i) [i])))
             (recur (inc i) (next s) res)))
         (persistent! res))))))
(defn tally
  "(tally u) yields a map of the elements of u to the number of times each element occurs in the
     traversable u. (tally u) is equivalent to (tally u eq?).
   (tally u test-fn) yields a map of, for each element e, (test-fn e) mapped to the number of times
     (test-fn e) occurs in the traversable u."
  ([u test-fn]
     (loop [s (traverse u), res (transient {})]
       (if s
         (recur (next s) (let [f (first s)] (assoc! res f (inc (.valAt res f 0)))))
         (persistent! res))))
  ([u] (tally u eq?)))
(defn commonest
  "(commonest u) yields the commonest element of u; if more than one element is commonest, yields
     the one that occurs earliest in u.
   (commonest u n) yields the n commonest elements of u; in the case of ambiguities, the elements
     occuring earliest in the list are returned first."
  ([u] (loop [s (traverse u), i 0, tally (transient {}), cur-best nil]
         (if s
           (let [f (first s), cur (.valAt tally f)]
             (if (nil? cur)
               (recur (next s) (inc i) (assoc! tally f [1 i]) (or cur-best [[-1 i] f]))
               (let [[count i0] cur, new-count (inc count),
                     new-el [new-count i0], new-cur [(- new-count) i0]]
                 (recur (next s) (inc i) (assoc! tally f new-el)
                        (if (precedes? (first cur-best) new-cur) cur-best [new-cur f])))))
           (fnext cur-best))))
  ([u n] (loop [s (traverse u), i 0, tally (transient {})]
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
     (loop [s (traverse u), i 0,
            r (sorted-set-by #(comp-fn (first %1) (first %2)))]
       (if s
         (recur (next s) (inc i)
                (if (and k (= (count r) k))
                  (disj (conj r [(first s) i]) (first (rseq r)))
                  (conj r [(first s) i])))
         (map fnext (seq r)))))
  ([arg1 arg2]
     (cond (and (traversable? arg1) (integer? arg2) (>= 0 arg2)) (ordering precedes? arg1 arg2)
           (and (ifn? arg1) (traversable? arg2)) (ordering arg1 arg2 nil)
           :else (arg-err "ordering requires a tensor and integer or function and tensor")))
  ([u] ;; full ordering is just a sorting problem
     (map fnext
          (sort #(precedes? (first %1) (first %2))
                (map list (traverse u) (range))))))
(defn part
  "(part u k) yields the kth element of traversable u.
   (part u k1 k2 ...) yields the k2th element of the k1th element of traversable u, etc.
   The k's may be traversables of indices, in which case the elements are returned as a lazy seq in
     order. E.g., (part u [1 2 3] 9) yields ((part u 1 9) (part u 2 9) (part u 3 9)).

   Part is effectively a nested version of the elements function of the traversable protocol."
  [u & indices]
  (if indices
    (if (traversable? u)
      (let [f (first indices)
          els (elements u f)
          n (next indices)]
        (map part els (repeat n)))
      (not-traversable-err u 'part))
    u))
;; used by flatter and flattest to lazily walk and flatten tensors
(defn- flatten-walk [u k stack level]
  (cond (nil? u) (when stack (recur (first stack) k (next stack) (dec level)))
        (and k (> level k)) (cons (first u)
                                  (lazy-seq (flatten-walk (next u) k stack level)))
        :else
        (let [f (first u)]
          (if (ttraversable? f)
            (recur (traverse f) k (cons (next u) stack) (inc level))
            (cons f (lazy-seq (flatten-walk (next u) k stack level)))))))
(defn flatter
  "(flatter u) yields a lazy seq of the traversable u with one level of its elements flattened.
   (flatter u 0) yields (traverse u).
   (flatter u k) yields a lazy seq of the traversable u with k levels of its elements flattened."
  ([u k]
     (if (traversable? u) 
       (if (and (integer? k) (>= k 0))
         (arg-err "flatter requires a non-negative integer"))
         (if (= k 0) (traverse u) (flatten-walk (traverse u) k nil 1)))
       (not-traversable-err u 'flatter))
  ([u] (flatter u 1)))
(defn flattest
  "(flatterst u) yields a lazy seq of the tensor u with all levels of its elements flattened.
   (flatterst u 0) yields (normal u).
   (flatterst u k) yields a lazy seq of the tensor u with k levels of its elements flattened."
  ([u k]
     (if (traversable? u)
       (if (and (integer? k) (>= k 0))
         (if (= k 0)
           (traverse u)
           (flatten-walk (traverse u) k nil 1))
         (arg-err "flatter requires a non-negative integer"))
       (not-tensor-err u 'flattest)))
  ([u]
     (if (traversable? u)
       (flatten-walk u nil nil 1)
       (not-traversable-err u 'flattest))))
(defmacro reap
  "(reap expr) yields a vector of all items, in order, for which (sow item) was called."
  [& expr]
  `(let [result# (transient [])]
       (letfn [(~'sow [~'item] (conj! result# ~'item))]
         ~@expr)
       (persistent! result#)))
(defmacro reap-tags
  "(reap expr tags) yields a map of each tag in the traversable tags mapped to the items, in order,
     that were sown via (sow item tag). Multiple tags may be sown at once with
     (sow item tag1 tag2 ...).
   (reap expr :all) yields a map of all tags sown in expr."
  [tags & expr]
  (let [result (gensym "result"), tag (gensym "tag"), items (gensym "tmp"), pers (gensym "pers")]
    (cond
     (= tags :all)
     `(let [result# (transient {})]
        (letfn [(~'sow [~'item & ~'tag-list]
                       (doseq [tag# ~'tag-list]
                         (if-let [items# (.valAt result# tag#)]
                           (conj! items# ~'item)
                           (assoc! result# tag# (transient [~'item])))))]
          ~@expr)
        (let [pers# (persistent! result#)]
          (persistent!
           (reduce #(assoc! %1 (key %2) (persistent! (val %2)))
                   (transient pers#)
                   pers#))))
     (coll? tags)
     `(let [result# (reduce #(assoc! %1 %2 (transient [])) (transient {}) ~tags)]
        (letfn [(~'sow [~'item & ~'tag-list]
                       (doseq [tag# ~'tag-list]
                         (if-let [~items (.valAt result# tag#)]
                           (conj! items# ~'item)
                           (throw (IllegalArgumentException.
                                   (str "sow called with unrecognized tag: " tag#))))))]
          ~@expr)
        (let [pers# (persistent! result#)]
          (persistent!
           (reduce #(assoc! %1 (key %2) (persistent! (val %2)))
                   (transient pers#)
                   pers#))))
     :else (arg-err "tag must be either :all or a collection of tags"))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Section 1.3. The tensor protocol
;; #tensor

;; Section 1.3.1. The tensor protocol definition ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defprotocol tensor
  "The tensor is a protocol requires a minimal number of functions for performing tensor operations
   in conjunction with the nben clojure library. Tensors are considered to be rectangular
   traversables."

  ;; First: the functions that determine what this is and how well it runs
  (tensor? [self]
    "(tensor? u) yields true if u is a tensor and false if it is not."))

;; Section 1.3.2. The functions that depend only on the tensor protocol ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- as-tensor-thread
  "(as-tensor-thread u) yields the dimensions of u if u is a tensor with dimensionality greater than
     0 and nil otherwise."
  ([u guide]
     (cond (nil? u) nil
           (complex? u) (when (nil? guide) u)
           (not (tranversable? u)) nil
           (loop [s (traverse u)
                  d (next guide)
                  n 0]
             (if s
               (let [u0 (as-tensor-thread (first u) d)]
                 (when u0 (recur (next s) d (inc n))))
               (if (== n (first guide))
                 (if (nil? d) (cons n nil) (cons n d))
                 nil)))))
  ([u]
     (cond (nil? u) nil
           (complex? u) 0
           (not (traversable? u)) nil
           :else
           (loop [s (traverse u)
                  d (as-tensor-thread (first s))
                  n 0]
             (if (= n 0)
               (when s (recur (next s) d 1))
               (if s
                 (let [u0 (as-tensor-thread (first u) d)]
                   (when u0 (recur (next s) d (inc n))))
                 (if (== d 0) (cons n nil) (cons n d))))))))
(defn as-tensor
  "(as-tensor u) yields a version of u with added tensor-related meta-data in some cases if u is
     both a tensor and a clojure IObj. If u is not a tensor, yields nil."
  [u] (or (and (complex? u) u)
          (and (traversable? u)
               (let [dims (as-tensor-thread u)]
                 (if dims
                   (if (instance? clojure.lang.IObj u)
                     (with-meta u (assoc (meta u) :nben/math/dimensions dims))
                     u)
                   nil)))))
(defn assert-tensor
  "(assert-tensor u) yields a version of u with added tensor-related meta-data in some cases if u is
     a tensor (a traversable all of whose elements are tensors of the same dimensionality). If u is
     not a tensor, an exception is thrown.
   (assert-tensor u & msgs) is identical to (assert-tensor u) except that it throws an illegal
     argument exception with the error message (apply str msgs)."
  ([u] (or (as-tensor u) (arg-err "assert-tensor failed")))
  ([u & msgs] (or (as-tensor u) (arg-err (apply str msgs)))))
(defn rank
  "(rank u) yields the rank of tensor u or throws an exception if u is not a tensor."
  [u] (let [uu (assert-tensor u "rank requested of non-tensor")]
        (dimensionality uu)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Section 1.2. The complex protocol
;; #complex
(defprotocol complex
  "The complex-number protocol defines the functions for dealing with complex numbers."
  (complex? [self] "(complex? x) yields true if and only if x is a complex number.")
  (re [self] "(re z) yields the real part of the complex number z.")
  (im [self] "(im z) yields the imaginary part of the complex number z."))

;; Also, the data structure for a complex number
(defrecord complex-number [real-part imaginary-part]
  complex
  (complex? [self] true)
  (re [self] real-part)
  (im [self] imaginary-part))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Section 1.3. The real protocol
(defprotocol real
  "The real protocol defines the functions for dealing with real numbers. This protocol requires
   only that real numbers ever be resolved to a finite precision."
  (real? [self] "(real? x) yields true if and only if x is a real number; otherwise false.")
  (estimate [self precision]
    "(estimate x precision) yields a Double or BigDecimal estimate of x whose error is less than
       precision, which must be positive."))

;; We also have a real operator place-holder
(defrecord real-operator [operator-fn error-fn arguments]
  real
  (real? [self] true)
  (estimate [self precision]
    (let [arg-precisions (error-fn precision arguments)]
      (apply operator-fn
        (map estimate arguments (if (seq? arg-precisions) (repeat arg-precisions)))))))
      
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
;; Section 2.1. Extension of the datum protocol.
;; #datum
(extend-protocol datum

  ;; nil is not a datum
  nil
  (datum? [o] false)
  (math-type [o] nil)
  
  ;; Objects generally can be datums if they are numbers or traversables
  Object
  (datum? [o] (or (traversable? o) (complex? o)))
  (math-type [o]
    (if (traversable? o)
      (when (tensor? o) ::tensor)
      (when (complex? o)
        (if (real? o)
          (if (quotient? o)
            (if (int? o)
              (if (natural? o)
                ::natural
                ::integer)
              ::rational)
            ::real)
          ::complex))))

  ;; we know a bit more than this generic object description for most things though; e.g. numbers
  Number
  (datum? [o] true)
  (math-type [o] (if (int? o)
                   (if (natural? o) ::natural ::integer)
                   ::rational))
  ;; doubles and floats we need an extra check to manage
  Double
  (datum? [o] true)
  (math-type [o] (when (not (or (.isNaN o) (.isInfinite o)))
                   (if (int? o)
                     (if (natural? o) ::natural ::integer)
                     ::rational)))
  Float
  (datum? [o] true)
  (math-type [o] (when (not (or (.isNaN o) (.isInfinite o)))
                   (if (int? o)
                     (if (natural? o) ::natural ::integer)
                     ::rational)))

  ;; Integers we have more information on as well
  Integer
  (datum? [n] true)
  (math-type [n] (if (> n 0) ::natural ::integer))
  Long
  (datum? [n] true)
  (math-type [n] (if (> n 0) ::natural ::integer))
  Short
  (datum? [n] true)
  (math-type [n] (if (> n 0) ::natural ::integer))
  Byte
  (datum? [n] true)
  (math-type [n] (if (> n 0) ::natural ::integer))
  java.math.BigInteger
  (datum? [n] true)
  (math-type [n] (if (> n 0) ::natural ::integer))
  clojure.core.BigInt
  (datum? [n] true)
  (math-type [n] (if (> n 0) ::natural ::integer))

  ;; collections are datums generally
  clojure.lang.IPersistentCollection
  (datum? [c] true)
  (math-type [o] (when (tensor? o) ::tensor)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Section 2.2. Extension of the traversable protocol
;; #traversable
(extend-protocol traversable

  ;; nil is not a travesavle
  nil
  (traversable? [o] false)
  (traverse [self] nil)
  (length [self] 0)
  (elements [self indices] (arg-err "elements requested of nil"))

  ;; Objects are not generally traversable
  Object
  (traversable? [o] false)
  (traverse [o] (arg-err "cannot traverse non-traversable"))
  (length [o] (arg-err "cannot take length of non-traversable"))
  (elements [o indices] (arg-err "elements requested of non-traversable"))

  ;; most collections, however, are traversables
  clojure.lang.IPersistentCollection
  (traversable? [c] true)
  (traverse [c] (.seq c))
  (length [c] (count c))
  (elements [c indices]
    (cond 
     (= indices :all) c
     (int? indices) (nth c (if (< indices 0) (+ N indices) indices))
     :else                             
     (let [N (delay (count c))
           idcs0 (map (fn [i] (cond (not int?) (arg-err "non-integer index given to elements")
                                    (< i 0) (+ @N i)
                                    :else i))
                      (if (traversable? indices)
                        (traverse indices)
                        (arg-err "non-integer and non-traversable given to elements")))]
       (loop [s (seq c), idcs (sort idcs0), n 0, els (transient {})]
         (if idcs
           (if s
             (let [i (first idcs)]
               (if (== n i)
                 (recur (next s) (next idcs) (assoc! els i (first s)))
                 (let [tmp (nthnext s (- n i))]
                   (recur (next tmp) (next idcs) (assoc! els i (first tmp))))))
             (arg-err "index requested of traversable is out of range"))
           (let [e (persistent! els)] (map e idcs0)))))))
  ;; some collections we have better strategies for...
  clojure.lang.IPersistentVector
  (traversable? [u] true)
  (traverse [u] (.seq u))
  (length [u] (.count u))
  (elements [u indices]
    (let [N (.count u)]
      (map (fn [i] (if (int? i)
                     (.nth u (if (< i 0) (+ N i) i))
                     (arg-err "non-integer index given to elements")))
           indices)))
  ;; maps we don't consider traversables except in special circumstances
  clojure.lang.IPersistentMap
  (traversable? [u] (let [n (.valAt u :length)]
                      (and n (int? n) (>= n 0))))
  (traverse [u] (let [n (.valAt u :length)
                      dflt (when n (.valAt u :default))]
                  (if (and n (int? n) (>= n 0))
                    (map (fn [i] (or (.valAt u i) dflt))
                         (range n))
                    (arg-err "cannot traverse non-traversable map"))))
  (length [u] (let [n (.valAt u :length)]
                (if (and n (int? n) (>= n 0))
                  n
                  (arg-err "cannot take length of non-traversable map"))))
  (elements [u indices]
    (let [n (.valAt u :length)
          dflt (.valAt u :default)]
      (if (and n (int? n) (>= n 0))
        (map (fn [i] (if (int? i)
                       (.valAt u (if (< n 0) (+ n i) i) dflt)
                       (arg-err "non-integer index given to elements")))
             indices)
        (arg-err "cannot take elements from non-traversable map"))))
  ;; sets are not traversables either...
  clojure.lang.IPersistentSet
  (traversable? [u] false)
  (traverse [u] (arg-err "sets are not considered traversables"))
  (length [u] (arg-err "length requested of non-traversable set"))
  (elements [u] (arg-err "elements requested of non-traversable set")))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Section 2.3. Extension of the tensor protocol
;; #tensor
(extend-protocol tensor

  ;; Objects are not tensors unless they are complex numbers or one of the below types
  Object (tensor? [o] (complex? o))

  ;; Numbers we can do directly... 
  Number (tensor? [o] (if (as-tensor o) true false))
      
  ;; though Doubles must be checked for nan's
  Double (tensor? [o] (not (or (.isNaN o) (.isInfinite o))))
  Float  (tensor? [o] (not (or (.isNaN o) (.isInfinite o))))

  ;; real and complex numbers we can also do directly
  nben.jvm.IRealNumber (tensor? [o] true)

  complex-number (tensor? [o] true)

  ;; But collections may be...
  clojure.lang.IPersistentCollection (tensor? [o] (if (as-tensor o) true false))

  ;; Anything that implements Seqable is equally a possible tensor
  clojure.lang.Seqable (tensor? [o] (if (as-tensor (.seq o)) true false))

  ;; IPersistentSet's, however, are not tensors
  clojure.lang.IPersistentSet (tensor? [o] false)

  ;; IPersistentMap's may be (sparse) tensors, if properly formatted
  ;; Note that IPersistentMap's may also be complex numbers, thus must be checked as such
  clojure.lang.IPersistentMap (tensor? [o] (if (as-tensor o) true false)))

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
  clojure.lang.BigInt (int? [k] true))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Section 2.6. Extension of the natural protocol
;; #natural
(extend-protocol natural

  ;; Objects are not natural numbers, as a general rule
  Object (natural? [n] false)

  ;; Numbers we can check with clojure's mod
  Number (natural? [k] (and (> k 0) (== (mod k 1) 0)))
  ;; Integer types we can answer with more confidence though...
  Integer (natural? [k] (> k 0))
  Short (natural? [k] (> k 0))
  Long (natural? [k] (> k 0))
  BigInteger (natural? [k] (> k 0))
  Byte (natural? [k] (> k 0))
  java.math.BigInteger (natural? [k] (> k 0))
  clojure.lang.BigInt (natural? [k] (> k 0)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Section 3. Definition of the math type hierarchy
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def math-type-hierarchy
  "math-type-hierarchy is a hierarchy of the mathematical types supported by the core nben.math
   library. Generally speaking, the mathematical multimethods in nben.math dispatch on the math-type
   value of each argument (though this is not always true)."
  (-> (make-hierarchy)
      (derive ::datum ::traversable)
      (derive ::datum ::tensor)
      (derive ::tensor ::complex)
      (derive ::complex ::real)
      (derive ::real ::rational)
      (derive ::rational ::integer)
      (derive ::integer ::natural)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Section 4. General mathematical functions that use the above protocols
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Section 4.1. The Real Number functions
;; #real
(defmacro infinite-sum
  "(infinite-sum iterator term) yields a real number that is estimated by summing the terms formed
     by the term-form when bound under iterator. The iterator must be expressed as follows:
     [iterator-symbol <:from start-value> <:by step-size>] where both the :from and :by arguments
     are optional and will default to 0 and 1 respectively. For example, [k :from 1] will bind k to
     1, 2, 3 ... etc. when evaluating term and error; [k :from -1 :by -1] will bind k to the
     infinite sequence -1, -2, -3 ....
     The term may be expressed as a vector [term-symbol term-expression], in which case the error
     expression, if described, may refer to term-symbol when calculating the error.

   Options:
     :error (default: :automatic) If :automatic, uses the absolute value of the term as the error
       and requires that all terms decrease monotonically in size. Otherwise, may be a function of
       three arguments, the iterator value, its new term, and the new sum including the new term; it
       must yield a positive rational number that decreases monotonically as the sequence proceeds.
     :bind-previous-term (default: nil) If a symbol is given, then binds the symbol to the previous
       term during every evaluation of the term and error; for the first term, the symbol is bound
       to nil.
     :bind-sum (default: nil) If a symbol is given, then binds the total sum, prior to this term, to
       the symbol during evaluation of the term and the error; for the first term, the symbol is
       bound to nil.

   Examples:
     (infinite-sum [k :from 0] (/ 1 (factorial k))) => e"
  [[iterator-sym &{:keys [from by] :or {from 0 by 1}}]
   term
   &{:keys [error bind-sum bind-previous-term] :or {error :automatic}}]
  (let [termsym (if (vector? term) (first term) (gensym "term"))
        termexpr (if (vector? term) (fnext term) term)]
    (cond
     (and bind-sum bind-previous-term)
     `(let [by-arg# ~by
            iter# (iterator :from ~from
                            :to (if (< by-arg# 0) :-infinity :infinity)
                            :by by-arg#)]
        (letfn [(next-step-fn# [i# ~bind-sum ~bind-previous-term]
                 (let [~iterator-sym (first i#)
                       ~termsym ~termexpr]
                   (cons 
                    ~(if (= error :automatic) termsym [termsym error])
                    (lazy-seq (next-step-fn# (next i#)
                                             (+ ~termsym ~bind-sum)
                                             ~termsym)))))]
          (nben.jvm.RealNumber. (lazy-seq (next-step-fn# iter# nil nil)))))
     bind-sum
     `(let [by-arg# ~by
            iter# (iterator :from ~from
                            :to (if (< by-arg# 0) :-infinity :infinity)
                            :by by-arg#)]
        (letfn [(next-step-fn# [i# ~bind-sum]
                 (let [~iterator-sym (first i#)
                       ~termsym ~termexpr]
                   (cons 
                    ~(if (= error :automatic) termsym [termsym error])
                    (lazy-seq (next-step-fn# (next i#) (+ ~termsym ~bind-sum))))))]
          (nben.jvm.RealNumber. (lazy-seq (next-step-fn# iter# nil nil)))))
     bind-previous-term
     `(let [by-arg# ~by
            iter# (iterator :from ~from
                            :to (if (< by-arg# 0) :-infinity :infinity)
                            :by by-arg#)]
        (letfn [(next-step-fn# [i# ~bind-previous-term]
                 (let [~iterator-sym (first i#)
                       ~termsym ~termexpr]
                   (cons 
                    ~(if (= error :automatic) termsym [termsym error])
                    (lazy-seq (next-step-fn# (next i#) ~termsym)))))]
          (nben.jvm.RealNumber. (lazy-seq (next-step-fn# iter# nil nil)))))
     :else
     `(let [by-arg# ~by
            iter# (iterator :from ~from
                            :to (if (< by-arg# 0) :-infinity :infinity)
                            :by by-arg#)]
        (letfn [(next-step-fn# [i#]
                 (let [~iterator-sym (first i#)
                       ~termsym ~termexpr]
                   (cons 
                    ~(if (= error :automatic) termsym [termsym error])
                    (lazy-seq (next-step-fn# (next i#))))))]
          (nben.jvm.RealNumber. (lazy-seq (next-step-fn# iter#))))))))

(defn real-operation
  "(real-operation op error x1 x2 ...) yields a real number that represents the operation applied to
     the arguments x1 x2 etc. The op argument must be a function that accepts a rational number for
     each of the xi. The error must be a function that takes as arguments an error boundary (as
     passed to estimate) and a seq of the real number arguments (x1 x2 ...) and yields either a
     single numerical error bound (to which all the xi must be estimated) or a sequence of error
     bounds to which each individual argument must be estimated (in the order of the xi)."
  [op error & args]
  (cond (nil? args) (arg-err "real-operation cannot be called without arguments")
        (not (ifn? op)) (arg-err "op argument to real-operation must be an ifn")
        (not (ifn? error)) (arg-err "error argument to real-operation must be an ifn")
        (not (every? real? args)) (arg-err "arguments to real-operation must be real")
        :else (real-operator. op error args)))
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Section 4.2. Functions for arithmetic.
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
  ([a b] (cond
          (quotient? a) (cond (quotient? b) (+ a b)
                              (real? b) (real-operation + #(/ %1 (count %2)) a b)
                              (complex? b) (complex-number. (plus (re b) a) (re im))
                              (tensor? b) 
           (if (tensor? b)
             (if (= (length a) (length b))
               (map plus (normal a) (normal b))
               (arg-err "Cannot add tensors of unequal length (" (length a) " and " (lenth b) ")"))
             (map #(plus % b) (normal a)))
           (if (tensor? b)
             (map #(plus % a) (normal b))
             (+ a b))))
  ([a b & more] (reduce plus (plus a b) more)))
(defn pplus
  "pplus is identical to plus except that it maps over tensor addition in parallel using pmap."
  ([] 0)
  ([a] a)
  ([a b] (if (tensor? a)
           (if (tensor? b)
             (if (= (length a) (length b))
               (pmap pplus (normal a) (normal b))
               (arg-err "Cannot add tensors of unequal length (" (length a) " and " (lenth b) ")"))
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
;; Section 5. General mathematicam multimethods that use the above protocols; defmethods
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Convenient redefinitions of basic arithmatic for tensors.



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



;; and functions for creating complex numbers
(defn re-im
  "(re-im a b) yields the complex number a + b*i for real numbers a and b."
  [a b] (complex-number. a b))
(defn arg-abs
  "(arg-abs r theta) yields the complex number r * exp(i * theta) for real r and theta."
  [r theta] (complex-number. (times r (cos theta)) (times r (sin theta))))                               

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tensor.clj, part of nben, a mathematics library for clojure.
;; This file defines the clojure functions for handling tensors; these are intended as a set of
;; minimal functions that can be used to operate efficiently with any tensor format. Ideally, any
;; format can extend this protocol and have a ready tensor class that will operate optimally with
;; all nben math functions..
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

(ns nben.math.tensor)

;;
;; These are just for internal error use
;;
(defn- arg-err [txt] (throw (IllegalArgumentException. txt)))
(defn- not-tensor-err [arg-name fn-name]
  (throw (IllegalArgumentException.
          (str "Argument " arg-name " to function " fn-name " is not a tensor"))))

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


(extend-protocol tensor

  ;; Objects are not tensors:
  Object
  (tensor? [o] false)
  (tensor-time-profile [o] nil)
  (length [o] (not-tensor-err o 'length))
  (normal [o] (not-tensor-err o 'normal))
  (el [o k] (not-tensor-err o 'el))
  (prepend [o q] (not-tensor-err o 'prepend))
  (append [o q] (not-tensor-err o 'append))
  (replace-el [o k q] (not-tensor-err o 'replace-el))

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
  clojure.lang.IPersistentMap
  (tensor? [o]
    (if-let [len (.valAt o :length)]
      (and (integer? len) (>= len 0))
      false))
  (tensor-time-profile [o]
    (when (tensor? o)
      {'length :logarithmic
       'normal :logarithmic
       'el :logarithmic
       'prepend :linear
       'append :logarithmic
       'replace-el :logarithmic}))
  (length [o]
    (let [len (.valAt o :length)]
      (if (and (integer? len) (>= len 0))
        len
        (not-tensor-err o 'length))))
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
        (not-tensor-err o 'normal))))
  (el [o k]
    (let [len (.valAt o :length)]
      (cond         
       (or (not (integer? len)) (< len 0)) (not-tensor-err o 'el)
       (or (not (integer? k)) (< k 0)) (arg-err "argument k to el must be an integer > 0")
       (>= k len) (arg-err "argument " k " is greater than the length of tensor")
       :else (let [val (.valAt o k o)]
               (if (= o val) (.valAt o :default) val)))))
  (prepend [o q]
    (let [len (.valAt o :length)]
      (if (or (not (integer? len)) (< len 0))
        (not-tensor-err o 'prepend)
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
        (not-tensor-err o 'append)
        (if (= (.valAt o :default) q)
          (assoc o :length (inc len))
          (assoc o :length (inc len) len q)))))
  (replace-el [o k q]
    (let [len (.valAt o :length)]
      (if (or (not (integer? len)) (< len 0))
        (not-tensor-err o 'replace-el)
        (if (= (.valAt o :default) q)
          (dissoc o k)
          (assoc o k q))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Below are general tensor functions that use only the above to run optimally.

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

(defn plus
  "(plus a) yields a.
   (plus a b) yields (+ a b) if a and b are not tensors, and the element-wise tensor sum of a and b
     if they are tensors of the same length. If a or b is a tensor, but not both, then yields the
     tensor whose elements are the sum of the scalar with each element of the tensor.
   (plus a1 a2 ... an) yields the sum of all items or tensors reduced using plus."
  ([a] a)
  ([a b] (if (tensor? a)
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

;(defn inner
;  "(inner f u v g) is a generalization of dot that yields the inner product of tensors u and v using
;     function f in place of plus and function g in place of times. The resulting tensor will be of
;     rank (+ (rank u) (rank v) -2)"
;  ([f u v g]
;     (cond (or (not (tensor? u)) (not (tensor? v))) (arg-err "inner can only operate over tensors")
;     
;(defn pinner
;  "pinner is identical to inner except that it maps in parallel over tensors using pmap."
;  )

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
  "(naive-matrix-multiply u v) yields the result of 


                               
                           
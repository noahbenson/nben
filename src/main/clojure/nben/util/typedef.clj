;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; typedef.clj, part of nben, a mathematics library for the JVM.
;; This namespace adds to potemkin's dev-map-type a def-vec-type and dev-set-type.
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

(ns nben.util.typedef
  (:use [potemkin :exclude [def-map-type]])
  (:use [nben.util error]))

(import-vars [potemkin def-map-type])
(defmacro def-set-type
  "(def-set-type ...) is like def-map-type except that it defines a set-like structure. Note that
   the functions that must be overloaded are shown in the following example:
     (def-set-type TestSet [u m]
       (contains? [_ o] (contains? u o))
       ;; get is optional
       ;(get [_ o] (get u o))
       (count [_] (count u))
       (empty [_] #{})
       (seq [_] (seq u))
       (conj [_ o] (TestSet. (conj u o) m))
       (disj [_ o] (TestSet. (disj u o) m))
       (meta [_] m)
       (with-meta [_ mta] (TestSet. u mta))
   Other interfaces may be extended but they must be extended after these funtions are defined."
  [name [& vars] & defs]
  (let [fns (apply hash-map
              (apply concat 
                (map (fn [x] (clojure.lang.MapEntry. (first x) x))
                     (take-while #(and (list? %) (vector? (fnext %))) defs))))
        remainder (nthnext defs (count fns))
        codes (map #(get fns %) '[get conj count empty contains? disj seq meta with-meta])
        [get-code conj-code count-code empty-code contains-code
         disj-code seq-code meta-code with-meta-code] codes]
    (when (some nil? [conj-code count-code empty-code contains-code
                      disj-code seq-code meta-code with-meta-code])
      (arg-err "Set type requires functions:"
               "conj, count, empty, contains?, disj, seq, meta, with-meta, and optionally get"))
    `(deftype ~name ~(vec vars)

       clojure.lang.IPersistentSet
       ~(cons 'disjoin (next disj-code))
       ~(cons 'contains (next contains-code))
       ~(if (nil? get-code)
          `(get [self# k#] (if (contains? self# k#) k# nil))
          get-code)

       clojure.lang.IPersistentCollection
       ~count-code ;;(count [self#] ...)
       ~empty-code ;;(empty [self#] ...)
       ~(cons 'cons (next conj-code)) ;;(cons [self# obj#] ...)
       (equiv [self# obj#]
         (cond (not (instance? java.util.Set obj#)) false
               (not= (.count self#) (count obj#)) false
               :else (loop [s# (.seq self#)]
                       (cond (nil? s#) true
                             (contains? obj# (first s#)) (recur (next s#))
                             :else false))))

       clojure.lang.Seqable
       ~seq-code

       java.io.Serializable
       ;; ****

       java.util.Set
       (toArray [self#] (clojure.lang.RT/seqToArray (.seq self#)))
       (toArray [self# a#] (clojure.lang.RT/seqToPassedArray (.seq self#) a#))
       (add [_ _] (unsupported-err))
       (remove [_ _] (unsupported-err))
       (addAll [_ _] (unsupported-err))
       (clear [_] (unsupported-err))
       (retainAll [_ _] (unsupported-err))
       (removeAll [_ _] (unsupported-err))
       (containsAll [self# c#] (every? #(.contains self# %) (seq c#)))
       (size [self#] (.count self#))
       (isEmpty [self#] (= 0 (.count self#)))
       (iterator [self#] (clojure.lang.SeqIterator. (.seq self#)))
       
       clojure.lang.IHashEq
       ~(or (get fns 'hasheq)
            `(hasheq [self#]
                     (reduce #(+ (* 31 %1) (if (nil? %2) 0 (clojure.lang.Util/hasheq %2)))
                             1
                             (.seq self#))))
       
       clojure.lang.IObj
       ~(cons 'withMeta (next with-meta-code)) ;;(withMeta [_] ...)
       
       clojure.lang.IMeta
       ~meta-code ;;(meta [_] ...)
       
       ;;clojure.lang.IEditableCollection
       ;;(asTransient [_] ) ;;*

       clojure.lang.IFn
       (invoke [self# i#] (.get self# i#))
       (invoke [self# i1# i2#] (arity-err self# 2))
       (invoke [self# i1# i2# i3#] (arity-err self# 3))
       (invoke [self# i1# i2# i3# i4#] (arity-err self# 4))
       (invoke [self# i1# i2# i3# i4# i5#] (arity-err self# 5))
       (invoke [self# i1# i2# i3# i4# i5# i6#] (arity-err self# 6))
       (invoke [self# i1# i2# i3# i4# i5# i6# i7#] (arity-err self# 7))
       (invoke [self# i1# i2# i3# i4# i5# i6# i7# i8#] (arity-err self# 8))
       (invoke [self# i1# i2# i3# i4# i5# i6# i7# i8# i9#] (arity-err self# 9))
       (invoke [self# i1# i2# i3# i4# i5# i6# i7# i8# i9# i19#] (arity-err self# 10))
       (invoke [self# i1# i2# i3# i4# i5# i6# i7# i8# i9# i10# i11#] (arity-err self# 11))
       (invoke [self# i1# i2# i3# i4# i5# i6# i7# i8# i9# i10# i11# i12#] (arity-err self# 12))
       (invoke [self# i1# i2# i3# i4# i5# i6# i7# i8# i9# i10# i11# i12# i13#] (arity-err self# 13))
       (invoke [self# i1# i2# i3# i4# i5# i6# i7# i8# i9# i10# i11# i12# i13# i14#]
         (arity-err self# 14))
       (invoke [self# i1# i2# i3# i4# i5# i6# i7# i8# i9# i10# i11# i12# i13# i14# i15#]
         (arity-err self# 15))
       (invoke [self# i1# i2# i3# i4# i5# i6# i7# i8# i9# i10# i11# i12# i13# i14# i15# i16#]
         (arity-err self# 16))
       (invoke [self# i1# i2# i3# i4# i5# i6# i7# i8# i9# i10# i11# i12# i13# i14# i15# i16# i17#]
         (arity-err self# 17))
       (invoke [self# i1# i2# i3# i4# i5# i6# i7# i8# i9# i10# i11# i12# i13# i14#
                i15# i16# i17# i18#] (arity-err self# 18))
       (invoke [self# i1# i2# i3# i4# i5# i6# i7# i8# i9# i10# i11# i12# i13# i14#
                i15# i16# i17# i18# i19#] (arity-err self# 19))
       (invoke [self# i1# i2# i3# i4# i5# i6# i7# i8# i9# i10# i11# i12# i13# i14#
                i15# i16# i17# i18# i19# i20#] (arity-err self# 20))
       (invoke [self# i1# i2# i3# i4# i5# i6# i7# i8# i9# i10# i11# i12# i13# i14#
                i15# i16# i17# i18# i19# i20# ii#] (arity-err self# 21))
       (applyTo [self# args#]
         (if (= (count args#) 1) (.nth self# (first args#)) (arity-err self# (count args#))))

       Object
       (equals [self# obj#]
         (cond (identical? self# obj#) true
               (not (instance? java.util.Set obj#)) false
               (not= (.count self#) (count obj#)) false
               :else (every? #(.contains self# %) (seq obj#))))
       ~(or (get fns 'hashCode)
            `(hashCode [self#] (apply + (map #(clojure.lang.Util/hash %) (.seq self#)))))
       ~(or (get fns 'toString)
            `(toString [self#] (clojure.lang.RT/printString self#)))

       ~@remainder)))


(defmacro def-vec-type
  "(def-vec-type ...) is like def-map-type except that it defines a vector-like structure. Note
   that the functions that must be overloaded are shown in the following example:
     (def-vec-type TestVec [u m]
       (assoc [_ k v] (TestVec. (assoc u k v) m))
       (conj [_ v] (TestVec. (conj u v) m))
       (count [_] (count u))
       (empty [_] [])
       (pop [_] (TestVec. (pop u) m))
       (meta [_] m)
       (nth [_ k nf] (nth u k nf))
       (with-meta [_ mta] (TestVec. u mta)))
   Other interfaces may be extended but they must be extended after these funtions are defined."
  [name [& vars] & defs]
  (let [fns (apply hash-map
              (apply concat 
                (map (fn [x] (clojure.lang.MapEntry. (first x) x))
                     (take-while #(and (list? %) (vector? (fnext %))) defs))))
        remainder (nthnext defs (count fns))
        codes (map #(get fns %) '[assoc conj count empty pop nth meta with-meta])
        [assoc-code conj-code count-code empty-code pop-code nth-code meta-code with-meta-code]
        codes
        tmp (gensym) tmp2 (gensym) tmp3 (gensym)
        self (gensym) arg1 (gensym) arg2 (gensym)]
    (when (some nil? codes)
      (arg-err "Vector type requires functions:"
               " assoc, cons, count, empty, pop, nth, meta, with-meta"))
    `(deftype ~name ~(vec vars)
       clojure.lang.IPersistentVector
       ~(cons 'assocN (next assoc-code)) ;;(assocN [~self k val] ...)
       ~(cons 'cons (next conj-code)) ;;(cons [~self val] ...)
       (length [~self] (.count ~self))
       
       clojure.lang.Associative
       (containsKey [~self k#] (and (integer? k#) (>= k# 0) (< k# (.count ~self))))
       (entryAt [~self ~arg1] (clojure.lang.MapEntry. ~arg1 (.nth ~self ~arg1)))
       (assoc [~self ~arg1 ~arg2] (.assocN ~self ~arg1 ~arg2))
       
       clojure.lang.IPersistentCollection
       ~count-code ;;(count [~self] ...)
       ~empty-code ;;(empty [_] ...)
       (equiv [~self ~arg1]
         (cond (or (instance? clojure.lang.IPersistentVector ~arg1)
                   (instance? java.util.List ~arg1))
               (if (= (.count ~self) (count ~arg1))
                 (loop [~tmp (.seq ~self), ~tmp2 (seq ~arg1)]
                   (cond
                    (nil? ~tmp) true
                    (clojure.lang.Util/equiv (first ~tmp) (first ~tmp2))
                    (recur (next ~tmp) (next ~tmp2))
                    :else false))
                 false)
               (instance? clojure.lang.Sequential ~arg1)
               (loop [~tmp (.seq ~self), ~tmp2 (clojure.lang.RT/seq ~arg1)]
                 (cond
                  (and (nil? ~tmp) (nil? ~tmp2)) true
                  (or (nil? ~tmp) (nil? ~tmp2)) false
                  (clojure.lang.Util/equiv (first ~tmp) (first ~tmp2))
                  (recur (next ~tmp) (next ~tmp2))
                  :else false))
               :else false))
     
       clojure.lang.Seqable
       (seq [~self] (map #(.nth ~self %) (range (.count ~self))))
     
       clojure.lang.ILookup
       (valAt [~self ~arg1] (.nth ~self ~arg1 nil))
       (valAt [~self ~arg1 ~arg2] (.nth ~self ~arg1 ~arg2))
       
       clojure.lang.Sequential
       
       clojure.lang.IPersistentStack
       (peek [~self] (.nth ~self (dec (.count ~self))))
       ~pop-code ;;(pop [_] ...)
       
       clojure.lang.Reversible
       (rseq [~self] (let [~tmp (.count ~self)] (map #(.nth ~self (dec (- ~tmp %))) (range ~tmp))))
       
       clojure.lang.Indexed
       (nth [~self ~arg1] (let [~tmp (.nth ~self ~arg1 ~self)]
                            (if (identical? ~self ~tmp)
                              (throw (IndexOutOfBoundsException.))
                              ~tmp)))
       ~nth-code ;;(nth [_ k nf] ...)
       
       java.lang.Iterable
       (iterator [~self] (clojure.lang.SeqIterator. (.seq ~self)))
       
       java.util.List
       (add [_ ~arg1] (unsupported-err))
       (add [_ ~arg1 ~arg2] (unsupported-err))
       (addAll [_ ~arg1] (unsupported-err))
       (addAll [_ ~arg1 ~arg2] (unsupported-err))
       (clear [_] (unsupported-err))
       (contains [~self ~arg1]
         (loop [~tmp (.seq ~self)]
           (cond (nil? ~tmp) false
                 (clojure.lang.Util/equiv (first ~tmp) ~arg1) true
                 :else (recur (next ~tmp)))))
       (containsAll [~self ~arg1] (every? #(.contains ~self %) ~arg1))
       (indexOf [~self ~arg1] (loop [~tmp (.seq ~self), ~tmp2 0]
                             (cond (nil? ~tmp) -1
                                   (clojure.lang.Util/equiv (first ~tmp) ~arg1) ~tmp2
                                   :else (recur (next ~tmp) (inc ~tmp2)))))
       (isEmpty [~self] (= 0 (.count ~self)))
       (lastIndexOf [~self ~arg1] (loop [~tmp (dec (.count ~self))]
                             (cond (> 0 ~tmp) -1
                                   (clojure.lang.Util/equiv (.nth ~self ~tmp) ~arg1) ~tmp
                                   :else (recur (dec ~tmp)))))
       (^boolean remove [_ ~arg1] (unsupported-err))
       ;(^Object remove [_ ~arg1] (unsupported-err))
       (removeAll [_ ~arg1] (unsupported-err))
       (retainAll [_ ~arg1] (unsupported-err))
       (set [_ ~arg1 ~arg2] (unsupported-err))
       (size [~self] (.count ~self))
       (subList [~self ~arg1 ~arg2] (clojure.lang.RT/subvec ~self ~arg1 ~arg2))
       (toArray [~self] (clojure.lang.RT/seqToArray (.seq ~self)))
       (toArray [~self ~arg1] (clojure.lang.RT/seqToPassedArray (.seq ~self) ~arg1))
       
       java.util.RandomAccess
       
       java.lang.Comparable
       (compareTo [~self ~arg1]
         (cond (< (.count ~self) (count ~arg1)) -1
               (> (.count ~self) (count ~arg1)) 1
               :else (loop [~tmp2 (.seq ~self), ~tmp3 (seq ~arg1)]
                       (if ~tmp2
                         (let [~tmp (clojure.lang.Util/compare (first ~tmp2) (first ~tmp3))]
                           (if (= ~tmp 0) (recur (next ~tmp2) (next ~tmp3)) ~tmp))
                         0))))
       
       java.io.Serializable
       ;; ****
       
       clojure.lang.IHashEq
       (hasheq [~self]
         (reduce #(+ (* 31 %1) (if (nil? %2) 0 (clojure.lang.Util/hasheq %2))) (.seq ~self)))
       
       clojure.lang.IObj
       ~(cons 'withMeta (next with-meta-code)) ;;(withMeta [_] ...)
       
       clojure.lang.IMeta
       ~meta-code ;;(meta [_] ...)
       
       ;;clojure.lang.IEditableCollection
       ;;(asTransient [_] ) ;;*

       clojure.lang.IFn
       (invoke [~self i#] (.nth ~self i#))
       (invoke [~self i1# i2#] (arity-err ~self 2))
       (invoke [~self i1# i2# i3#] (arity-err ~self 3))
       (invoke [~self i1# i2# i3# i4#] (arity-err ~self 4))
       (invoke [~self i1# i2# i3# i4# i5#] (arity-err ~self 5))
       (invoke [~self i1# i2# i3# i4# i5# i6#] (arity-err ~self 6))
       (invoke [~self i1# i2# i3# i4# i5# i6# i7#] (arity-err ~self 7))
       (invoke [~self i1# i2# i3# i4# i5# i6# i7# i8#] (arity-err ~self 8))
       (invoke [~self i1# i2# i3# i4# i5# i6# i7# i8# i9#] (arity-err ~self 9))
       (invoke [~self i1# i2# i3# i4# i5# i6# i7# i8# i9# i19#] (arity-err ~self 10))
       (invoke [~self i1# i2# i3# i4# i5# i6# i7# i8# i9# i10# i11#] (arity-err ~self 11))
       (invoke [~self i1# i2# i3# i4# i5# i6# i7# i8# i9# i10# i11# i12#] (arity-err ~self 12))
       (invoke [~self i1# i2# i3# i4# i5# i6# i7# i8# i9# i10# i11# i12# i13#] (arity-err ~self 13))
       (invoke [~self i1# i2# i3# i4# i5# i6# i7# i8# i9# i10# i11# i12# i13# i14#]
         (arity-err ~self 14))
       (invoke [~self i1# i2# i3# i4# i5# i6# i7# i8# i9# i10# i11# i12# i13# i14# i15#]
         (arity-err ~self 15))
       (invoke [~self i1# i2# i3# i4# i5# i6# i7# i8# i9# i10# i11# i12# i13# i14# i15# i16#]
         (arity-err ~self 16))
       (invoke [~self i1# i2# i3# i4# i5# i6# i7# i8# i9# i10# i11# i12# i13# i14# i15# i16# i17#]
         (arity-err ~self 17))
       (invoke [~self i1# i2# i3# i4# i5# i6# i7# i8# i9# i10# i11# i12# i13# i14#
                i15# i16# i17# i18#] (arity-err ~self 18))
       (invoke [~self i1# i2# i3# i4# i5# i6# i7# i8# i9# i10# i11# i12# i13# i14#
                i15# i16# i17# i18# i19#] (arity-err ~self 19))
       (invoke [~self i1# i2# i3# i4# i5# i6# i7# i8# i9# i10# i11# i12# i13# i14#
                i15# i16# i17# i18# i19# i20#] (arity-err ~self 20))
       (invoke [~self i1# i2# i3# i4# i5# i6# i7# i8# i9# i10# i11# i12# i13# i14#
                i15# i16# i17# i18# i19# i20# ii#] (arity-err ~self 21))
       (applyTo [~self ~arg1]
         (if (= (count ~arg1) 1) (.nth ~self (first ~arg1)) (arity-err ~self (count ~arg1))))

       java.util.concurrent.Callable
       (call [~self] (.invoke ~self))

       java.lang.Runnable
       (run [~self] (.invoke ~self))
       
       Object
       (equals [~self ~arg1]
         (cond (or (instance? clojure.lang.IPersistentVector ~arg1)
                   (instance? java.util.List ~arg1))
               (if (= (.count ~self) (count ~arg1))
                 (loop [~tmp (.seq ~self), ~tmp2 (seq ~arg1)]
                   (cond
                    (nil? ~tmp) true
                    (clojure.lang.Util/equals (first ~tmp) (first ~tmp2))
                    (recur (next ~tmp) (next ~tmp2))
                    :else false))
                 false)
               (instance? clojure.lang.Sequential ~arg1)
               (loop [~tmp (.seq ~self), ~tmp2 (clojure.lang.RT/seq ~arg1)]
                 (cond
                  (and (nil? ~tmp) (nil? ~tmp2)) true
                  (or (nil? ~tmp) (nil? ~tmp2)) false
                  (clojure.lang.Util/equals (first ~tmp) (first ~tmp2))
                  (recur (next ~tmp) (next ~tmp2))
                  :else false))
               :else false))
       (hashCode [~self] (reduce #(+ (* 31 %1) (if (nil? %2) 0 (.hashCode %2))) 1 (.seq ~self)))
       (toString [~self] (clojure.lang.RT/printString ~self))

       ~@remainder)))

(defmacro defmultipro
  "The defmultipro macro is similar to both the defmulti and the defprotocol macros. The motivation
   of the macro is to make it simpler to provide default behavior for a protocol depending on an
   abstract feature of the first argument to the methods. The syntax for defmultipro is:
     * (defmultipro protocol-name doc-string? triage-fn impl-fn method-signatures...)
   This is exactly like the defprotocol macro except for the addition of the triage-fn and the
   impl-fn arguments. With a defmultipro protocol, whenenver an object is passed to any of its
   methods, it is run through the triage-fn, the result of which is passed to the impl-fn, which
   must return a map of method mappings appropriate for the extend macro. All objects of a
   particular class will only be triaged once, the first time any of them are triaged; after that
   point, the protocol will be implemented directly.

   Examples:
     (defmultipro abstract-dictionary
       #(cond (map? %) :map, (set? %) :set, (vector? %) :vec)
       {:map {:abstract-dictionary? (fn [_] true), :lookup get}
        :set {:abstract-dictionary? (fn [_] true), :lookup get}
        :vec {:abstract-dictionary? (fn [_] true), :lookup nth}
        nil  {:abstract-dictionary? (fn [_] false), :lookup (fn [_ _] nil)}}
       (abstract-dictionary? [_] \"yields true if the object is an abstract dictionary\")
       (lookup [_ k] \"yields the val associated with k or nil\"))"
  [protocol-name & args]
  (let [[docstr triage-fn impl-fn & methods] (if (string? (first args)) args (cons nil args))
        triage-sym (gensym)]
    `(do (defprotocol ~protocol-name
           ~@(when docstr [docstr])
           ~@methods)
         (let [triage-fn# ~triage-fn, impl-fn# ~impl-fn]
           (letfn [(~triage-sym [obj# fn-name# args#]
                    (if (identical? (class obj#) Object)
                      (if-let [objfns# (impl-fn# Object)]
                        (apply (fn-name# objfns#) obj# args#)
                        (arg-err "multi-pro protocol " ~protocol-name
                                 " not implemented for class Object"))
                      (if-let [impl-map# (impl-fn# (triage-fn# obj#))]
                        (do (extend (class obj#) ~protocol-name impl-map#)
                            (apply (get impl-map# fn-name#) obj# args#))
                        (arg-err "multi-pro protocol " ~protocol-name
                                 " not implemented for class " (class obj#)))))]
             (extend Object
               ~protocol-name
               ~(reduce (fn [m mth]
                          (let [nm (keyword (first mth))]
                            (assoc m nm `(fn [obj# & more#] (~triage-sym obj# ~nm more#)))))
                        {}
                        methods)))))))

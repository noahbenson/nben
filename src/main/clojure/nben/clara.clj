;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; clara.clj
;; A wrapper around clara for nben, which enforces and enables simple clara rules with maps.
;; By Noah C. Benson

(ns nben.clara
  (:use nben.util)
  (:require [clara.rules :as cr]
            [clara.rules.dsl :as dsl]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; All facts must have a fact type, which is stored in the map's metadata and is based on the keys
;; of the map. This should be automatically attached to facts as they are inserted, but the code
;; for managing this is here.
(defonce ^:private fact-types (atom {}))
(defn- fact-types-swap [ft cols nm] (if (contains? ft cols) ft (assoc ft cols nm)))
(defn- fact-type-name [cols]
  (->> (sort (map name cols))
       (map #(-> [\< (count %) \> %]))
       (apply concat) (apply str) keyword))
(defn- fact-type-lookup [cols]
  (let [cols (cond (set? cols) cols (map? cols) (key-set cols) :else (set cols))
        e    (find @fact-types cols)]
    (if e
      (val e)
      (let [nm (fact-type-name cols)
            ft (swap! fact-types fact-types-swap cols nm)]
        (get ft cols)))))
(defn fact-type
  "(fact-type map) yields the fact-type object for the given map."
  [m] (let [mm (meta m)]
        (or (get mm ::type)
            (fact-type-lookup m))))
(defn annotate-fact-type
  "(annotate-fact-type map) yields a duplicate of map in which the fact type is included in the
     map's meta-data."
  [m] (let [mm (meta m)]
        (if (keyword? (get mm ::type))
          m
          (with-meta m (assoc mm ::type (fact-type-lookup m))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Below are the wrappers around the clara functions and macros.

;; defrule ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(declare parse-map-conditions)
(defmacro defrule
  "(defrule name body...) defines a map-base rule for clara and yields its name. This is a
     syntactical wrapper around clara's defrule and uses a slightly modified domain specific
     language."
  [name & body]
  ;; start by building up the maps (conditions) and statements (tests)
  (let [[docstr body] (if (string? (first body)) [(first body) (next body)] [nil body])
        {:keys [lhs rhs]} (dsl/split-lhs-rhs body)
        [conds tests] (parse-map-conditions lhs)]
    `(cr/defrule ~name
       ~@(concat (if docstr (cons docstr conds) conds)
                 tests)
       =>
       ~rhs)))
(defn- wildcard? [obj] (and (symbol? obj) (= \? (first (name obj)))))
(defn- is-?? [obj] (and (symbol? obj) (= "?" (name obj))))
(defn- is-binder? [obj] (and (symbol? obj) (= "<-" (name obj))))
(defn parse-map-fact [q]
  (let [[tok0 & q] q, tok1 (first q)
        [name fact nextok q] (if (is-binder? tok1)
                               (if-not (symbol? tok0)
                                 (arg-err "binding vars in facts must be symbols")
                                 (let [q (next q)] [tok0 (first q) (fnext q) (next q)]))
                               [nil tok0 tok1 q])
        bind (gensym)
        fact (cond (map? fact) fact, (set? fact) (apply hash-map (interleave fact fact)),
                   (coll? fact) (apply hash-map (interleave (range) (seq fact)))
                   :else (arg-err "facts must be maps or simple collections"))
        ;; here we build up the conditions implicit in the map
        reqs (->> (filter (comp not is-?? val) fact)
                  (map (fn [[k v]] `(= ~v (get ~bind ~k))))
                  (reduce conj! (transient []))
                  (persistent!))]
    [(cond (= nextok :from) (arg-err "cannot parse fact out of accumulator")
           name  `[~(fact-type fact) ~[bind] ~@(cons `(= ~name ~bind) reqs)]
           :else `[~(fact-type fact) ~[bind] ~@reqs])
     q]))
(def- boolean-expr-keys #{:or :and :not})
(defn parse-map-expr [q]
  (if-not (vector? (first q))
    (parse-map-fact q)
    (let [[[f & more] & nextq] q]
      (if-not (contains? boolean-expr-keys f)
        (arg-err "boolean sub-expressions must start with :or, :and, or :not")
        [(loop [q more, conds nil]
           (if q
             (let [[c q] (parse-map-expr q)] (recur q (cons c conds)))
             (vec (cons f conds))))
         nextq]))))
(defn parse-map-condition [q]
  (let [[tok0 tok1 tok2 tok3] (take 4 q)]
    (if (= tok3 :from)
      ;; this is an accumulator
      (if-not (is-binder? tok1)
        (arg-err "Accumulators must be of the form <sym> <- <accum> :from <fact>")
        (let [[fact nextq] (parse-map-fact (nthnext q 4))]
          [`[~tok0 <- ~tok2 :from ~fact] nextq]))
      ;; this is just a fact or expression
      (parse-map-expr q))))
(defn parse-map-conditions [args]
  (loop [conds [], tests [], q args]
    (cond (nil? q)            [conds (cond (empty? tests) []
                                           (next tests)   `[[:test (and ~@tests)]]
                                           :else          `[[:test ~@tests]])]
          (= (first q) :test) (recur conds (conj tests (fnext q)) (nnext q))
          :else               (let [[p nq] (parse-map-condition q)]
                                (recur (conj conds p) tests nq)))))

;; defquery ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro defquery
  "(defquery name [args...] conditions...) defines a query with the given name and yields name. The
     format for defining conditions is like in this namespace's defrule macro. This is a wrapper
     around clara.rules/defquery."
  [name & args]
  (let [[docstr args] (if (string? (first args)) [(first args) (next args)] [nil args])
        [params args] [(first args) (next args)]
        [conds tests] (parse-map-conditions args)]
    `(cr/defquery ~name
       ~@(if docstr [docstr params] [params])
       ~@conds
       ~@tests)))

;; query ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(import-fn clara.rules/query)
    
;; insert ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn insert
  "(insert session facts...) yields a duplicate of session in which the given facts have been
     inserted. This is a wrapper around clara.rules/insert."
  [session & facts]
  (apply cr/insert session (map annotate-fact-type facts)))

;; insert! ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn insert!
  "(insert! facts...) ensures that the firing of a rule will insert the given facts. This is a
     wrapper around clara.rules/insert!."
  [& facts]
  (apply cr/insert! (map annotate-fact-type facts)))

;; retract ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn retract
  "(retract session facts...) yields a duplicate of session in which the given facts have been
     retracted. This is a wrapper around clara.rules/retract."
  [session & facts]
  (apply cr/retract session (map annotate-fact-type facts)))

;; retract! ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn retract!
  "(retract! facts...) ensures that the firing of a rule will retarct the given facts. This is a
     wrapper around clara.rules/retract!."
  [& facts]
  (apply cr/retract! (map annotate-fact-type facts)))

;; insert-all ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn insert-all
  "(insert-all session facts) yields a duplicate of session in which the given seq of facts has been
     inserted. This is a wrapper around clara.rules/insert-all."
  [session facts]
  (apply cr/insert session (map annotate-fact-type facts)))

;; fire-rules ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(import-fn cr/fire-rules)

;; insert-unconditional! ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn insert-unconditional!
  "(insert-unconditional! facts...) ensures that the firing of a rule will insert the given facts
     without condition, i.e. such that the retraction of the rule will not retract the facts. This
      is a wrapper around clara.rules/insert-unconditional!."
  [& facts]
  (apply cr/insert-unconditional! (map annotate-fact-type facts)))

;; defsession ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro defsession
  "(defsession name sources-and-options...) is equivalent to (clara.rules/defsession...) except that
     the session is constructed for use with maps only. The :fact-type-fn option is not supported."
  [name & sources-and-options]
  `(cr/defsession ~name ~@sources-and-options :fact-type-fn fact-type))

;; mk-session ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro mk-session
  "(mk-session args...) yields a new session using the given arguments. This is equivalent to the
     function clara.rules/mk-session but does not support the :fact-type-fn argument and constructs
     a session for use with maps."
  [& args]
  `(cr/mk-session ~@args :fact-type-fn fact-type))

;; accumulate ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(import-fn cr/accumulate)

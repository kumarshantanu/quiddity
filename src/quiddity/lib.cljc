;   Copyright (c) Shantanu Kumar. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file LICENSE at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.


(ns quiddity.lib
  "Library of pre-built functions and evaluators. Useful for providing an evaluation 'environment'."
  (:require
    [quiddity.core :as core]
    [quiddity.internal :as i]))


;;----- evaluators | unsupported | special forms and macros that use them -----


(def unsupported (let [no-support-for (fn [msg]
                                        (core/make-evaluator (fn [& _]
                                                               (i/*error-handler* (str "No support for " msg)))))]
                   {;; assertion
                    :assert  (no-support-for "`assert`")
                    ;; def, var, binding
                    :binding (no-support-for "`def`, `var`, `binding`")
                    :def     (no-support-for "`def`, `var`, `binding`")
                    :var     (no-support-for "`def`, `var`, `binding`")
                    ;; loop, recur
                    :loop    (no-support-for "`loop` and `recur`")
                    :recur   (no-support-for "`loop` and `recur`")
                    ;; try, catch, finally
                    :try     (no-support-for "`try`, `catch`, `finally`")
                    :catch   (no-support-for "`try`, `catch`, `finally`")
                    :finally (no-support-for "`try`, `catch`, `finally`")}))


;;----- evaluators | special forms | http://clojure.org/special_forms -----


(defn e-if
  "Evaluator re-implementation of the `if` special form."
  ([maps test then]
    (if (core/evaluate test maps)
      (core/evaluate then maps)))
  ([maps test then else]
    (if (core/evaluate test maps)
      (core/evaluate then maps)
      (core/evaluate else maps))))


(defn e-do
  "Evaluator re-implementation of the `do` special form."
  [maps & forms]
  (last (map #(core/evaluate % maps) forms)))


(defn e-quote
  "Evaluator re-implementation of the `quote` speial form."
  [maps x]
  x)


(def special-forms {:if    (core/make-evaluator e-if)
                    :do    (core/make-evaluator e-do)
                    :quote (core/make-evaluator e-quote)})


;;----- destructuring helper | http://clojure.org/special_forms -----


(defn i-destructure
  "Destructure `value` into `local` and return a map of keywordized-symbols to
  corresponding values."
  [maps local value]
  (let [ds  (partial i-destructure maps)
        rmm (fn [f coll & more] (reduce merge {} (apply map f coll more)))
        afa #(i/*error-handler*
               "Unsupported binding form, only :as can follow & parameter")
        ubf #(i/*error-handler* (str "Unsupported binding form: " %))]
    (cond
      (= '_ local)    {}
      (symbol? local) {(keyword local) value}
      (vector? local) (let [n-locals  (take-while (comp not #{'& :as}) local)
                            n-count   (count n-locals)
                            base-env  (rmm ds n-locals (->> (repeat nil)
                                                         (concat (seq value))
                                                         (take n-count)))
                            [a s b t] (nthnext local n-count)
                            addn-env (condp = a
                                       nil {}
                                       '&  (merge
                                             (let [vs (nthnext value n-count)]
                                               (ds s vs))
                                             (if (= b :as) (ds t value)
                                               (when b (afa))))
                                       :as (ds s value))]
                        (merge base-env addn-env))
      (map? local)    (if-let [bs (->> (keys local)
                                    (filter keyword?)
                                    (filter (comp not #{:keys :strs :syms :or :as}))
                                    seq)]
                        (ubf (first bs))
                        (let [value (if (associative? value) value
                                      (apply array-map value))
                              r-env (rmm (fn [[k v]]
                                           (ds k (core/evaluate v maps)))
                                         (:or local))
                              ->env (fn [f coll]
                                      (let [g #(let [k (f %)
                                                     c (if (contains? value k)
                                                         value r-env)]
                                                 (ds % (get c k)))]
                                          (rmm g coll)))
                              l-env (->env local   (-> (comp not keyword?)
                                                     (filter (keys local))))
                              k-env (->env keyword (:keys local))
                              t-env (->env str     (:strs local))
                              y-env (->env symbol  (:syms local))
                              a-env (let [as (:as local)]
                                      (when as (ds as value)))]
                          (merge l-env k-env t-env y-env a-env)))
      :otherwise      (ubf local))))


;;----- evaluators | macros | http://clojure.org/macros -----


(defn e-if-not
  "Evaluator re-implementation of the `if-not` macro."
  ([maps test then]
    (if-not (core/evaluate test maps)
      (core/evaluate then maps)))
  ([maps test then else]
    (if-not (core/evaluate test maps)
      (core/evaluate then maps)
      (core/evaluate else maps))))


(defn e-when
  "Evaluator re-implementation of the `when` macro."
  [maps test & forms]
  (when (core/evaluate test maps)
    (apply e-do maps forms)))


(defn e-when-not
  "Evaluator re-implementation of the `when-not` macro."
  [maps test & forms]
  (when-not (core/evaluate test maps)
    (apply e-do maps forms)))


(defn e-while
  "Evaluator re-implementation of the `while` macro."
  [maps test & forms]
  (while (core/evaluate test maps)
    (apply e-do maps forms)))


(defn e-let
  "Evaluator re-implementation of the `let` macro."
  [maps bindings & forms] {:pre [(seq bindings)
                                 (even? (count bindings))]}
  (let [[k v & more] bindings
        kv-map       (i-destructure maps k (core/evaluate v maps))]
    (if (seq more)
      (apply e-let (cons kv-map maps) more forms)
      (apply e-do  (cons kv-map maps) forms))))


(defn e-if-let
  "Evaluator re-implementation of the `if-let` macro."
  ([maps binding then]
    (e-if-let binding then nil))
  ([maps binding then else] {:pre [(vector? binding) (= 2 (count binding))]}
    (if-let [v (core/evaluate (second binding) maps)]
      (e-let maps [(first binding) v] then)
      (e-do  maps else))))


(defn e-when-let
  "Evaluator re-implementation of the `when-let` macro."
  [maps binding & forms] {:pre [(vector? binding) (= 2 (count binding))]}
  (when-let [v (core/evaluate (second binding) maps)]
    (apply e-let maps [(first binding) v] forms)))


(defn e-for-each
  "Basic evaluator re-implementation of the `for` macro, where :let, :when and
  :while binding forms are not supported."
  [maps bindings form] {:pre [(seq bindings)
                              (even? (count bindings))]}
  (let [[k v & more] bindings
        realized-v   (core/evaluate v maps)]
    (if (seq more)
      (apply concat
             (for [each realized-v]
               (e-for-each (cons (i-destructure maps k each) maps) more form)))
      (for [each realized-v]
        (core/evaluate form (cons (i-destructure maps k each) maps))))))


(defn e-cond
  "Re-implementation of the `cond` macro as an evaluator."
  [maps & forms] {:pre [(even? (count forms))]}
  (loop [[test then & more] forms]
    (if (core/evaluate test maps)
      (core/evaluate then maps)
      (when (seq more)
        (recur more)))))


(defn e-condp
  "Re-implementation of the `condp` macro as an evaluator."
  [maps pred-form expr-form & clauses]
  (let [pred (core/evaluate pred-form maps)
        expr (core/evaluate expr-form maps)
        nmce #(i/*error-handler* (str "No matching clause: " expr))
        chop #(if (and (= :>> (second %)) (>= (count %) 3))
                (let [[k _ v & more] %] [k true  v more])
                (let [[k   v & more] %] [k false v more]))]
    (if (seq clauses)
      (loop [[k u? v more] (chop clauses)]
        (let [texpr (core/evaluate k maps)
              match (pred texpr expr)]
          (if match (let [value (core/evaluate v maps)]
                      (if u? (value match) value))
            (if (< (count more) 2)
              (if (empty? more) (nmce)
                (core/evaluate (first more) maps))
              (recur (chop more))))))
      (nmce))))


(defn e-case
  "Re-implementation of the `case` macro as an evaluator."
  [maps expr-form & clauses]
  (let [expr (core/evaluate expr-form maps)
        exp= (partial = expr)
        nmce #(i/*error-handler* (str "No matching clause: " expr))]
    (if (seq clauses)
      (loop [[k v & more] clauses]
        (cond (and (not (list? k)) (exp= k)) (core/evaluate v maps)
              (and (list? k) (some exp= k))  (core/evaluate v maps)
              (empty? more)      (nmce)
              (= 1 (count more)) (core/evaluate (first more) maps)
              :otherwise         (recur more)))
      (nmce))))


(defn e-and
  "Re-implementation of the `and` macro as an evaluator."
  [maps & forms]
  (reduce #(and %1 (core/evaluate %2 maps)) true forms))


(defn e-or
  "Re-implementation of the `or` macro as an evaluator."
  [maps & forms]
  (reduce #(or %1 (core/evaluate %2 maps)) nil forms))


(defn e->
  "Re-implementation of the `->` (thread first) macro as an evaluator."
  [maps x & forms]
  (loop [value (core/evaluate x maps)
         forms forms]
    (if (empty? forms)
      value
      (let [[form-1 & more] forms]
        (recur (if (or (list? form-1) (seq? form-1))
                 (let [[f & args] form-1]
                   (apply (core/evaluate f maps) value
                          (i/realize-coll args maps core/evaluate)))
                 ((core/evaluate form-1 maps) value))
               more)))))


(defn e->>
  "Re-implementation of the `->>` (thread last) macro as an evaluator."
  [maps x & forms]
  (loop [value (core/evaluate x maps)
         forms forms]
    (if (empty? forms)
      value
      (let [[form-1 & more] forms]
        (recur (if (or (list? form-1) (seq? form-1))
                 (let [[f & args] form-1]
                   (apply (core/evaluate f maps)
                          (concat (i/realize-coll args maps core/evaluate) [value])))
                 ((core/evaluate form-1 maps) value))
               more)))))


(defn e-fn
  "Re-implementation of the `fn` (and `fn*`) macro as an evaluator."
  [maps fn-name & more]
  (if-not (symbol? fn-name)
    (apply e-fn maps '_ fn-name more)
    (if-not (list? (first more))
      (e-fn maps fn-name (apply list more))
      ;; here we have "arg-vector followed-by body" as a list
      (let [variadic -1
            err-hand i/*error-handler*
            va-error #(err-hand
                        "Can't have fixed arity function with more params than variadic function")
            split-fn (fn [[args-vec & body]] ;split fn decl into [min-argc spec]
                       (when-not (vector? args-vec)
                         (err-hand
                           (str "Expected arg vector, found " (pr-str args-vec))))
                       (let [n (count (take-while (partial not= '&) args-vec))
                             v (some (partial = '&) args-vec)]
                         [(if v variadic n) {:args-vec args-vec
                                             :min-argc n
                                             :body body}]))
            m-bodies (reduce (fn [m decl]
                               (let [[n spec] (split-fn decl)
                                     n (long n)]
                                 (when (contains? m n)
                                   (err-hand
                                     (i/sformat "Can't have %s: %s"
                                       (if (neg? n)
                                         "more than 1 variadic overload"
                                         "2 overloads with same arity")
                                       (pr-str more))))
                                 (if (neg? n)
                                   (when (some #(< (long (:min-argc spec)) (long %))
                                               (remove neg? (keys m)))
                                     (va-error))
                                   (when (and (contains? m variadic)
                                              (> n (long (get m variadic))))
                                     (va-error)))
                                 (merge m {n spec})))
                             {} more)
            maps-atm (atom maps)
            return-f (fn [& args]
                       (let [n (count args)
                             a @maps-atm
                             h #(let [spec (get m-bodies %)]
                                  (binding [i/*error-handler* err-hand]
                                    (apply e-do
                                           (-> a
                                             (i-destructure (:args-vec spec) args)
                                             (cons a))
                                           (:body spec))))]
                         (cond
                           ;; arity match
                           (contains? m-bodies n)
                           (h n)
                           ;; variadic match
                           (and (contains? m-bodies variadic)
                                (>= n (long (:min-argc (get m-bodies variadic)))))
                           (h variadic)
                           ;; no match
                           :otherwise
                           (err-hand
                             (i/sformat "Wrong number of args (%d) passed to: %s"
                               n fn-name)))))]
        (swap! maps-atm (partial cons (i-destructure maps fn-name return-f)))
        return-f))))


(def macros (let [let-evaluator (core/make-evaluator e-let)
                  fn-evaluator  (core/make-evaluator e-fn)]
              {:if-not   (core/make-evaluator e-if-not)
               :when     (core/make-evaluator e-when)
               :when-not (core/make-evaluator e-when-not)
               :while    (core/make-evaluator e-while)
               :let      let-evaluator
               :let*     let-evaluator
               :if-let   (core/make-evaluator e-if-let)
               :when-let (core/make-evaluator e-when-let)
               :for-each (core/make-evaluator e-for-each)
               :cond     (core/make-evaluator e-cond)
               :condp    (core/make-evaluator e-condp)
               :case     (core/make-evaluator e-case)
               :and      (core/make-evaluator e-and)
               :or       (core/make-evaluator e-or)
               :->       (core/make-evaluator e->)
               :->>      (core/make-evaluator e->>)
               :fn       fn-evaluator
               :fn*      fn-evaluator
               }))


;;----- functions -----


(def fns {:clojure.core/deref deref  ; for Clojure reader
          :deref deref               ; for ClojureScript reader
          })


;; ----- everything merged -----


(def all (merge unsupported special-forms macros fns))

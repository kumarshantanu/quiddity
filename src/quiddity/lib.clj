(ns quiddity.lib
  (:require [quiddity.core :as core]))


;;----- evaluators | unsupported | special forms and macros that use them -----


(defn no-support-for
  [msg] {:pre [(string? msg)]}
  (core/make-evaluator (fn [& _]
                         (core/*error-handler* (str "No support for " msg)))))


(def unsupported {;; assertion
                  :assert  (no-support-for "`assert`")
                  ;; def, var, binding
                  :binding (no-support-for "`def`, `var`, `binding`")
                  :def     (no-support-for "`def`, `var`, `binding`")
                  :var     (no-support-for "`def`, `var`, `binding`")
                  ;; creating functions
                  :fn      (no-support-for "creating functions")
                  :fn*     (no-support-for "creating functions")
                  ;; loop, recur
                  :loop    (no-support-for "`loop` and `recur`")
                  :recur   (no-support-for "`loop` and `recur`")
                  ;; try, catch, finally
                  :try     (no-support-for "`try`, `catch`, `finally`")
                  :catch   (no-support-for "`try`, `catch`, `finally`")
                  :finally (no-support-for "`try`, `catch`, `finally`")})


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
        afa #(core/*error-handler*
               "Unsupported binding form, only :as can follow & parameter")
        ubf #(core/*error-handler* (str "Unsupported binding form: " %))]
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
                                               (ds s (if (map? s) ; '& {}' form
                                                       (apply array-map vs)
                                                       vs)))
                                             (if (= b :as) (ds t value)
                                               (when b (afa))))
                                       :as (ds s value))]
                        (merge base-env addn-env))
      (map? local)    (if-let [bs (->> (keys local)
                                    (filter keyword?)
                                    (filter (comp not #{:keys :strs :syms :or :as}))
                                    seq)]
                        (ubf (first bs))
                        (let [r-env (rmm (fn [[k v]]
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


(defn e-and
  "Evaluator re-implementation of the `and` macro."
  [maps & forms]
  (reduce #(and %1 (core/evaluate %2 maps)) true forms))


(defn e-or
  "Evaluator re-implementation of the `or` macro."
  [maps & forms]
  (reduce #(or %1 (core/evaluate %2 maps)) nil forms))


(def macros {:if-not   (core/make-evaluator e-if-not)
             :when     (core/make-evaluator e-when)
             :when-not (core/make-evaluator e-when-not)
             :let      (core/make-evaluator e-let)
             :if-let   (core/make-evaluator e-if-let)
             :when-let (core/make-evaluator e-when-let)
             :for-each (core/make-evaluator e-for-each)
             :and      (core/make-evaluator e-and)
             :or       (core/make-evaluator e-or)})


;;----- functions -----


;; ----- everything merged -----

(def all (merge unsupported special-forms macros))
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
  "Implementation of the `if` special form."
  ([maps test then]
    (if (core/evaluate test maps)
      (core/evaluate then maps)))
  ([maps test then else]
    (if (core/evaluate test maps)
      (core/evaluate then maps)
      (core/evaluate else maps))))


(defn e-do
  "Implementation of the `do` special form."
  [maps & forms]
  (last (map #(core/evaluate % maps) forms)))


(defn e-quote
  "Implementation of the `quote` speial form."
  [maps x]
  x)


(def special-forms {:if    (core/make-evaluator e-if)
                    :do    (core/make-evaluator e-do)
                    :quote (core/make-evaluator e-quote)})


;;----- evaluators | macros | http://clojure.org/macros -----


(defn e-if-not
  "Implementation of the `if-not` macro."
  ([maps test then]
    (if-not (core/evaluate test maps)
      (core/evaluate then maps)))
  ([maps test then else]
    (if-not (core/evaluate test maps)
      (core/evaluate then maps)
      (core/evaluate else maps))))


(defn e-when
  "Implementation of the `when` macro."
  [maps test & forms]
  (when (core/evaluate test maps)
    (apply e-do maps forms)))


(defn e-when-not
  "Implementation of the `when-not` macro."
  [maps test & forms]
  (when-not (core/evaluate test maps)
    (apply e-do maps forms)))


(defn e-let
  "Implementation of the `let` macro. No destructuring support."
  [maps bindings & forms] {:pre [(seq bindings)
                                 (even? (count bindings))
                                 (every? symbol?
                                         (map first (partition 2 bindings)))]}
  (let [[k v & more] bindings
        kv-map       {(keyword k) (core/evaluate v maps)}]
    (if (seq more)
      (apply e-let (cons kv-map maps) more forms)
      (apply e-do  (cons kv-map maps) forms))))


(defn e-for-each
  "Basic implementation of the `for` macro, where :let, :when and :while
  binding forms are not supported."
  [maps bindings form] {:pre [(seq bindings)
                              (even? (count bindings))
                              (every? symbol?
                                      (map first (partition 2 bindings)))]}
  (let [[k v & more] bindings
        realized-k   (keyword k)
        realized-v   (core/evaluate v maps)
        kv-map       {realized-k realized-v}]
    (if (seq more)
      (apply concat
             (for [each realized-v]
               (e-for-each (cons {realized-k each} maps) more form)))
      (for [each realized-v]
        (core/evaluate form (cons {realized-k each} maps))))))


(defn e-and
  "Implementation of the `and` macro."
  [maps & forms]
  (reduce #(and %1 (core/evaluate %2 maps)) true forms))


(defn e-or
  [maps & forms]
  (reduce #(or %1 (core/evaluate %2 maps)) nil forms))


(def macros {:if-not   (core/make-evaluator e-if-not)
             :when     (core/make-evaluator e-when)
             :when-not (core/make-evaluator e-when-not)
             :let      (core/make-evaluator e-let)
             :for-each (core/make-evaluator e-for-each)
             :and      (core/make-evaluator e-and)
             :or       (core/make-evaluator e-or)})


;;----- functions -----


;; ----- everything merged -----

(def all (merge unsupported special-forms macros))
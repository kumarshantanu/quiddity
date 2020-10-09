;   Copyright (c) Shantanu Kumar. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file LICENSE at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.


(ns quiddity.core
  "Core evaluation functions."
  (:require
    [quiddity.internal :as i]))


(def ^{:dynamic true
       :doc "Error handler that accepts error message and deals with it"}
      *error-handler* (fn [message]
                        (assert (not "Must override *error-handler* before use"))))


(defn env-get
  "Look up the given key in a collection of supplied maps, and return the value. Absence of the key in all maps amounts
  to an error."
  [k maps]
  (let [r (some (fn [m]
                  (or (and (symbol? k)
                           (let [kw (keyword k)]
                             (and (contains? m kw) [(get m kw)])))
                      (and (contains? m k)         [(get m k)])))
                maps)]
    (if r (first r)
      (*error-handler* (i/sformat "No such key '%s' in env keys %s" (str k)
                         (pr-str (map keys maps)))))))


(defn evaluator?
  "Return `true` if given argument is an evaluator, `false` otherwise."
  [f]
  (and (vector? f)
       (true? (:quiddity-evaluator? (meta f)))))


(defn make-evaluator
  "Create a tagged evaluator."
  [x]
  (if (evaluator? x) x
    (->> {:quiddity-evaluator? true}
      (with-meta [x]))))


(defn evaluate
  "Evaluate S-expression using specified env maps."
  ([form maps]
    {:pre [(every? map? maps)]}
    (cond
      ;; symbol, hence lookup in env
      (symbol? form)    (env-get form maps)
      ;; function call
      (and (or (list? form)
               (seq? form))
           (seq form))  (let [func (evaluate (first form) maps)
                              tail (rest form)]
                          (if (evaluator? func)
                            (apply (first func) maps tail)
                            (apply func (i/realize-coll tail maps evaluate))))
      ;; any collection
      (coll? form)      (i/realize-coll form maps evaluate)
      ;; constant
      :otherwise        form))
  ([form maps error-handler]
    {:pre [(fn? error-handler)]}
    (binding [*error-handler* error-handler]
      (evaluate form maps))))

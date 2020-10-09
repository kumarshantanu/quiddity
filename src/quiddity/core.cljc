(ns quiddity.core
  #?(:cljs (:require
             [goog.string :as gstring]
             [goog.string.format])))


(def ^{:dynamic true
       :doc "Error handler that accepts error message and deals with it"}
      *error-handler* (fn [message]
                        (assert (not "Must override *error-handler* before use"))))


(defn env-get
  [k maps]
  (let [r (some (fn [m]
                  (or (and (symbol? k)
                           (let [kw (keyword k)]
                             (and (contains? m kw) [(get m kw)])))
                      (and (contains? m k)         [(get m k)])))
                maps)]
    (if r (first r)
      (*error-handler* (#?(:cljs gstring/format :clj format)
                         "No such key '%s' in env keys %s" (str k)
                         (pr-str (map keys maps)))))))


(declare evaluate)


(defn realize-coll
  [form maps]
  (if (map? form)
    (let [ks (realize-coll (keys form) maps)
          vs (realize-coll (vals form) maps)]
      (zipmap ks vs))
    (let [coll-fn (cond
                    (vector? form) vec
                    (set? form)    set
                    :otherwise     (partial apply list))]
      (coll-fn (map #(evaluate % maps) form)))))


(defn evaluator?
  [f]
  (and (vector? f)
       (true? (:quiddity-evaluator? (meta f)))))


(defn make-evaluator
  [x]
  (if (evaluator? x) x
    (->> {:quiddity-evaluator? true}
      (with-meta [x]))))


(defn evaluate
  "Evaluate S-expression using specified env `maps`"
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
                            (apply func (realize-coll tail maps))))
      ;; any collection
      (coll? form)      (realize-coll form maps)
      ;; constant
      :otherwise        form))
  ([form maps error-handler]
    {:pre [(fn? error-handler)]}
    (binding [*error-handler* error-handler]
      (evaluate form maps))))

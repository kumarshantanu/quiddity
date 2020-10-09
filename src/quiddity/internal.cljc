;   Copyright (c) Shantanu Kumar. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file LICENSE at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.


(ns quiddity.internal
  #?(:cljs (:require
             [goog.string :as gstring]
             [goog.string.format]))
  #?(:clj (:import
            [clojure.lang IDeref])))


(def sformat #?(:cljs gstring/format :clj format))


(def ^{:dynamic true
       :doc "Volatile (containing boolean flag) to indicate whether to stop evaluation"}
     *stop-evaluation?* (reify
                          IDeref
                          (#?(:clj deref :cljs -deref) [_] false)))


(def ^{:dynamic true
       :doc "Error handler that accepts error message and deals with it"}
      *error-handler* (fn [message]
                        (throw (ex-info (str "Evaluation error: " message) {}))))


(defn stop-if-requested
  "Signal error if stop requested, return `false` otherwise. Suggested usage:

  ```clojure
  (or
    (stop-if-requested?)
    (other-stuff))
  ```"
  []
  (if @*stop-evaluation?*
    (*error-handler* "Stop-evaluation requested")
    false))


(defn realize-coll
  "Evaluate elements in given collection using the supplied environment maps."
  [form maps evaluate]
  (or
    (stop-if-requested)
    (if (map? form)
      (let [ks (realize-coll (keys form) maps evaluate)
            vs (realize-coll (vals form) maps evaluate)]
        (zipmap ks vs))
      (let [coll-fn (cond
                      (vector? form) vec
                      (set? form)    set
                      :otherwise     (partial apply list))]
        (coll-fn (map #(evaluate % maps) form))))))

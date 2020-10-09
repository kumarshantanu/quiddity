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
             [goog.string.format])))


(def sformat #?(:cljs gstring/format :clj format))


(defn realize-coll
  "Evaluate elements in given collection using the supplied environment maps."
  [form maps evaluate]
  (if (map? form)
    (let [ks (realize-coll (keys form) maps evaluate)
          vs (realize-coll (vals form) maps evaluate)]
      (zipmap ks vs))
    (let [coll-fn (cond
                    (vector? form) vec
                    (set? form)    set
                    :otherwise     (partial apply list))]
      (coll-fn (map #(evaluate % maps) form)))))

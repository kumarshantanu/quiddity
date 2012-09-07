(ns quiddity.lib-cljs-test
  (:require [quiddity.core :as core]
            [quiddity.lib  :as lib])
  (:use [clip-test.testutil-cljs
         :only [RuntimeException
                read-str re-quote throw-msg try-catch error-msg]])
  (:use-macros
    [clip-test.core-cljs
     :only [deftest testing is
            thrown? thrown-with-msg?
            ]]))


(defn es
  [form-str & maps] {:pre [(string? form-str)]}
  (core/evaluate (read-str form-str) maps #(throw-msg %)))


(deftest test-unsupported
  (testing "creating functions"
    (is (thrown-with-msg? RuntimeException
                          #"Could not find tag parser for.*"
                          (es "#(do true)" lib/unsupported)) "implicit fn*")))


(defn test-ns-hook
  []
  (println "\n** Running tests for quiddity.lib-cljs-test **")
  (test-unsupported))
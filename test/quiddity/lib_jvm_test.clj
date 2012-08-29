(ns quiddity.lib-jvm-test
  (:require [quiddity.core :as core]
            [quiddity.lib  :as lib])
  (:use [clip-test.testutil
         :only [read-str re-quote throw-msg try-catch error-msg]])
  (:use [clip-test.core
         :only [deftest testing is]]))


(defn es
  [form-str & maps] {:pre [(string? form-str)]}
  (core/evaluate (read-str form-str) maps #(throw-msg %)))


(deftest test-unsupported
  (testing "def, var, binding"
    (is (thrown-with-msg? RuntimeException
                          (re-quote "No support for `def`, `var`, `binding`")
                          (es "#'a" lib/unsupported)) "#' shortcut for var"))
  (testing "creating functions"
    (is (thrown-with-msg? RuntimeException
                          (re-quote "No support for creating functions")
                          (es "#(do true)" lib/unsupported)) "implicit fn*")))


(deftest test-arity-mismatch
  (testing "evaluator"
    (is (thrown-with-msg? RuntimeException
                          (re-quote "Wrong number of args (2) passed to: lib$e-if")
                          (es "(if true)" lib/special-forms)))))


(defn test-ns-hook
  []
  (test-unsupported)
  (test-arity-mismatch))
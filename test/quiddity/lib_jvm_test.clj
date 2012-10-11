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
                          (es "#'a" lib/unsupported)) "#' shortcut for var")))


(deftest test-arity-mismatch
  (testing "evaluator"
    (is (thrown-with-msg? RuntimeException
                          (re-quote "Wrong number of args (2) passed to: lib$e-if")
                          (es "(if true)" lib/special-forms)))))


(deftest test-macro-equiv-anonymous-fn
  (testing "anonymous fn - #()"
    (let [f (es "#(+ 10 20)" lib/macros {:+ +})]
      (is (fn? f))
      (is (= 30 (f))       "anonymous fn with no argument"))
    (let [f (es "#(+ 10 %)" lib/macros {:+ +})]
      (is (fn? f))
      (is (= 30 (f 20))    "anonymous fn with 1 argument"))
    (let [f (es "#(+ %1 %2)" lib/macros {:+ +})]
      (is (fn? f))
      (is (= 30 (f 10 20)) "anonymous fn with 2 argument"))
    (is (= 30 (es "(let [a #(+ %1 %2)] (a 10 20))" lib/macros {:+ +}))
        "anonymous fn in an expression")))


(defn test-ns-hook
  []
  (test-unsupported)
  (test-arity-mismatch)
  (test-macro-equiv-anonymous-fn))
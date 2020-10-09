;   Copyright (c) Shantanu Kumar. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file LICENSE at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.


(ns quiddity.lib-jvm-test
  (:require
    [clojure.test :refer [deftest is testing]]
    [quiddity.core :as quid]
    [quiddity.lib  :as lib])
  (:import
    [java.util.concurrent ExecutionException]
    [clojure.lang ArityException ExceptionInfo]))


(def read-str read-string)


(defn es
  [form-str & maps] {:pre [(string? form-str)]}
  (quid/evaluate (read-str form-str) maps #(throw (ex-info % {}))))


(deftest test-unsupported
  (testing "def, var, binding"
    (is (thrown-with-msg? ExceptionInfo
                          #"No support for `def`, `var`, `binding`"
                          (es "#'a" lib/unsupported)) "#' shortcut for var")))


(deftest test-arity-mismatch
  (testing "evaluator"
    (is (thrown-with-msg? ArityException
                          #"Wrong number of args \(2\) passed to: (quiddity.)?lib/e-if"
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


(deftest test-macroexpand
  (let [sform (read-str "(or 5 (* 3 4))")
        eform (macroexpand sform)]
    (is (= 5
          (quid/evaluate sform [lib/all])
          (quid/evaluate eform [lib/all])))))


(deftest test-stop-evaluation
  (let [sleep #(Thread/sleep (long %))
        form (read-string "(do (sleep 100) 50 (sleep 1000) 5)")
        flag (volatile! false)
        exec (future (quid/evaluate form [lib/all {:sleep sleep}] {:stop-evaluation? flag}))]
    (sleep 50)
    (vreset! flag true)
    (is (thrown? ExecutionException
          @exec))))


(defn test-ns-hook
  []
  (test-unsupported)
  (test-arity-mismatch)
  (test-macro-equiv-anonymous-fn)
  (test-macroexpand)
  (test-stop-evaluation))

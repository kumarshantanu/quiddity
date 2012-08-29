(ns quiddity.lib-test
  (:require [quiddity.core :as core]
            [quiddity.lib  :as lib])
  (:use [clip-test.testutil;*CLJSBUILD-REMOVE*;-cljs
         :only [;*CLJSBUILD-REMOVE*;RuntimeException
                read-str re-quote throw-msg try-catch error-msg]])
  (:use;*CLJSBUILD-REMOVE*;-macros
    [clip-test.core;*CLJSBUILD-REMOVE*;-cljs
     :only [deftest testing is
            ;*CLJSBUILD-REMOVE*;thrown? thrown-with-msg?
            ]]))


(defn es
  [form-str & maps] {:pre [(string? form-str)]}
  (core/evaluate (read-str form-str) maps #(throw-msg %)))


(deftest test-unsupported
  (testing "assertion"
    (is (thrown-with-msg? RuntimeException
                          (re-quote "No support for `assert`")
                          (es "(assert true)" lib/unsupported)) "assert"))
  (testing "def, var, binding"
    (is (thrown-with-msg? RuntimeException
                          (re-quote "No support for `def`, `var`, `binding`")
                          (es "(def a 10)" lib/unsupported)) "def")
    (is (thrown-with-msg? RuntimeException
                          (re-quote "No support for `def`, `var`, `binding`")
                          (es "(var a)" lib/unsupported)) "var")
    (is (thrown-with-msg? RuntimeException
                          (re-quote "No support for `def`, `var`, `binding`")
                          (es "(binding [*a* 2] 1)" lib/unsupported)) "binding"))
  (testing "creating functions"
    (is (thrown-with-msg? RuntimeException
                          (re-quote "No support for creating functions")
                          (es "(fn [] true)" lib/unsupported)) "fn"))
  (testing "loop-recur"
    (is (thrown-with-msg? RuntimeException
                          (re-quote "No support for `loop` and `recur`")
                          (es "(loop [i 0] recur i)" lib/unsupported)) "loop")
    (is (thrown-with-msg? RuntimeException
                          (re-quote "No support for `loop` and `recur`")
                          (es "(recur 0)" lib/unsupported)) "recur"))
  (testing "try-catch-finally"
    (is (thrown-with-msg? RuntimeException
                          (re-quote "No support for `try`, `catch`, `finally`")
                          (es "(try)" lib/unsupported)) "try")
    (is (thrown-with-msg? RuntimeException
                          (re-quote "No support for `try`, `catch`, `finally`")
                          (es "(catch)" lib/unsupported)) "catch")
    (is (thrown-with-msg? RuntimeException
                          (re-quote "No support for `try`, `catch`, `finally`")
                          (es "(finally)" lib/unsupported)) "finally")))


(deftest test-special-form-equivs
  (testing "if"
    (let [a1 (atom 0)
          a2 (atom 0)
          f1 #(swap! a1 inc)
          f2 #(swap! a2 inc)
          ra #(do (reset! a1 0) (reset! a2 0))
          fs {:f1 f1 :f2 f2}]
      (testing "if (truthy-constant)"
        (is (= 1 (es "(if true (f1) (f2))" fs lib/special-forms)))
        (is (= 1 @a1))
        (is (= 0 @a2)))
      (testing "if (falsy-constant)"
        (ra)
        (is (= 1 (es "(if false (f1) (f2))" fs lib/special-forms)))
        (is (= 0 @a1))
        (is (= 1 @a2)))
      (testing "if (truthy symbol-value)"
        (ra)
        (is (= 1 (es "(if a (f1) (f2))" {:a true} fs lib/special-forms)))
        (is (= 1 @a1))
        (is (= 0 @a2)))
      (testing "if (falsy symbol-value)"
        (ra)
        (is (= 1 (es "(if a (f1) (f2))" {:a false} fs lib/special-forms)))
        (is (= 0 @a1))
        (is (= 1 @a2)))
      (testing "if (truthy test-expr)"
        (ra)
        (is (= 1 (es "(if (a b) (f1) (f2))" {:a identity :b true} fs lib/special-forms)))
        (is (= 1 @a1))
        (is (= 0 @a2)))
      (testing "if (falsy test-expr)"
        (ra)
        (is (= 1 (es "(if (a b) (f1) (f2))" {:a identity :b false} fs lib/special-forms)))
        (is (= 0 @a1))
        (is (= 1 @a2)))))
  (testing "do"
    (is (nil? (es "(do)" lib/special-forms)))
    (is (= 11 (es "(do 10 11)" lib/special-forms)))
    (is (= 11 (es "(do a b)" {:a 10 :b 11} lib/special-forms)))
    (is (= 11 (es "(do (+ 1 1) (+ 5 6))" {:+ + } lib/special-forms))))
  (testing "quote"
    (is (= 11 (es "(quote 11)" lib/special-forms)))
    (is (= 'a (es "(quote a)" lib/special-forms)))
    (is (= '(1 a) (es "(quote (1 a))" lib/special-forms)))
    (is (= '(1 (2 a)) (es "(quote (1 (2 a)))" lib/special-forms)))))


;; ---------- Equivalent of Macros ----------


(deftest test-macro-equiv-if-not
  (let [a1 (atom 0)
        a2 (atom 0)
        f1 #(swap! a1 inc)
        f2 #(swap! a2 inc)
        ra #(do (reset! a1 0) (reset! a2 0))
        fs {:f1 f1 :f2 f2}]
    (testing "if-not (truthy-constant)"
      (is (= 1 (es "(if-not true (f1) (f2))" fs lib/macros)))
      (is (= 0 @a1))
      (is (= 1 @a2)))
    (testing "if-not (falsy-constant)"
      (ra)
      (is (= 1 (es "(if-not false (f1) (f2))" fs lib/macros)))
      (is (= 1 @a1))
      (is (= 0 @a2)))
    (testing "if-not (truthy symbol-value)"
      (ra)
      (is (= 1 (es "(if-not a (f1) (f2))" {:a true} fs lib/macros)))
      (is (= 0 @a1))
      (is (= 1 @a2)))
    (testing "if-not (falsy symbol-value)"
      (ra)
      (is (= 1 (es "(if-not a (f1) (f2))" {:a false} fs lib/macros)))
      (is (= 1 @a1))
      (is (= 0 @a2)))
    (testing "if-not (truthy test-expr)"
      (ra)
      (is (= 1 (es "(if-not (a b) (f1) (f2))" {:a identity :b true} fs lib/macros)))
      (is (= 0 @a1))
      (is (= 1 @a2)))
    (testing "if-not (falsy test-expr)"
      (ra)
      (is (= 1 (es "(if-not (a b) (f1) (f2))" {:a identity :b false} fs lib/macros)))
      (is (= 1 @a1))
      (is (= 0 @a2)))))


(deftest test-macro-equiv-when
  (testing "when (without body)"
    (is (nil? (es "(when true)" lib/macros)) "truthy constant")
    (is (nil? (es "(when false)" lib/macros)) "falsy constant")
    (is (nil? (es "(when a)" {:a true} lib/macros)) "truthy symbol value")
    (is (nil? (es "(when a)" {:a false} lib/macros)) "falsy symbol value")
    (is (nil? (es "(when (a b))" {:a identity :b true} lib/macros)) "truthy test-expr")
    (is (nil? (es "(when (a b))" {:a identity :b false} lib/macros)) "falsy test-expr)"))
  (testing "when (single constant as body)"
    (is (= 10 (es "(when true 10)" lib/macros)) "truthy constant")
    (is (nil? (es "(when false 10)" lib/macros)) "falsy constant")
    (is (= 10 (es "(when a 10)" {:a true} lib/macros)) "truthy symbol value")
    (is (nil? (es "(when a 10)" {:a false} lib/macros)) "falsy symbol value")
    (is (= 10 (es "(when (a b) 10)" {:a identity :b true} lib/macros)) "truthy test-expr")
    (is (nil? (es "(when (a b) 10)" {:a identity :b false} lib/macros)) "falsy test-expr)"))
  (testing "when (single symbol as body)"
    (is (= 10 (es "(when true x)" {:x 10} lib/macros)) "truthy constant")
    (is (nil? (es "(when false x)" {:x 10} lib/macros)) "falsy constant")
    (is (= 10 (es "(when a x)" {:a true :x 10} lib/macros)) "truthy symbol value")
    (is (nil? (es "(when a x)" {:a false :x 10} lib/macros)) "falsy symbol value")
    (is (= 10 (es "(when (a b) x)" {:a identity :b true :x 10} lib/macros)) "truthy test-expr")
    (is (nil? (es "(when (a b) x)" {:a identity :b false :x 10} lib/macros)) "falsy test-expr)"))
  (testing "when (single expr as body)"
    (is (= 11 (es "(when true (+ 1 x))" {:+ + :x 10} lib/macros)) "truthy constant")
    (is (nil? (es "(when false (+ 1 x))" {:+ + :x 10} lib/macros)) "falsy constant")
    (is (= 11 (es "(when a (+ 1 x))" {:a true :+ + :x 10} lib/macros)) "truthy symbol value")
    (is (nil? (es "(when a (+ 1 x))" {:a false :+ + :x 10} lib/macros)) "falsy symbol value")
    (is (= 11 (es "(when (a b) (+ 1 x))" {:a identity :b true :+ + :x 10} lib/macros)) "truthy test-expr")
    (is (nil? (es "(when (a b) (+ 1 x))" {:a identity :b false :+ + :x 10} lib/macros)) "falsy test-expr)"))
  (testing "when (multiple exprs as body)"
    (is (= 11 (es "(when true (+ 2 x) (+ 1 x))" {:+ + :x 10} lib/macros)) "truthy constant")
    (is (nil? (es "(when false (+ 2 x) (+ 1 x))" {:+ + :x 10} lib/macros)) "falsy constant")
    (is (= 11 (es "(when a (+ 2 x) (+ 1 x))" {:a true :+ + :x 10} lib/macros)) "truthy symbol value")
    (is (nil? (es "(when a (+ 2 x) (+ 1 x))" {:a false :+ + :x 10} lib/macros)) "falsy symbol value")
    (is (= 11 (es "(when (a b) (+ 2 x) (+ 1 x))" {:a identity :b true :+ + :x 10} lib/macros)) "truthy test-expr")
    (is (nil? (es "(when (a b) (+ 2 x) (+ 1 x))" {:a identity :b false :+ + :x 10} lib/macros)) "falsy test-expr)")))


(deftest test-macro-equiv-when-not
  (testing "when-not (without body)"
    (is (nil? (es "(when-not true)" lib/macros)) "truthy constant")
    (is (nil? (es "(when-not false)" lib/macros)) "falsy constant")
    (is (nil? (es "(when-not a)" {:a true} lib/macros)) "truthy symbol value")
    (is (nil? (es "(when-not a)" {:a false} lib/macros)) "falsy symbol value")
    (is (nil? (es "(when-not (a b))" {:a identity :b true} lib/macros)) "truthy test-expr")
    (is (nil? (es "(when-not (a b))" {:a identity :b false} lib/macros)) "falsy test-expr)"))
  (testing "when-not (single constant as body)"
    (is (nil? (es "(when-not true 10)" lib/macros)) "truthy constant")
    (is (= 10 (es "(when-not false 10)" lib/macros)) "falsy constant")
    (is (nil? (es "(when-not a 10)" {:a true} lib/macros)) "truthy symbol value")
    (is (= 10 (es "(when-not a 10)" {:a false} lib/macros)) "falsy symbol value")
    (is (nil? (es "(when-not (a b) 10)" {:a identity :b true} lib/macros)) "truthy test-expr")
    (is (= 10 (es "(when-not (a b) 10)" {:a identity :b false} lib/macros)) "falsy test-expr)"))
  (testing "when-not (single symbol as body)"
    (is (nil? (es "(when-not true x)" {:x 10} lib/macros)) "truthy constant")
    (is (= 10 (es "(when-not false x)" {:x 10} lib/macros)) "falsy constant")
    (is (nil? (es "(when-not a x)" {:a true :x 10} lib/macros)) "truthy symbol value")
    (is (= 10 (es "(when-not a x)" {:a false :x 10} lib/macros)) "falsy symbol value")
    (is (nil? (es "(when-not (a b) x)" {:a identity :b true :x 10} lib/macros)) "truthy test-expr")
    (is (= 10 (es "(when-not (a b) x)" {:a identity :b false :x 10} lib/macros)) "falsy test-expr)"))
  (testing "when-not (single expr as body)"
    (is (nil? (es "(when-not true (+ 1 x))" {:+ + :x 10} lib/macros)) "truthy constant")
    (is (= 11 (es "(when-not false (+ 1 x))" {:+ + :x 10} lib/macros)) "falsy constant")
    (is (nil? (es "(when-not a (+ 1 x))" {:a true :+ + :x 10} lib/macros)) "truthy symbol value")
    (is (= 11 (es "(when-not a (+ 1 x))" {:a false :+ + :x 10} lib/macros)) "falsy symbol value")
    (is (nil? (es "(when-not (a b) (+ 1 x))" {:a identity :b true :+ + :x 10} lib/macros)) "truthy test-expr")
    (is (= 11 (es "(when-not (a b) (+ 1 x))" {:a identity :b false :+ + :x 10} lib/macros)) "falsy test-expr)"))
  (testing "when-not (multiple exprs as body)"
    (is (nil? (es "(when-not true (+ 2 x) (+ 1 x))" {:+ + :x 10} lib/macros)) "truthy constant")
    (is (= 11 (es "(when-not false (+ 2 x) (+ 1 x))" {:+ + :x 10} lib/macros)) "falsy constant")
    (is (nil? (es "(when-not a (+ 2 x) (+ 1 x))" {:a true :+ + :x 10} lib/macros)) "truthy symbol value")
    (is (= 11 (es "(when-not a (+ 2 x) (+ 1 x))" {:a false :+ + :x 10} lib/macros)) "falsy symbol value")
    (is (nil? (es "(when-not (a b) (+ 2 x) (+ 1 x))" {:a identity :b true :+ + :x 10} lib/macros)) "truthy test-expr")
    (is (= 11 (es "(when-not (a b) (+ 2 x) (+ 1 x))" {:a identity :b false :+ + :x 10} lib/macros)) "falsy test-expr)")))


(deftest test-macro-equiv-let
  (testing "let (without body)"
    (is (nil? (es "(let [a 1])" lib/macros))     "1 var")
    (is (nil? (es "(let [a 1 b 2])" lib/macros)) "2 vars, no intra-reference")
    (is (nil? (es "(let [a 1 b a])" lib/macros)) "2 vars with direct intra-reference")
    (is (nil? (es "(let [a 1 b (inc a)])" {:inc inc} lib/macros)) "2 vars with intra-reference expr"))
  (testing "let (single constant as body)"
    (is (= 0 (es "(let [a 1] 0)" lib/macros))     "1 var")
    (is (= 0 (es "(let [a 1 b 2] 0)" lib/macros)) "2 vars, no intra-reference")
    (is (= 0 (es "(let [a 1 b a] 0)" lib/macros)) "2 vars with direct intra-reference")
    (is (= 0 (es "(let [a 1 b (inc a)] 0)" {:inc inc} lib/macros)) "2 vars with intra-reference expr"))
  (testing "let (single external symbol as body)"
    (is (= 1 (es "(let [a 1] x)" {:x 1} lib/macros))     "1 var")
    (is (= 1 (es "(let [a 1 b 2] x)" {:x 1} lib/macros)) "2 vars, no intra-reference")
    (is (= 1 (es "(let [a 1 b a] x)" {:x 1} lib/macros)) "2 vars with direct intra-reference")
    (is (= 1 (es "(let [a 1 b (inc a)] x)" {:inc inc :x 1} lib/macros)) "2 vars with intra-reference expr"))
  (testing "let (single let-bound symbol as body)"
    (is (= 1 (es "(let [a 1] a)" lib/macros))     "1 var")
    (is (= 2 (es "(let [a 1 b 2] b)" lib/macros)) "2 vars, no intra-reference")
    (is (= 1 (es "(let [a 1 b a] b)" lib/macros)) "2 vars with direct intra-reference")
    (is (= 2 (es "(let [a 1 b (inc a)] b)" {:inc inc} lib/macros)) "2 vars with intra-reference expr"))
  (testing "let (single expr as body)"
    (is (= 2 (es "(let [a 1] (+ x a))" {:+ + :x 1} lib/macros))     "1 var")
    (is (= 3 (es "(let [a 1 b 2] (+ x b))" {:+ + :x 1} lib/macros)) "2 vars, no intra-reference")
    (is (= 2 (es "(let [a 1 b a] (+ x b))" {:+ + :x 1} lib/macros)) "2 vars with direct intra-reference")
    (is (= 3 (es "(let [a 1 b (inc a)] (+ x b))" {:+ + :x 1 :inc inc} lib/macros)) "2 vars with intra-reference expr"))
  (testing "let (multiple exprs as body)"
    (is (= 2 (es "(let [a 1] (+ 1 a) (+ x a))" {:+ + :x 1} lib/macros))     "1 var")
    (is (= 3 (es "(let [a 1 b 2] (+ 1 a) (+ x b))" {:+ + :x 1} lib/macros)) "2 vars, no intra-reference")
    (is (= 2 (es "(let [a 1 b a] (+ 1 a) (+ x b))" {:+ + :x 1} lib/macros)) "2 vars with direct intra-reference")
    (is (= 3 (es "(let [a 1 b (inc a)] (+ 1 a) (+ x b))" {:+ + :x 1 :inc inc} lib/macros)) "2 vars with intra-reference expr")))


(deftest test-macro-equiv-for-each
  (testing "for-each (single constant as body)"
    (is (= (for [a nil] 0) (es "(for-each [a nil] 0)" lib/macros))     "1 var over nil")
    (is (= (for [a [1]] 0) (es "(for-each [a [1]] 0)" lib/macros))     "1 var over [1]")
    (is (= (for [a (range 2)] 0) (es "(for-each [a (range 2)] 0)" {:range range} lib/macros))     "1 var over expr")
    (is (= (for [a [1] b [2]] 0) (es "(for-each [a [1] b [2]] 0)" lib/macros)) "2 vars, no intra-reference")
    (is (= (for [a [[1]] b a] 0)   (es "(for-each [a [[1]] b a] 0)" lib/macros))   "2 vars with direct intra-reference")
    (is (= (for [a [[1]] b (conj a 2)] 0) (es "(for-each [a [[1]] b (conj a 2)] 0)" {:conj conj} lib/macros)) "2 vars with intra-reference expr"))
  (testing "let (single external symbol as body)"
    (is (= (for [a nil] 1) (es "(for-each [a nil] x)" {:x 1} lib/macros))     "1 var over nil")
    (is (= (for [a [1]] 1) (es "(for-each [a [1]] x)" {:x 1} lib/macros))     "1 var over [1]")
    (is (= (for [a (range 2)] 1) (es "(for-each [a (range 2)] x)" {:range range :x 1} lib/macros))     "1 var over expr")
    (is (= (for [a [1] b [2]] 1) (es "(for-each [a [1] b [2]] x)" {:x 1} lib/macros)) "2 vars, no intra-reference")
    (is (= (for [a [[1]] b a] 1)   (es "(for-each [a [[1]] b a] x)" {:x 1} lib/macros))   "2 vars with direct intra-reference")
    (is (= (for [a [[1]] b (conj a 2)] 1) (es "(for-each [a [[1]] b (conj a 2)] x)" {:conj conj :x 1} lib/macros)) "2 vars with intra-reference expr"))
  (testing "let (single let-bound symbol as body)"
    (is (= (for [a nil] a) (es "(for-each [a nil] a)" lib/macros))     "1 var over nil")
    (is (= (for [a [1]] a) (es "(for-each [a [1]] a)" lib/macros))     "1 var over [1]")
    (is (= (for [a (range 2)] a) (es "(for-each [a (range 2)] a)" {:range range} lib/macros))     "1 var over expr")
    (is (= (for [a [1] b [2]] b) (es "(for-each [a [1] b [2]] b)" lib/macros)) "2 vars, no intra-reference")
    (is (= (for [a [[1]] b a] b) (es "(for-each [a [[1]] b a] b)" lib/macros))   "2 vars with direct intra-reference")
    (is (= (for [a [[1]] b (conj a 2)] b) (es "(for-each [a [[1]] b (conj a 2)] b)" {:conj conj} lib/macros)) "2 vars with intra-reference expr"))
  (testing "let (single expr as body)"
    (is (= (for [a nil] (+ 1 a)) (es "(for-each [a nil] (+ x a))" {:+ + :x 1} lib/macros))     "1 var over nil")
    (is (= (for [a [1]] (+ 1 a)) (es "(for-each [a [1]] (+ x a))" {:+ + :x 1} lib/macros))     "1 var over [1]")
    (is (= (for [a (range 2)] (+ 1 a)) (es "(for-each [a (range 2)] (+ x a))" {:range range :+ + :x 1} lib/macros))     "1 var over expr")
    (is (= (for [a [1] b [2]] (+ 1 b)) (es "(for-each [a [1] b [2]] (+ x b))" {:+ + :x 1} lib/macros)) "2 vars, no intra-reference")
    (is (= (for [a [[1]] b a] (+ 1 b))   (es "(for-each [a [[1]] b a] (+ x b))" {:+ + :x 1} lib/macros))   "2 vars with direct intra-reference")
    (is (= (for [a [[1]] b (conj a 2)] (+ 1 b)) (es "(for-each [a [[1]] b (conj a 2)] (+ x b))" {:conj conj :+ + :x 1} lib/macros)) "2 vars with intra-reference expr")))


(deftest test-macro-equiv-and
  (is (= (and)     (es "(and)" lib/macros)) "no arg")
  (is (= (and 100) (es "(and 100)" lib/macros)) "1 truthy constant arg")
  (is (= (and nil) (es "(and nil)" lib/macros)) "1 falsy constant arg")
  (is (= (and 100) (es "(and foo)" {:foo 100} lib/macros)) "1 truthy symbol")
  (is (= (and nil) (es "(and foo)" {:foo nil} lib/macros)) "1 falsy symbol")
  (is (= (and (+ 1 2))         (es "(and (+ 1 a))"                {:+ + :a 2} lib/macros))                    "1 truthy expr")
  (is (= (and nil)             (es "(and (identity nil))"         {:identity identity} lib/macros))           "1 falsy expr")
  (is (= (and 1 (+ 1 2))       (es "(and 1 (+ 1 a))"              {:+ + :a 2} lib/macros))                    "1 truthy constant, 1 truthy expr")
  (is (= (and 1 nil)           (es "(and 1 (identity nil))"       {:identity identity} lib/macros))           "1 truthy constant, 1 falsy expr")
  (is (= (and nil (+ 1 2))     (es "(and nil (+ 1 a))"            {:+ + :a 2} lib/macros))                    "1 falsy constant, 1 truthy expr")
  (is (= (and nil nil)         (es "(and nil (identity nil))"     {:identity identity} lib/macros))           "1 falsy constant, 1 falsy expr")
  (is (= (and (+ 1 2) (+ 1 1)) (es "(and (+ 1 a) (+ 1 1))"        {:+ + :a 2} lib/macros))                    "1 truthy expr, 1 truthy expr")
  (is (= (and (+ 1 2) nil)     (es "(and (+ 1 a) (identity nil))" {:+ + :a 2 :identity identity} lib/macros)) "1 truthy expr, 1 falsy expr"))


(deftest test-macro-equiv-or
  (is (= (or)     (es "(or)" lib/macros)) "no arg")
  (is (= (or 100) (es "(or 100)" lib/macros)) "1 truthy constant arg")
  (is (= (or nil) (es "(or nil)" lib/macros)) "1 falsy constant arg")
  (is (= (or 100) (es "(or foo)" {:foo 100} lib/macros)) "1 truthy symbol")
  (is (= (or nil) (es "(or foo)" {:foo nil} lib/macros)) "1 falsy symbol")
  (is (= (or (+ 1 2))         (es "(or (+ 1 a))"                {:+ + :a 2} lib/macros))                    "1 truthy expr")
  (is (= (or nil)             (es "(or (identity nil))"         {:identity identity} lib/macros))           "1 falsy expr")
  (is (= (or 1 (+ 1 2))       (es "(or 1 (+ 1 a))"              {:+ + :a 2} lib/macros))                    "1 truthy constant, 1 truthy expr")
  (is (= (or 1 nil)           (es "(or 1 (identity nil))"       {:identity identity} lib/macros))           "1 truthy constant, 1 falsy expr")
  (is (= (or nil (+ 1 2))     (es "(or nil (+ 1 a))"            {:+ + :a 2} lib/macros))                    "1 falsy constant, 1 truthy expr")
  (is (= (or nil nil)         (es "(or nil (identity nil))"     {:identity identity} lib/macros))           "1 falsy constant, 1 falsy expr")
  (is (= (or (+ 1 2) (+ 1 1)) (es "(or (+ 1 a) (+ 1 1))"        {:+ + :a 2} lib/macros))                    "1 truthy expr, 1 truthy expr")
  (is (= (or (+ 1 2) nil)     (es "(or (+ 1 a) (identity nil))" {:+ + :a 2 :identity identity} lib/macros)) "1 truthy expr, 1 falsy expr"))


(defn test-ns-hook
  []
  (println "\n** Running tests for quiddity.lib-test **")
  (test-unsupported)
  (test-special-form-equivs)
  (test-macro-equiv-if-not)
  (test-macro-equiv-when)
  (test-macro-equiv-when-not)
  (test-macro-equiv-let)
  (test-macro-equiv-for-each)
  (test-macro-equiv-and)
  (test-macro-equiv-or)
)
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
    ;; See 'creating anon fns' in quiddity.lib-jvm-test and quiddity-cljs-test
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


;; ---------- Destructuring helper ----------


(defn ds
  [local value]
  (binding [core/*error-handler* throw-msg]
    (lib/i-destructure [] local value)))


(defn rds
  [local value]
  (ds (read-str local) value))


(deftest test-destructuring
  (testing "no destructuring"
    (is (= {:a 10} (rds "a" 10)) "simple local var")
    (is (= {}      (rds "_" 10)) "simple placeholder"))
  (testing "seq destructuring (simple)"
    (is (= (let [[a b]           [10 20]]    {:a a :b b})           (rds "[a b]"           [10 20]))    "local vars")
    (is (= (let [[a b]           [10]]       {:a a :b b})           (rds "[a b]"           [10]))       "local vars with missing value")
    (is (= (let [[a _]           [10 20]]    {:a a})                (rds "[a _]"           [10 20]))    "local var and placeholder")
    (is (= (let [[a _ c]         [10 20 30]] {:a a :c c})           (rds "[a _ c]"         [10 20 30])) "local vars and placeholder")
    (is (= (let [[_ _]           [10 20]]    {})                    (rds "[_ _]"           [10 20]))    "only placeholders")
    (is (= (let [[& c]           [10 20]]    {:c c})                (rds "[& c]"           [10 20]))    "opt var")
    (is (= (let [[a b & c]       [10 20 30]] {:a a :b b :c c})      (rds "[a b & c]"       [10 20 30])) "local and opt vars")
    (is (= (let [[:as d]         [10 20]]    {:d d})                (rds "[:as d]"         [10 20]))    "only :as")
    (is (= (let [[a b :as d]     [10 20]]    {:a a :b b :d d})      (rds "[a b :as d]"     [10 20]))    "local vars and :as")
    (is (= (let [[& c :as d]     [10 20]]    {:c c :d d})           (rds "[& c :as d]"     [10 20]))    "opt var and :as")
    (is (= (let [[a b & c :as d] [10 20 30]] {:a a :b b :c c :d d}) (rds "[a b & c :as d]" [10 20 30])) "local vars, opt var and :as"))
  (testing "seq destructuring (nested)"
    (is (= (let [[[a] [b]]           [[10] [20]]] {:a a :b b})        (rds "[[a] [b]]"         [[10] [20]]))  "local vars")
    (is (= (let [[[a] [_]]           [[10] [20]]] {:a a})             (rds "[[a] [_]]"         [[10] [20]]))  "local var and placeholder")
    (is (= (let [[[_] [_]]           [[10] [20]]] {})                 (rds "[[_] [_]]"         [[10] [20]]))  "only placeholders")
    (is (= (let [[a b & [c]]         [10 20 30]]  {:a a :b b :c c})   (rds "[a b & [c]]"       [10 20 30]))   "local and opt vars")
    (is (= (let [[:as [d]]           [10]]        {:d d})             (rds "[:as [d]]"         [10]))         "only :as")
    (is (= (let [[[a] [b] :as [d]]   [[10] [20]]] {:a a :b b :d d})   (rds "[[a] [b] :as [d]]" [[10] [20]]))  "local vars and :as")
    (is (= (let [[& [c] :as [d e]]   [10 20]]     {:c c :d d :e e})   (rds "[& [c] :as [d e]]" [10 20]))      "opt var and :as")
    (is (= (let [[[a] [b]
                  & [c] :as [d e]]   [[10] [20] [30]]] {:a a :b b :c c
                                                        :d d :e e})   (rds "[[a] [b] & [c] :as [d e]]"
                                                                           [[10] [20] [30]]))                    "local vars, opt var and :as")
    (is (= (let [[a {b :b}]          [10 {:b 20}]]  {:a a :b b})      (rds "[a {b :b}]"          [10 {:b 20}]))  "local var and value lookup (map)")
    (is (= (let [[a {:keys [b]}]     [10 {:b 20}]]  {:a a :b b})      (rds "[a {:keys [b]}]"     [10 {:b 20}]))  "local var and :keys (map)")
    (is (= (let [[& {:keys [a b]}]   [:a 10 :b 20]] {:a a :b b})      (rds "[& {:keys [a b]}]"   [:a 10 :b 20])) "opt :keys (map)")
    (is (= (let [[& {a :a [b c] :b}] [:a 10
                                      :b [20 30]]]  {:a a :b b :c c}) (rds "[& {a :a [b c] :b}]" [:a 10 :b [20 30]])) "opt :keys (map)"))
  (testing "map destructuring (simple)"
    (is (= (let [{a 0}        [10 11 12 13]]   {:a a})           (rds "{a 0}"       [10 11 12 13]))    "value lookup on a vector")
    (is (= (let [{a 9}        [10 11 12 13]]   {:a a})           (rds "{a 9}"       [10 11 12 13]))    "missing value lookup on a vector")
    (is (= (let [{a :a}       {:a 10 :b 20}]   {:a a})           (rds "{a :a}"      {:a 10 :b 20}))    "value lookup on a map")
    (is (= (let [{:keys [a]}  {:a 10 :b 20}]   {:a a})           (rds "{:keys [a]}" {:a 10 :b 20}))    ":keys")
    (is (= (let [{:strs [a]}  {"a" 10 "b" 20}] {:a a})           (rds "{:strs [a]}" {"a" 10 "b" 20}))  ":strs")
    (is (= (let [{:syms [a]}  {'a 10 'b 20}]   {:a a})           (rds "{:syms [a]}" {'a 10 'b 20}))    ":syms") ;see workaround below
    (is (= (let [{:syms [a]}  {'a 10 'b 20}]   {:a a})           (rds "{:syms [a]}" {(symbol 'a) 10
                                                                                     (symbol 'b) 20})) ":syms (coerced as symbol)")
    (is (= (let [{a :a
                  :or {a 20}} {:a 10}]         {:a a})           (rds "{a :a :or {a 20}}" {:a 10}))    "value lookup with :or")
    (is (= (let [{a :a
                  :or {a 20}} {}]              {:a a})           (rds "{a :a :or {a 20}}" {}))         "missing value lookup with :or")
    (is (= (let [{a 0 :as b}  [10]]            {:a a :b b})      (rds "{a 0 :as b}"       [10]))       "value lookup on a vector and :as")
    (is (= (let [{a :a :as b} {:a 10}]         {:a a :b b})      (rds "{a :a :as b}"      {:a 10}))    "value lookup on a map and :as")
    (is (= (let [{:keys [a b]
                  :or {b 20} :as c} {:a 10}]   {:a a :b b :c c}) (rds "{:keys [a b] :or {b 20} :as c}"
                                                                      {:a 10}))                        ":keys, :or and :as")
    )
  (testing "map destructuring (nested)"
    (is (= (let [{{p :p} :a}      {:a {:p 10}}] {:p p}) (rds "{{:keys [p]} :a}" {:a {:p 10}})) "value lookup in value lookup")
    (is (= (let [{[p] :a}     {:a [10]}] {:p p})        (rds "{[p] :a}"         {:a [10]}))    "local var (seq) in value lookup")
    (is (= (let [{{:keys [p]} :a} {:a {:p 10}}] {:p p}) (rds "{{:keys [p]} :a}" {:a {:p 10}})) ":keys lookup in value lookup")))


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
    (is (= 3 (es "(let [a 1 b (inc a)] (+ 1 a) (+ x b))" {:+ + :x 1 :inc inc} lib/macros)) "2 vars with intra-reference expr"))
  (testing "let (destructuring)"
    (is (= 2 (es "(let [[a]         [1]] (+ 1 a) (+ x a))"              {:+ + :x 1} lib/macros)) "1 local var")
    (is (= 3 (es "(let [[a b]       [1 2]] (+ 1 a) (+ x b))"            {:+ + :x 1} lib/macros)) "2 local vars")
    (is (= 2 (es "(let [{a :a}      {:a 1} b a] (+ 1 a) (+ x b))"       {:+ + :x 1} lib/macros)) "value lookup")
    (is (= 3 (es "(let [{:keys [a]} {:a 1} b (inc a)] (+ 1 a) (+ x b))" {:+ + :x 1
                                                                         :inc inc}  lib/macros)) ":keys lookup")))


(deftest test-macro-equiv-if-let
  (let [a1 (atom 0)
        a2 (atom 0)
        f1 #(swap! a1 inc)
        f2 #(swap! a2 inc)
        ra #(do (reset! a1 0) (reset! a2 0))
        fs {:f1 f1 :f2 f2}]
    (testing "if-let (truthy-constant)"
      (ra)
      (is (= 1 (es "(if-let [_ true] (f1) (f2))" fs lib/macros)))
      (is (= 1 @a1))
      (is (= 0 @a2)))
    (testing "if-let (falsy-constant)"
      (ra)
      (is (= 1 (es "(if-let [_ false] (f1) (f2))" fs lib/macros)))
      (is (= 0 @a1))
      (is (= 1 @a2)))
    (testing "if-let (truthy symbol-value)"
      (ra)
      (is (= 1 (es "(if-let [_ a] (f1) (f2))" {:a true} fs lib/macros)))
      (is (= 1 @a1))
      (is (= 0 @a2)))
    (testing "if-let (falsy symbol-value)"
      (ra)
      (is (= 1 (es "(if-let [_ a] (f1) (f2))" {:a false} fs lib/macros)))
      (is (= 0 @a1))
      (is (= 1 @a2)))
    (testing "if-let (truthy test-expr)"
      (ra)
      (is (= 1 (es "(if-let [_ (a b)] (f1) (f2))" {:a identity :b true} fs lib/macros)))
      (is (= 1 @a1))
      (is (= 0 @a2)))
    (testing "if-let (falsy test-expr)"
      (ra)
      (is (= 1 (es "(if-let [_ (a b)] (f1) (f2))" {:a identity :b false} fs lib/macros)))
      (is (= 0 @a1))
      (is (= 1 @a2)))
    (testing "if-let (destructuring)"
      (ra)
      (is (= [1 2] (es "(if-let [[p] (a b)] [(f1) p] (f2))" {:a identity :b [2]} fs lib/macros))                "1 local")
      (ra)
      (is (= [1 5] (es "(if-let [[p q] (a b)] [(f1) (+ p q)] (f2))" {:a identity :b [2 3] :+ +} fs lib/macros)) "2 locals")
      (ra)
      (is (= [1 2] (es "(if-let [{p :q} (a b)] [(f1) p] (f2))" {:a identity :b {:q 2}} fs lib/macros))          " value lookup")
      (ra)
      (is (= [1 2] (es "(if-let [{:keys [p]} (a b)] [(f1) p] (f2))" {:a identity :b {:p 2}} fs lib/macros))     ":keys lookup"))))


(deftest test-macro-equiv-when-let
  (let [a1 (atom 0)
        a2 (atom 0)
        f1 #(swap! a1 inc)
        f2 #(swap! a2 inc)
        ra #(do (reset! a1 0) (reset! a2 0))
        fs {:f1 f1 :f2 f2}]
    (testing "when-let (truthy constant)"
      (is (= nil (es "(when-let [_ true])"     lib/macros)) "no body")
      (is (= :aa (es "(when-let [_ true] :aa)" lib/macros)) "constant body")
      (ra)
      (is (= 1 (es "(when-let [_ true] (f1))"      fs lib/macros)) "expression body")
      (is (= 1 @a1))
      (ra)
      (is (= 1 (es "(when-let [_ true] (f1) (f2))" fs lib/macros)) "multiple expression body")
      (is (= 1 @a1))
      (is (= 1 @a2)))
    (testing "when-let (falsy constant)"
      (is (= nil (es "(when-let [_ false])"     lib/macros)) "no body")
      (is (= nil (es "(when-let [_ false] :aa)" lib/macros)) "constant body"))
    (testing "when-let (truthy expression)"
      (is (= nil (es "(when-let [_ (a b)])"     {:a identity :b true} lib/macros)) "no body")
      (is (= :aa (es "(when-let [_ (a b)] :aa)" {:a identity :b true} lib/macros)) "constant body")
      (ra)
      (is (= 1 (es "(when-let [_ (a b)] (f1))"      {:a identity :b true} fs lib/macros)) "expression body")
      (is (= 1 @a1))
      (ra)
      (is (= 1 (es "(when-let [_ (a b)] (f1) (f2))" {:a identity :b true} fs lib/macros)) "multiple expression body")
      (is (= 1 @a1))
      (is (= 1 @a2)))
    (testing "when-let (falsy expression)"
      (is (= nil (es "(when-let [_ (a b)])"     {:a identity :b nil} lib/macros)) "no body")
      (is (= nil (es "(when-let [_ (a b)] :aa)" {:a identity :b nil} lib/macros)) "constant body"))
    (testing "when-let (destructuring)"
      (ra)
      (is (= [1 2] (es "(when-let [[p] (a b)] [(f1) p])" {:a identity :b [2]} fs lib/macros))                "1 local")
      (ra)
      (is (= [1 5] (es "(when-let [[p q] (a b)] [(f1) (+ p q)])" {:a identity :b [2 3] :+ +} fs lib/macros)) "2 locals")
      (ra)
      (is (= [1 2] (es "(when-let [{p :q} (a b)] [(f1) p])" {:a identity :b {:q 2}} fs lib/macros))          " value lookup")
      (ra)
      (is (= [1 2] (es "(when-let [{:keys [p]} (a b)] [(f1) p])" {:a identity :b {:p 2}} fs lib/macros))     ":keys lookup"))))


(deftest test-macro-equiv-for-each
  (testing "for-each (single constant as body)"
    (is (= (for [a nil] 0)       (es "(for-each [a nil]       0)" lib/macros)) "1 var over nil")
    (is (= (for [a [1]] 0)       (es "(for-each [a [1]]       0)" lib/macros)) "1 var over [1]")
    (is (= (for [a (range 2)] 0) (es "(for-each [a (range 2)] 0)" {:range range} lib/macros)) "1 var over expr")
    (is (= (for [a [1] b [2]] 0) (es "(for-each [a [1] b [2]] 0)" lib/macros)) "2 vars, no intra-reference")
    (is (= (for [a [[1]] b a] 0) (es "(for-each [a [[1]] b a] 0)" lib/macros)) "2 vars with direct intra-reference")
    (is (= (for [a [[1]] b (conj a 2)] 0) (es "(for-each [a [[1]] b (conj a 2)] 0)" {:conj conj} lib/macros)) "2 vars with intra-reference expr"))
  (testing "for-each (single external symbol as body)"
    (is (= (for [a nil] 1)       (es "(for-each [a nil]       x)" {:x 1} lib/macros)) "1 var over nil")
    (is (= (for [a [1]] 1)       (es "(for-each [a [1]]       x)" {:x 1} lib/macros)) "1 var over [1]")
    (is (= (for [a (range 2)] 1) (es "(for-each [a (range 2)] x)" {:range range :x 1} lib/macros)) "1 var over expr")
    (is (= (for [a [1] b [2]] 1) (es "(for-each [a [1] b [2]] x)" {:x 1} lib/macros)) "2 vars, no intra-reference")
    (is (= (for [a [[1]] b a] 1) (es "(for-each [a [[1]] b a] x)" {:x 1} lib/macros)) "2 vars with direct intra-reference")
    (is (= (for [a [[1]] b (conj a 2)] 1) (es "(for-each [a [[1]] b (conj a 2)] x)" {:conj conj :x 1} lib/macros)) "2 vars with intra-reference expr"))
  (testing "for-each (single let-bound symbol as body)"
    (is (= (for [a nil] a)       (es "(for-each [a nil]       a)" lib/macros)) "1 var over nil")
    (is (= (for [a [1]] a)       (es "(for-each [a [1]]       a)" lib/macros)) "1 var over [1]")
    (is (= (for [a (range 2)] a) (es "(for-each [a (range 2)] a)" {:range range} lib/macros)) "1 var over expr")
    (is (= (for [a [1] b [2]] b) (es "(for-each [a [1] b [2]] b)" lib/macros)) "2 vars, no intra-reference")
    (is (= (for [a [[1]] b a] b) (es "(for-each [a [[1]] b a] b)" lib/macros)) "2 vars with direct intra-reference")
    (is (= (for [a [[1]] b (conj a 2)] b) (es "(for-each [a [[1]] b (conj a 2)] b)" {:conj conj} lib/macros)) "2 vars with intra-reference expr"))
  (testing "for-each (single expr as body)"
    (is (= (for [a nil] (+ 1 a))       (es "(for-each [a nil]       (+ x a))" {:+ + :x 1} lib/macros)) "1 var over nil")
    (is (= (for [a [1]] (+ 1 a))       (es "(for-each [a [1]]       (+ x a))" {:+ + :x 1} lib/macros)) "1 var over [1]")
    (is (= (for [a (range 2)] (+ 1 a)) (es "(for-each [a (range 2)] (+ x a))" {:range range :+ + :x 1} lib/macros)) "1 var over expr")
    (is (= (for [a [1] b [2]] (+ 1 b)) (es "(for-each [a [1] b [2]] (+ x b))" {:+ + :x 1} lib/macros)) "2 vars, no intra-reference")
    (is (= (for [a [[1]] b a] (+ 1 b)) (es "(for-each [a [[1]] b a] (+ x b))" {:+ + :x 1} lib/macros)) "2 vars with direct intra-reference")
    (is (= (for [a [[1]] b (conj a 2)] (+ 1 b)) (es "(for-each [a [[1]] b (conj a 2)] (+ x b))" {:conj conj :+ + :x 1} lib/macros)) "2 vars with intra-reference expr"))
  (testing "for-each (destructuring)"
    (is (= (for [[a]         [[1]]]    (+ 1 a)) (es "(for-each [[a]         [[1]]]    (+ x a))" {:+ + :x 1} lib/macros)) "1 local var")
    (is (= (for [[a b]       [[1 2]]]  (+ a b)) (es "(for-each [[a b]       [[1 2]]]  (+ a b))" {:+ +}      lib/macros)) "2 local vars")
    (is (= (for [{a :a}      [{:a 1}]] (+ 1 a)) (es "(for-each [{a :a}      [{:a 1}]] (+ x a))" {:+ + :x 1} lib/macros)) "value lookup")
    (is (= (for [{:keys [a]} [{:a 1}]] (+ 1 a)) (es "(for-each [{:keys [a]} [{:a 1}]] (+ x a))" {:+ + :x 1} lib/macros)) ":keys lookup")))


(deftest test-macro-equiv-cond
  (let [a1 (atom 0)
        a2 (atom 0)
        f1 #(swap! a1 inc)
        f2 #(swap! a2 inc)
        ra #(do (reset! a1 0) (reset! a2 0))
        a  identity
        b  true
        c  false
        fs {:f1 f1 :f2 f2 :a identity :b true :c false}]
    (testing "cond (no arg)"
      (is (= (cond)            (es "(cond)" lib/macros))               "no arg"))
    (testing "cond (test & then)"
      (is (= (cond true  1000) (es "(cond true  1000)" fs lib/macros)) "constant truthy test, constant then")
      (is (= (cond true  (f1)) (es "(cond true  (f2))" fs lib/macros)) "constant truthy test, expression then")
      (is (= (cond false 1000) (es "(cond false 1000)" fs lib/macros)) "constant falsy test, constant then")
      (is (= (cond false (f1)) (es "(cond false (f2))" fs lib/macros)) "constant falsy test, expression then")
      (is (= (cond (a b) 1000) (es "(cond (a b) 1000)" fs lib/macros)) "expression truthy test, constant then")
      (is (= (cond (a b) (f1)) (es "(cond (a b) (f2))" fs lib/macros)) "expression truthy test, expression then")
      (is (= (cond (a c) 1000) (es "(cond (a c) 1000)" fs lib/macros)) "expression falsy test, constant then")
      (is (= (cond (a c) (f1)) (es "(cond (a c) (f2))" fs lib/macros)) "expression falsy test, expression then"))
    (testing "cond (test1 & then1, test2 & then2)"
      (is (= (cond false 10 true  20) (es "(cond false 10 true  20)" fs lib/macros)) "2nd truthy test, constant then")
      (is (= (cond false 10 false 20) (es "(cond false 10 false 20)" fs lib/macros)) "2nd falsy test, constant then"))))


(deftest test-macro-equiv-condp
  (testing "condp (no clauses)"
    (is (thrown-with-msg? RuntimeException (re-quote "No matching clause: 1")
                          (es "(condp = 1)" {:= =} lib/macros)) "no clauses")
    )
  (testing "condp (constant test and value)"
    (is (= (condp = 1 1 1)         (es "(condp = 1 1 1)"         {:= =} lib/macros)) "matching")
    (is (= (condp = 1 2 2 1 1)     (es "(condp = 1 2 2 1 1)"     {:= =} lib/macros)) "non-matching, matching")
    (is (= (condp = 1 2 2 3)       (es "(condp = 1 2 2 3)"       {:= =} lib/macros)) "non-matching, default")
    (is (= (condp = 1 1 :>> not)   (es "(condp = 1 1 :>> not)"   {:= = :not not} lib/macros)) "matching unary")
    (is (= (condp = 1 2 :>> not
             1 :>> not)    (es "(condp = 1 2 :>> not 1 :>> not)" {:= = :not not} lib/macros)) "non-matching unary, matching unary")
    (is (= (condp = 1 2 :>> not 3) (es "(condp = 1 2 :>> not 3)" {:= = :not not} lib/macros)) "non-matching unary, default"))
  (testing "condp (expression test and value)"
    (is (= (condp = 1 (+ 0 1) 1)           (es "(condp = 1 (+ 0 1) 1)"             {:= = :+ +} lib/macros)) "matching")
    (is (= (condp = 1 (+ 0 2) 2 (+ 0 1) 1) (es "(condp = 1 (+ 0 2) 2 (+ 0 1) 1)"   {:= = :+ +} lib/macros)) "non-matching, matching")
    (is (= (condp = 1 (+ 0 2) 2 3)         (es "(condp = 1 (+ 0 2) 2 3)"           {:= = :+ +} lib/macros)) "non-matching, default")
    (is (= (condp = 1 (+ 0 1) :>> not)     (es "(condp = 1 (+ 0 1) :>> not)"       {:= = :+ + :not not} lib/macros)) "matching unary")
    (is (= (condp = 1 (+ 0 2) :>> not
             (+ 0 1) :>> not)    (es "(condp = 1 (+ 0 2) :>> not (+ 0 1) :>> not)" {:= = :+ + :not not} lib/macros)) "non-matching unary, matching unary")
    (is (= (condp = 1 (+ 0 2) :>> not 3)   (es "(condp = 1 (+ 0 2) :>> not 3)"     {:= = :+ + :not not} lib/macros)) "non-matching unary, default"))
  (testing "condp (side-effect)"
    (let [a1 (atom 0)
          a2 (atom 0)
          f1 #(swap! a1 inc)
          f2 #(swap! a2 inc)
          ra #(do (reset! a1 0) (reset! a2 0))
          fs {:f1 f1 :f2 f2 := =}]
      (is (= 2 (es "(condp = 1 (f1) (f1))"           fs lib/macros)) "matching")
      (ra)
      (f2) (f2)
      (is (= 3 (es "(condp = 2 (f1) (f1) (f2))"      fs lib/macros)) "non-matching, default")
      (is (= 1 @a1))
      (ra)
      (f2)
      (is (= 3 (es "(condp = 2 (f1) (f1) (f2) (f2))" fs lib/macros)) "non-matching, matching")
      (is (= 1 @a1))
      (is (= 3 @a2)))))


(deftest test-macro-equiv-case
  (testing "case (no match)"
    (is (thrown-with-msg? RuntimeException (re-quote "No matching clause: 1")
                          (es "(case 1)"     lib/macros))   "no clauses")
    (is (thrown-with-msg? RuntimeException (re-quote "No matching clause: 1")
                          (es "(case 1 2 2)" lib/macros))   "no match, no default"))
  (testing "case (match)"
    (is (= (case 1       1 1)       (es "(case 1 1 1)"             lib/macros))        "const matching value-constant")
    (is (= (case (+ 0 1) 1 1)       (es "(case (+ 0 1) 1 1)"       {:+ +} lib/macros)) "expr  matching value-constant")
    (is (= (case (+ 0 1) 1 (+ 1 2)) (es "(case (+ 0 1) 1 (+ 1 2))" {:+ +} lib/macros)) "expr  matching value-expr")
    (is (= (case (+ 1 1)
             1 (+ 1 2)
             2 (+ 2 3))   (es "(case (+ 1 1) 1 (+ 1 2) 2 (+ 2 3))" {:+ +} lib/macros)) "expr  non-matching, matching expr")
    (is (= (case 1 (1 2) 2)         (es "(case 1 (1 2) 2)"         lib/macros))        "const matching list")
    (is (= (case 1
             (2 3) 2
             (1 4) 4)               (es "(case 1 (2 3) 2 (1 4) 4)" lib/macros))        "const non-matching list, matching list")
    (is (= (case ()
             ()   1
             (()) 2)                (es "(case () () 1 (()) 2)"    lib/macros))        "non-matching const, matching list"))
  (testing "case (side effects)"
    (let [a1 (atom 0)
          a2 (atom 0)
          f1 #(swap! a1 inc)
          f2 #(swap! a2 inc)
          ra #(do (reset! a1 0) (reset! a2 0))
          fs {:f1 f1 :f2 f2 := =}]
      (is (thrown-with-msg? RuntimeException (re-quote "No matching clause: 1")
                            (es "(case 1 2 (f1))" fs lib/macros)) "non-matching")
      (is (= 0 @a1))
      (ra)
      (is (= 1 (es "(case 1 1 (f1))" fs lib/macros)) "matching")
      (ra)
      (is (= 1 (es "(case 1 2 (f1) (f2))" fs lib/macros)) "non-matching, default")
      (is (= 0 @a1) "non-matching value expression should not be evaluated")
      (is (= 1 @a2) "default value expression should be evaluated")
      (ra)
      (is (= 1 (es "(case 1 2 (f1) 1 (f2))" fs lib/macros)) "non-matching, default")
      (is (= 0 @a1) "non-matching value expression should not be evaluated")
      (is (= 1 @a2) "matching value expression should be evaluated"))))


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
  (test-destructuring)
  (test-macro-equiv-if-not)
  (test-macro-equiv-when)
  (test-macro-equiv-when-not)
  (test-macro-equiv-let)
  (test-macro-equiv-if-let)
  (test-macro-equiv-when-let)
  (test-macro-equiv-for-each)
  (test-macro-equiv-cond)
  (test-macro-equiv-condp)
  (test-macro-equiv-case)
  (test-macro-equiv-and)
  (test-macro-equiv-or))
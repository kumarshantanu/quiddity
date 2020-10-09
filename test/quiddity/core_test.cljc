(ns quiddity.core-test
  (:require
    #?(:cljs [cljs.test    :refer-macros [deftest is testing]]
        :clj [clojure.test :refer        [deftest is testing]])
    #?(:cljs [cljs.reader :refer [read-string]])
    #?(:cljs [quiddity.core :as quid :include-macros true]
        :clj [quiddity.core :as quid]))
  #?(:clj (:import
            [clojure.lang ExceptionInfo])))


(defn rs [s] (read-string s))


(defn throw-msg
  [msg]
  (throw (ex-info msg {})))


(defn ev
  [form & maps]
  (quid/evaluate form maps #(throw-msg %)))


(deftest test-primitives
  (testing "Numbers"
    (is (= 0 (ev `0)) "zero")
    (is (= -99 (ev `-99)) "-ve integer")
    (is (= 100 (ev `100)) "+ve integer"))
  (testing "String, Keyword"
    (is (= "foo" (ev `"foo")) "string")
    (is (= :foo  (ev `:foo)) "keyword")))


(deftest test-collection
  (testing "Sequence"
    (is (= [1 2]    (ev `[1 2]))    "vector")
    (is (= #{:a :b} (ev `#{:a :b})) "set"))
  (testing "Map"
    (is (= {:a 10 :b 20} (ev `{:a 10 :b 20})) "map")))


(deftest test-quoted
  (let [q {:quote (quid/make-evaluator (fn [maps x] x))}]
    (testing "Atom (not Clojure atom, but rather Lisp atom in lexical sense)"
      (is (= -999 (ev `'-999 q)) "-ve integer")
      (is (= 1000 (ev `'1000 q)) "+ve integer")
      (is (= :foo (ev `':foo q)) "keyword")
      (is (= "pi" (ev `'"pi" q)) "string"))
    (testing "Collection"
      (is (= () (ev `()))           "empty list")
      (is (= '(1 2) (ev `'(1 2) q)) "list")
      (is (= '[1 2] (ev `'[1 2] q)) "vector")
      (is (= '#{1 2} (ev `'#{1 2} q)) "set")
      (is (= '{:a 1} (ev `'{:a 1} q)) "map"))
    (testing "Form"
      (is (= '(:b {:b 20}) (ev `'(:b {:b 20}) q)) "S-expression"))))


(deftest test-substitution
  (is (not= `a (rs "a")) "symbols in back-quoted forms are auto-qualified")
  (testing "Atom"
    (is (= 10     (ev (rs "a") {:a 10}))     "atom pointing to integer")
    (is (= :foo   (ev (rs "a") {:a :foo}))   "atom pointing to keyword")
    (is (= '(1 2) (ev (rs "a") {:a '(1 2)})) "atom pointing to a list")
    (is (= [1 2]  (ev (rs "a") {:a [1 2]}))  "atom pointing to vector")
    (is (= #{1 2} (ev (rs "a") {:a #{1 2}})) "atom pointing to set")
    (is (= {:a 1} (ev (rs "a") {:a {:a 1}})) "atom pointing to map"))
  (testing "In collections (seq & map)"
    ;; list literal is quoted and is returned as is (see 'test-quoted' testcase)
    (is (= [1 2]       (ev (rs "[1 a]")  {:a 2}))      "symbol in a vector")
    (is (= #{1 2}      (ev (rs "#{1 a}") {:a 2}))      "symbol in a set")
    (is (= {:a {:c 1}} (ev (rs "{:a b}") {:b {:c 1}})) "symbol in a map"))
  (testing "Nested collections"
    (is (= [1 [2 3]]   (ev (rs "[1 [2 c]]")   {:c 3}))  "vector in a vector")
    (is (= #{1 #{2 3}} (ev (rs "#{1 #{2 c}}") {:c 3}))  "set in a set")
    (is (= {:a {:b 3}} (ev (rs "{k {:b 3}}")  {:k :a})) "map in a map (key)")
    (is (= {:a {:b 3}} (ev (rs "{:a {:b c}}") {:c 3}))  "map in a map (val)")))


(deftest test-missing
  (let [e (fn [form & maps]
            (quid/evaluate form maps #(throw-msg %)))]
    (is (thrown-with-msg? ExceptionInfo
                          #"No such key 'a' in env keys ()"
                          (e (rs "a")))
        "missing val for atom")
    (is (thrown-with-msg? ExceptionInfo
                          #"No such key 'inc' in env keys ()"
                          (e (rs "(inc a)")))
        "missing val for fn name")
    (is (thrown-with-msg? ExceptionInfo
                          #"No such key 'a' in env keys \(\(\:inc\)\)"
                          (e (rs "(inc a)") {:inc inc}))
        "missing val for fn arg")))


(deftest test-invocation
  (testing "Function calls"
    (is (= 10 (ev (rs "(foo)")         {:foo (constantly 10)})) "function call")
    (is (= 10 (ev (rs "(inc n)")       {:inc inc :n 9}))        "fn call w/arg")
    (is (= 10 (ev (rs "(inc (dec n))") {:inc inc
                                        :dec dec :n 10}))       "nested call")
    (is (= [10]   (ev (rs "[(inc n)]")  {:inc inc :n 9}))       "in vector")
    (is (= #{9}   (ev (rs "#{(dec n)}") {:dec dec :n 10}))      "in set")
    (is (= {:a 9} (ev (rs "{(k) (v)}")  {:k (constantly :a)
                                         :v (constantly 9)}))   "in map"))
  (testing "Evaluator"
    (is (= 'n (ev (rs "(quote n)")
                  {:quote (quid/make-evaluator
                            (fn [maps x] x))})) "evaluator w/arg")
    (is (= 10 (ev (rs "(do (inc n))")
                  {:do (quid/make-evaluator
                         (fn [maps & forms]
                           (last (map #(quid/evaluate % maps) forms))))
                   :inc inc
                   :n 9}))                      "evaluator w/sexp")))


(defn test-ns-hook
  []
  (println "\n** Running tests for quiddity.core-test **")
  (test-primitives)
  (test-collection)
  (test-quoted)
  (test-substitution)
  (test-missing)
  (test-invocation))
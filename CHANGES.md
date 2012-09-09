# Changes and TODO

## TODO

* Destructuring primitives
* Retrofit destructuring into `let` `for-each`
* [TODO] Implement `for` macro; deprecate/remove `for-each`
* [TODO] Function declaration (both `fn` and `fn*`) with destructuring
* [TODO] More macros: `if-let` `when-let` `letfn` with destructuring
* [TODO] Conditional macros `cond` `condp` `case`
* [TODO] As much as listed here: http://clojure.org/macros

## 2012-August-30 / 0.1.0

* S-Expression evaluation
* Clojure and ClojureScript support
* _Evaluator_ to make up for lack of support for
  * Special forms
    * `if`
    * `do`
    * `quote`
  * Macros
    * `if-not`
    * `when`
    * `when-not`
    * `let`
    * `for-each` (same as `for` without `:let` `:when` `:while` forms)
    * `and`
    * `or`

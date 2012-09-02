# Changes and TODO

## TODO

* Implement `for` macro; deprecate/remove `for-each`
* Function declaration (both `fn` and `fn*`)
* De-structuring (in `let` `for` `fn`)
* More macros: `if-let` `when-let` `letfn` `cond` `condp` `case`
* As much as listed here: http://clojure.org/macros

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

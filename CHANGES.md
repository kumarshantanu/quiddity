# Changes and TODO

## TODO

* [TODO] As much as listed here: http://clojure.org/macros
* [TODO] Implement `for` macro; deprecate/remove `for-each`
* [TODO] Implement `doseq` macro
* [TODO] Namespace/qualified symbols support

## 2012-October-[TODO] / 0.2.0

* Destructuring primitives
* Retrofit destructuring into
  * `let`
  * `for-each`
* Conditional assignment macros with destructuring
  * `if-let`
  * `when-let`
* Conditional macros
  * `cond`
  * `condp`
  * `case`
  * `while`
* Uniform deref (@) support - as a built-in lib fn
* Thread macros
  * `->`
  * `->>`
* [TODO] Creating functions `fn` and `fn*` macros
* [TODO] Exception handling
  * `try`
  * `catch`
  * `finally`

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

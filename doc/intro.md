# Introduction to quiddity

## Evaluation

You can invoke Quiddity evaluation with `quiddity.core/evaluate`:

Syntax:

```clojure
(evaluate form env err-h)
```

* `form`  is a valid Clojure form
* `env`   is user-specified environment - a collection of maps for value lookup
* `err-h` is error handler (function that accepts error message and deals with it)

Clojure example:

```clojure
(evaluate (read-string "(inc (dec (+ n 10)))") [{:+ + :inc inc :dec dec :n 20}]
          #(throw (RuntimeException. %)))
```

ClojureScript example:

```clojure
(evaluate (cljs.reader/read-string "(inc (dec (+ n 10)))") [{:+ + :inc inc :dec dec :n 20}]
          #(throw (js/Error %)))
```


### How lookup works

The _symbol_ values are looked up in each of the maps in `env`. When not found
in any of them, the error handler is called with the error message as argument.

The expression symbols can be present in `env` as keys, or their corresponding
keywords as the keys. For example, `(foo 42)` will look for both `'foo` and
`:foo` as a key in `env`.


## Limitation

Quiddity supports only limited forms in the S-expressions it evaluates. The
following are not supported (note that shortcuts to special forms eg. `'`, `#'`,
`#(...)`, `@` etc. are automatically expanded by the reader):

* Special forms (some are re-implemented using evaluator, see _Evaluator_)
* Macros (some are re-implemented using evaluator, see _Evaluator_)
* Destructuring (re-implemented using evaluator, see _Evaluator_)
* Classnames (they are treated as ordinary symbols)
* Creating functions (re-implemented using evaluator, see _Evaluator_)
* Namespaces


## Evaluator

In order to make up for some of the limitations listed above, Quiddity supports
something called an _evaluator_. An evaluator is an annotated function that

* accepts `env` as first argument
* accepts remaining arguments as listed in the expression
* is responsible for evaluating the arguments

Evaluators may appear at any levels of the S-expression. You can use
`quiddity.core/make-evaluator`, eg. `(make-evaluator fn-object)` to annotate a
function as an evaluator. Evaluators are passed as part of `env` during
evaluation just like other functions. However, they are treated specially
during invocation.

The following evaluators are provided in `quiddity.lib/all` (map):

* Special forms equivalent
  * `do`
  * `if`
  * `quote`
* Macros equivalent
  * `->`
  * `->>`
  * `and`
  * `case`
  * `cond`
  * `condp`
  * `fn` with destructuring, without pre and post conditions
  * `for-each`  with destructuring (same as `for` without `:let`, `:when`, `:while` forms)
  * `if-not`
  * `let` with destructuring
  * `or`
  * `when`
  * `when-not`
  * `while`
* Function equivalent
  * `deref` a.k.a `@`

# quiddity

An [S-Expression](http://en.wikipedia.org/wiki/S-expression) evaluation library
for Clojure and ClojureScript. Unlike Clojure's `eval`, _Quiddity_ lets you work
with limited forms using user-specified _environment_.


## Usage

This library is not on Clojars yet.

Leiningen dependency: `[quiddity "0.1.0-SNAPSHOT"]`


### Evaluation

You can invoke Quiddity evaluation with `quiddity.core/evaluate`:

Syntax:

```clojure
(evaluate form env err-h)
```

`form`  a valid Clojure form
`env`   user-specified environment -- a collection of maps for value lookup
`err-h` error handler (function that accepts error message and deals with it)

Clojure example:

```clojure
(evaluate (read-string "(inc (dec (+ n 10)))") [{:+ + :inc inc :dec dec :n 20}]
          #(throw (RuntimeException. %)))
```

ClojureScript example:

```clojure
(evaluate (read-string "(inc (dec (+ n 10)))") [{:+ + :inc inc :dec dec :n 20}]
          #(throw (js/Error %)))
```


#### How lookup works

The _symbol_ values are looked up in each of the maps in `env`. When not found
in any of them, the error handler is called with the error message as argument.

The expression symbols can be present in `env` as keys, or their corresponding
keywords as the keys. For example, `(foo 42)` will look for both `'foo` and
`:foo` as a key in `env`.


### Limitation

Quiddity supports only limited forms in the S-expressions it evaluates. The
following are not supported (note that shortcuts to special forms and macros
eg. `'`, `#'`, `#(...)` etc. are automatically expanded by the reader):

* Special forms
* Macros
* Destructuring
* Creating functions


### Evaluator

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
  * `if`
  * `do`
  * `quote`
* Macros equivalent
  * `if-not`
  * `when`
  * `when-not`
  * `let`
  * `for-each` (same as `for` without `:let`, `:when`, `:while` forms)
  * `and`
  * `or`


## Development

### Running tests

Clean older stuff if any (required for ClojureScript testing)

```
lein dev clean
```

Run the tests

```
lein dev test  # test with Clojure & ClojureScript (needs `phantomjs` installed)
lein all test  # test with all supported Clojure versions
```


### Building a JAR (and installing to local repo)

Assuming the line `:hooks [leiningen.cljsbuild]` in `project.clj` is
commented out:

```
lein jar     # create the JAR file
lein install # install JAR to local Maven repo
```


## Getting in touch

On Twitter: [@kumarshantanu](https://twitter.com/kumarshantanu)

E-mail: kumar(dot)shantanu at gmail.com


## License

Copyright © 2012 Shantanu Kumar

Distributed under the Eclipse Public License, the same as Clojure.

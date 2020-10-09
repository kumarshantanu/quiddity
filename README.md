# quiddity

[![cljdoc badge](https://cljdoc.org/badge/quiddity/quiddity)](https://cljdoc.org/d/quiddity/quiddity)

An [S-Expression](http://en.wikipedia.org/wiki/S-expression) evaluation library
for Clojure and ClojureScript. Unlike Clojure's `eval`, _Quiddity_ lets you work
with limited forms using user-specified _environment_.


## Usage

Leiningen dependency: `[quiddity "0.3.0"]`

Supported versions: Clojure 1.7 or higher, ClojureScript 1.9 or higher


### Evaluation Quickstart

You can invoke Quiddity evaluation with `quiddity.core/evaluate`:

Syntax:

```clojure
(evaluate form env err-h)
```

* `form`  is a valid Clojure form
* `env`   is user-specified environment -- a collection of maps for value lookup
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


## Documentation

See [Introduction to Quiddity](doc/intro.md)


## Development

### Running tests

```shell
lein do clean, test      # test against lowest supported Clojure version
lein do clean, clj-test  # test against all supported Clojure versions
lein do clean, cljs-test # test against all supported ClojureScript versions
```


## License

Copyright © 2012-2020 Shantanu Kumar

Distributed under the Eclipse Public License, the same as Clojure.

(defproject quiddity "0.2.0-SNAPSHOT"
  :description "An S-Expression evaluation library for Clojure and ClojureScript"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  ;; :dependencies [[org.clojure/clojure "1.4.0"]]
  :warn-on-reflection true
  :plugins [[lein-cljsbuild "0.2.8"]]
  :profiles {:tst {:dependencies [[clip-test "0.1.0"]]}
             :jst {:source-paths ["src" "test"]
                   ;; Enable the lein hooks for: clean, compile, test, and jar.
                   :hooks [leiningen.cljsbuild]
                   :cljsbuild {:crossovers [quiddity.core quiddity.lib
                                            quiddity.core-test quiddity.lib-test
                                            clip-test.internal]
                               ;; Command for running the unit tests in CLJS
                               ;;     $ lein cljsbuild test
                               :test-commands {"unit" ["phantomjs"
                                                       "run-tests.js"]}
                               :builds {:test {:source-path "test-cljs"
                                               :compiler
                                               {:output-to "target/quiddity-test.js"
                                                ;; :optimizations nil
                                                :pretty-print true}}}}}
             :1.2 {:dependencies [[org.clojure/clojure "1.2.1"]]}
             :1.3 {:dependencies [[org.clojure/clojure "1.3.0"]]}
             :1.4 {:dependencies [[org.clojure/clojure "1.4.0"]]}
             :1.5 {:dependencies [[org.clojure/clojure "1.5.0-alpha6"]]}}
  :aliases {"all" ["with-profile" "1.2,tst:1.3,tst:1.4,tst:1.5,tst"]
            "dev" ["with-profile" "1.4,tst,jst"]})

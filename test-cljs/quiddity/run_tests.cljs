(ns quiddity.run-tests
  (:require [quiddity.core-test      :as core-test]
            [quiddity.lib-test       :as lib-test]
            [clip-test.testutil-cljs :as tu]))


(defn ^:export run
  []
  (core-test/test-ns-hook)
  (lib-test/test-ns-hook)
  (tu/print-test-summary))
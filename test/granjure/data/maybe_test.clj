(ns granjure.data.maybe-test
  (:use clojure.test
        granjure.data.maybe
        granjure.control.monad_test))

(deftest monad-test
  (testing-monad maybe-unit))
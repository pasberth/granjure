(ns granjure.data.maybe-test
  (:use clojure.test
        granjure.data.maybe
        granjure.control.applicative_test
        granjure.control.monad_test))

(deftest applicative_test
  (testing-applicative (maybe-unit 42)))
(deftest monad-test
  (testing-monad maybe-unit))
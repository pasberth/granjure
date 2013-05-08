(ns granjure.data.maybe-test
  (:use clojure.test
        granjure.data.maybe
        granjure.control.applicative_test
        granjure.control.monad_test
        granjure.control.monad.plus_test)
  (:import [granjure.data.maybe Just Nothing]))

(deftest applicative_test
  (testing-applicative (maybe-unit 42)))
(deftest monad-test
  (testing-monad maybe-unit))
(deftest monad-plus-test
  (testing-monad-plus maybe-unit (Nothing.)))
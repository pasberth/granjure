(ns granjure.data.maybe-test
  (:use clojure.test
        granjure.data.maybe
        granjure.control.applicative_test
        granjure.control.monad_test
        granjure.control.monad.plus_test)
  (:import [granjure.data.maybe Just Nothing]))

(defn maybe-unit [a] (Just. a))

(deftest applicative-test
  (testing-applicative (Just. 42)))
(deftest monad-test
  (testing-monad maybe-unit))
(deftest monad-plus-test
  (testing-monad-plus maybe-unit (Nothing.)))
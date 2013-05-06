(ns granjure.control.monad.identity-test
  (:use clojure.test
        granjure.control.monad.identity
        granjure.control.monad_test)
  (:import granjure.control.monad.identity.Identity))

(deftest monad-test
  (testing-monad identity-return))
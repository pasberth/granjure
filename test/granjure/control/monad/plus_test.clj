(ns granjure.control.monad.plus_test
  (:refer-clojure :exclude [when])
  (:use clojure.test
        granjure.control
        granjure.control.functor
        granjure.control.monad
        granjure.control.monad.plus))

(defn testing-monad-plus [monad-unit monad-zero]
  (testing "MonadPlus"
    (is (= (>>= monad-zero return) monad-zero))
    (is (= (>> (monad-unit 42) monad-zero) monad-zero)))
  (testing "guard"
    (is (= (specialize (>> (guard true) (return 42)) monad-zero) (monad-unit 42)))
    (is (= (specialize (>> (guard false) (return 42)) monad-zero) monad-zero))))
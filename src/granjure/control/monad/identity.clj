(ns granjure.control.monad.identity
  (:refer-clojure :exclude [when])
  (:use granjure.primitive
        granjure.control.functor
        granjure.control.monad))

(defrecord Identity [run-identity])

(extend-type Identity
  Functor
    (commutated-fmap [m f] (Identity. (f (:run-identity m))))
  Monad
    (unit [_ a] (Identity. a))
    (bind [m k] (k (:run-identity m))))

(def identity-return (cfn [a] (Identity. a)))
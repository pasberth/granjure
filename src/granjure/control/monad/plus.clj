(ns granjure.control.monad.plus
  (:refer-clojure :exclude [when])
  (:use granjure.primitive
        granjure.control
        granjure.control.monad))

(defprotocol MonadPlus
  (zero-monad [this])
  (plus-monad [m k]))

(defrecord Mzero [])

(extend-protocol TypeClass
  Mzero
    (infer-context [this] nil)
    (specialize [t cxt] (zero-monad cxt)))

(def mzero (Mzero.))
(def mplus (cfn [m k] (plus-monad m k)))

(def guard (cfn [p] (if p (return '()) mzero)))
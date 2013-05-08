(ns granjure.control.monad.plus
  (:refer-clojure :exclude [when])
  (:use granjure.primitive
        granjure.control
        granjure.control.monad))

(defprotocol MonadPlus
  (zero-monad [this])
  (plus-monad [m k]))

(defrecord Mzero [])
(defrecord Mplus [m k])

(extend-protocol TypeClass
  Mzero
    (specialize [_ cxt] (zero-monad cxt))
  Mplus
    (specialize [this cxt] (plus-monad (try-specialize (:m this) cxt) (try-specialize (:k this) cxt))))

(def mzero (Mzero.))
(def mplus (cfn [m k] (specialize-when [m] (Mplus. m k))))

(def guard (cfn [p] (if p (return '()) mzero)))
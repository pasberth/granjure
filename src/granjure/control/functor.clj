(ns granjure.control.functor
  (:use granjure.core
        granjure.primitive))

(defprotocol Functor
  (commutated-fmap [functor f]))

(def fmap (cfn [f functor] (commutated-fmap functor f)))
(def <$> fmap)
(def <$  (compose <$> const))
(ns granjure.control.functor
  (:use granjure.core
        granjure.primitive
        granjure.control))

(defprotocol Functor
  (commutated-fmap [functor f]))

(defrecord Fmap [f fr])

(extend-protocol TypeClass
  Fmap
    (infer-context [this] (infer-context (:fr this)))
    (specialize [fr cxt] (commutated-fmap (try-specialize (:fr fr) cxt) (:f fr))))

(def fmap (cfn [f fr] (specialize-when [fr] (Fmap. f fr))))
(def <$> fmap)
(def <$  (compose <$> const))
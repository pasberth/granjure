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
    (specialize [fr cxt] (commutated-fmap (specialize (:fr fr) cxt) (:f fr))))

(def fmap (cfn [f fr]
  (let [cxt (infer-context fr)] (if cxt
    (specialize (Fmap. f fr) cxt)
    (Fmap. f fr)))))
(def <$> fmap)
(def <$  (compose <$> const))
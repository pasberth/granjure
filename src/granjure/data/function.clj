(ns granjure.data.function
  (:refer-clojure :exclude [empty])
  (:use clojure.test
        granjure.primitive
        granjure.control
        [granjure.control.category :exclude [id compose]]
        granjure.control.arrow
        granjure.control.functor
        granjure.control.applicative
        granjure.data.tuple))

(extend-type clojure.lang.Fn
  Category
    (identity-category [this] id)
    (compose-category  [f g] (compose f g))
  Arrow
    (lift-arrow [this f] f)
    (apply-first [f xs] (apply tuple (f (first xs)) (second xs) (nnext xs)))
  Functor
    (commutated-fmap [f g] (compose g f))
  Applicative
    (lift-applicative [this a] (cfn [_] a))
    (apply-applicative [f g] (cfn [x] (f x (g x)))))
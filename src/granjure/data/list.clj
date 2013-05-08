(ns granjure.data.list
  (:refer-clojure :exclude [when empty])
  (:use granjure.control
        granjure.control.functor
        granjure.control.applicative
        granjure.control.applicative.alternative
        granjure.control.monad
        granjure.control.monad.plus))

(extend-type clojure.lang.Seqable
  Functor
    (commutated-fmap [m f] (map f m))
  Monad
    (unit [_ a] (list a))
    (bind [m k] (apply concat (map #(try-specialize (k %) m) m)))
  MonadPlus
    (zero-monad [_] (list))
    (plus-monad [m k] (concat m k)))
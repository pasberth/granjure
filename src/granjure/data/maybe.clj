(ns granjure.data.maybe
  (:use granjure.control.functor
        granjure.control.applicative
        granjure.control.monad
        granjure.control.monad.plus))

(defrecord Just [a])
(defrecord Nothing [])

(defn maybe-unit [a] (Just. a))

(extend-protocol Functor
  Just
    (commutated-fmap [this f] (Just. (f (:a this))))
  Nothing
    (commutated-fmap [this f] (Nothing.)))

(extend-protocol Applicative
  Just
    (lift-applicative [this a] (Just. a))
    (apply-applicative [v u] (fmap (:a v) u))
  Nothing
    (lift-applicative [this a] (Just. a))
    (apply-applicative [v u] (Nothing.)))

(extend-protocol Alternative
  Just
    (zero-applicative [_] (Nothing.))
    (plus-applicative [v u] (mplus v u)))

(extend-protocol Monad
  Just
    (unit [this a] (maybe-unit a))
    (bind [m k] (k (:a m)))
  Nothing
    (unit [this a] (maybe-unit a))
    (bind [m k] (Nothing.)))

(extend-protocol MonadPlus
  Just
    (monad-zero [_] = (Nothing.))
    (monad-plus [m _] m)
  Nothing
    (monad-zero [_] = (Nothing.))
    (monad-plus [_ m] m))
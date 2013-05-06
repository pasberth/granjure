(ns granjure.data.maybe
  (:use granjure.control.functor
        granjure.control.monad))

(defrecord Just [a])
(defrecord Nothing [])

(defn maybe-unit [a] (Just. a))

(extend-protocol Functor
  Just
    (commutated-fmap [this f] (Just. (f (:a this))))
  Nothing
    (commutated-fmap [this f] (Nothing.)))

(extend-protocol Monad
  Just
    (unit [this a] (maybe-unit a))
    (bind [m k] (k (:a m)))
  Nothing
    (unit [this a] (maybe-unit a))
    (bind [m k] (Nothing.)))
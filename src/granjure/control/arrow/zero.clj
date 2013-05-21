(ns granjure.control.arrow.zero
  (:use infixing.core
        [granjure.primitive :exclude [id compose]]
        granjure.control
        granjure.control.category
        granjure.control.arrow))

(defprotocol ArrowZero
  (zero-arrow [this]))

(defrecord Zero [])

(extend-protocol TypeClass
  Zero
    (specialize [this cxt] (zero-arrow cxt)))

(def arrzero (Zero.))
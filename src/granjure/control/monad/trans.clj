(ns granjure.control.monad.trans
  (:use granjure.primitive
        granjure.control))

(defprotocol MonadTrans
  (lift-monad [this m]))

(defrecord Lift [m])

(extend-protocol TypeClass
  Lift
    (specialize [t cxt] (lift-monad cxt (:m t))))

(def lift (cfn [m] (Lift. m)))
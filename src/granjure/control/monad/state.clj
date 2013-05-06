(ns granjure.control.monad.state
  (:refer-clojure :exclude [get when])
  (:use granjure.primitive
        granjure.control
        granjure.control.monad
        granjure.control.monad.trans
        granjure.control.monad.identity))

(defprotocol MonadState
  (get-state [this])
  (put-state [this s]))

(defrecord Get [])
(defrecord Put [s])

(extend-protocol TypeClass
  Get
    (infer-context [this] nil)
    (specialize [this cxt] (get-state cxt))
  Put
    (infer-context [this] nil)
    (specialize [this cxt] (put-state cxt (:s this))))

(def get (Get.))
(def put (cfn [a] (Put. a)))
(def modify (cfn [f]
  (do-m (s <- get :in (put (f s))))))
(def gets (cfn [f]
  (do-m (s <- get :in (return (f s))))))


(defrecord StateT [run-state-t])

(extend-type StateT
  Monad
    (unit [_ a] (StateT. (cfn [s] (return (list a s)))))
    (bind [m k] (StateT. (cfn [s] (do-m (
      [a s] <- ((:run-state-t m) s) :.
      m- <- (return (specialize (k a) m)) :in
      ((:run-state-t m-) s))))))
  MonadState
    (get-state [_] (StateT. (cfn [s] (return (list s s)))))
    (put-state [_ s] (StateT. (cfn [_] (return (list nil s)))))
  MonadTrans
    (lift-monad [_ m] (StateT. (cfn [s] (do-m ( x <- m :in (return (list x s)) ))))))

(def state-return (cfn [a] (StateT. (cfn [s] (identity-return (list a s))))))
(def run-state (cfn [m a] (:run-identity ((:run-state-t (>> (state-return nil) m)) a))))
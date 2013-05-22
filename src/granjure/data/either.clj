(ns granjure.data.either
  (:refer-clojure :exclude [when])
  (:use granjure.primitive
        granjure.control.functor
        granjure.control.monad
        granjure.data))

(defmacro eithern [n]
  (let [ fs (for [i (range n)] (symbol (str "f" i))) 
         go (symbol (str "f" (dec n))) ]
    `(cfn [~'a] (fn [~@fs & ~'_] (~go ~'a)))))

(definductive Left [a]
  clojure.lang.IFn
    (invoke [_ f] ((eithern 1) a f))
    (invoke [_ f g] ((eithern 1) a f g))
    (applyTo [_ args] (apply (eithern 1) a args)))

(definductive Right [a]
  clojure.lang.IFn
    (invoke [_ f] ((eithern 2) a f))
    (invoke [_ f g] ((eithern 2) a f g))
    (applyTo [_ args] (apply (eithern 2) a args)))

(extend-protocol Functor
  Left
    (commutated-fmap [this _] this)
  Right
    (commutated-fmap [this f] (Right. (f (.a this)))))

(extend-protocol Monad
  Left
    (unit [_ a] (Right. a))
    (bind [m _] m)
  Right
    (unit [_ a] (Right. a))
    (bind [m k] (k (.a m))))

(def left (cfn [f e] (e (cfn [a] (Left. (f a))) (cfn [a] (Right. a)))))
(def right (cfn [f e] (e (cfn [a] (Left. a)) (cfn [a] (Right. (f a))))))
(def mirror (cfn [e] (e (cfn [a] (Right. a)) (cfn [a] (Left. a)))))
(ns granjure.control.monad
  (:refer-clojure :exclude [when])
  (:use infixing.core
        granjure.primitive
        granjure.control))

(def monad-rule
  (rules
    (infixl 1 '>>)
    (infixl 1 '>>=)
    primitive-rule))

(def do-rule
  (rules
    (infix-map  -1 '<- (fn [x m] (fn [k] `(~'>>= ~m (fn [~x] ~k)))))
    (infixr-map -3 :>>  (fn [m k] `(~'>> ~m ~k)))
    primitive-rule))

(defmacro do-m [code]
  (infixing do-rule code))

(defprotocol Monad
  (unit [this a])
  (bind [m k]))

(defrecord Unit [a])
(defrecord Bind [m k])

(extend-protocol TypeClass
  Unit
    (infer-context [this] nil)
    (specialize [this cxt] (unit cxt (:a this)))
  Bind
    (infer-context [this] (infer-context (:m this)))
    (specialize [this cxt]
      (let [m (specialize (:m this) cxt)
            r (bind m (:k this))]
        (specialize r cxt))))

(def return (cfn [a] (Unit. a)))
(def >>= (cfn [m k] (specialize-when [m] (Bind. m k))))
(def >> (cfn [m k] (>>= m (fn [_] k))))

(def join (cfn [m]
  (>>= m identity)))

(def map-m (cfn [f xs]
  (letfn [(g [m x] (do-m (x <- (f x) :. xs <- m :in (return (cons x xs)))))]
    (reduce g (return '()) (reverse xs)))))
(def for-m (flip map-m))

(def when (cfn [p a]
  (if p a (return '()))))
(def unless (flip when))
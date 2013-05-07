(ns granjure.control.applicative.alternative
  (:refer-clojure :exclude [empty])
  (:use infixing.core
        granjure.primitive
        granjure.control
        granjure.control.functor
        granjure.control.applicative))

(def alternative-rule
  (rules (infixl 3 '<|>)
         applicative-rule))

(defprotocol Alternative
  (zero-applicative [this])
  (plus-applicative [v u]))

(defrecord Empty [])
(defrecord Plus [v u])

(extend-protocol TypeClass
  Empty
    (infer-context [this] nil)
    (specialize [_ cxt] (zero-applicative cxt))
  Plus
    (infer-context [this] (or (infer-context (:v this)) (infer-context (:u this))))
    (specialize [w cxt] (plus-applicative (specialize (:v w) cxt) (specialize (:u w) cxt))))

(def empty (Empty.))
(def <|> (cfn [v u] (specialize-when [v u] (Plus. v u))))
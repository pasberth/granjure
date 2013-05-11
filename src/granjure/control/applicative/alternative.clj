(ns granjure.control.applicative.alternative
  (:refer-clojure :exclude [empty])
  (:use infixing.core
        granjure.primitive
        granjure.control
        granjure.control.functor
        granjure.control.applicative))

(def alternative-rule
  (merge-rule
    (infixl 3 '<|>)
    applicative-rule))

(defprotocol Alternative
  (zero-applicative [this])
  (plus-applicative [v u]))

(defrecord Empty [])
(defrecord Plus [v u])

(extend-protocol TypeClass
  Empty
    (specialize [_ cxt] (zero-applicative cxt))
  Plus
    (specialize [w cxt] (plus-applicative (try-specialize (:v w) cxt) (try-specialize (:u w) cxt))))

(def empty (Empty.))
(def <|> (cfn [v u] (specialize-when [v u] (Plus. v u))))
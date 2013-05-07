(ns granjure.control.applicative
  (:refer-clojure :exclude [empty])
  (:use infixing.core
        granjure.primitive
        granjure.control
        granjure.control.functor))

(def applicative-rule
  (rules (infixl 4 '<*>)
         (infixl 4 '<*)
         (infixl 4 '*>)
         (infixl 4 '<**>)))
(def alternative-rule
  (rules (infixl 3 '<|>)
         applicative-rule))

(defprotocol Applicative
  (lift-applicative [this a])
  (apply-applicative [v u]))

(defprotocol Alternative
  (zero-applicative [this])
  (plus-applicative [v u]))


(defrecord Pure [a])
(defrecord Apply [v u])
(defrecord Empty [])
(defrecord Plus [v u])

(extend-protocol TypeClass
  Pure
    (infer-context [this] nil)
    (specialize [v cxt] (lift-applicative cxt (:a v)))
  Apply
    (infer-context [this] (or (infer-context (:v this)) (infer-context (:u this))))
    (specialize [w cxt] (apply-applicative (specialize (:v w) cxt) (specialize (:u w) cxt)))
  Empty
    (infer-context [this] nil)
    (specialize [_ cxt] (zero-applicative cxt))
  Plus
    (infer-context [this] (or (infer-context (:v this)) (infer-context (:u this))))
    (specialize [w cxt] (plus-applicative (specialize (:v w) cxt) (specialize (:u w) cxt))))

(def pure (cfn [a] (Pure. a)))
(def <*> (cfn [v u] (specialize-when [v u] (Apply. v u))))
(def <**> (flip <*>))
(def lift-a2 (cfn [f a] (<*> (fmap f a))))
(def *> (lift-a2 (const id)))
(def <* (lift-a2 const))

(def empty (Empty.))
(def <|> (cfn [v u] (specialize-when [v u] (Apply. v u))))
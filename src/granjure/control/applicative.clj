(ns granjure.control.applicative
  (:use infixing.core
        granjure.primitive
        granjure.control
        granjure.control.functor))

(def applicative-rule
  (rules (infixl 4 '<*>)
         (infixl 4 '<*)
         (infixl 4 '*>)
         (infixl 4 '<**>)))

(defprotocol Applicative
  (lift-applicative [this a])
  (apply-applicative [v u]))


(defrecord Pure [a])
(defrecord Apply [v u])

(extend-protocol TypeClass
  Pure
    (infer-context [this] nil)
    (specialize [v cxt] (lift-applicative cxt (:a v)))
  Apply
    (infer-context [this] (or (infer-context (:v this)) (infer-context (:u this))))
    (specialize [w cxt] (apply-applicative (try-specialize (:v w) cxt) (try-specialize (:u w) cxt))))

(def pure (cfn [a] (Pure. a)))
(def <*> (cfn [v u] (specialize-when [v u] (Apply. v u))))
(def <**> (flip <*>))
(def lift-a2 (cfn [f a] (<*> (fmap f a))))
(def *> (lift-a2 (const id)))
(def <* (lift-a2 const))
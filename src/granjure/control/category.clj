(ns granjure.control.category
  (:use infixing.core
        [granjure.primitive :exclude [id compose]]
        granjure.control))

(def category-rule
  (rules
    (infixr-map 9 '.  (fn [f g] `(~'compose ~f ~g)))
    (infixr     1 '<<<)
    (infixr     1 '>>>)))

(defprotocol Category
  (identity-category [this])
  (compose-category [f g]))

(defrecord Identity [])
(defrecord Compose [f g])

(extend-protocol TypeClass
  Identity
    (infer-context [this] nil)
    (specialize [_ cxt] (identity-category cxt))
  Compose
    (infer-context [this] (or (infer-context (:f this)) (infer-context (:g this))))
    (specialize [this cxt] (compose-category (try-specialize (:f this) cxt) (try-specialize (:g this) cxt))))

(def id (Identity.))
(def compose (cfn [f g] (specialize-when [f g] (Compose. f g))))
(def <<< compose)
(def >>> (flip compose))
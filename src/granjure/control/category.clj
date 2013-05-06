(ns granjure.control.category
  (:use granjure.core
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
    (specialize [_ cxt] (compose-category (specialize (:f this) cxt) (specialize (:g this)))))

(def id (Identity.))
(def compose (cfn [f g]
  (let [cxt (infer-context (Compose. f g))] (if cxt
    (specialize (Compose. f g) cxt)
    (Compose. f g)))))
(def <<< compose)
(def >>> (flip compose))
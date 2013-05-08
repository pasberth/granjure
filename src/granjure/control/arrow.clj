(ns granjure.control.arrow
  (:use infixing.core
        [granjure.primitive :exclude [id compose]]
        granjure.control
        granjure.control.category))

(def arrow-rule
  (rules
    (infixr 3 '***)
    (infixr 3 '&&&)
    category-rule))

(defprotocol Arrow
  (lift-arrow [this a])
  (apply-first [arr pair]))

(defrecord Arr [f])
(defrecord Fst [arr pair])

(extend-protocol TypeClass
  Arr
    (specialize [this cxt] (lift-arrow cxt (:f this)))
  Fst
    (specialize [this cxt] (apply-first (try-specialize (:arr this) cxt) (:pair this))))

(def arr (cfn [f] (Arr. f)))
(def fst (cfn [arr pair] (specialize-when [arr] (Fst. arr pair))))
(def snd (cfn [f]
  (let [swp (fn [[a b & xs]] (apply list b a xs))]
    (>>> (>>> swp (fst f)) swp))))
(def *** (cfn [f g] (>>> (fst f) (snd g))))
(def &&& (cfn [f g] (>>> (arr (fn [x] (list x x))) (*** f g))))
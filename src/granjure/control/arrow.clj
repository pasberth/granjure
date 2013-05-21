(ns granjure.control.arrow
  (:use infixing.core
        [granjure.primitive :exclude [id compose]]
        granjure.control
        granjure.control.category
        granjure.data.tuple))

(def arrow-rule
  (merge-rule
    (infixr 3 '***)
    (infixr 3 '&&&)
    category-rule))

(defprotocol Arrow
  (lift-arrow [this a])
  (first-arrow [arr]))

(defrecord Arr [f])
(defrecord Fst [arr])

(extend-protocol TypeClass
  Arr
    (specialize [this cxt] (lift-arrow cxt (:f this)))
  Fst
    (specialize [this cxt] (first-arrow (try-specialize (:arr this) cxt))))

(def arr (cfn [f] (Arr. f)))
(def fst (cfn [arr] (specialize-when [arr] (Fst. arr))))
(def snd (cfn [f]
  (let [swp (fn [pair] (apply tuple (second pair) (first pair) (nnext pair)))]
    (>>> (>>> swp (fst f)) swp))))
(def *** (cfn [f g] (>>> (fst f) (snd g))))
(def &&& (cfn [f g] (>>> (arr (fn [x] (tuple x x))) (*** f g))))
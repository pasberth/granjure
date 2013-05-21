(ns granjure.control.arrow.choice
  (:use infixing.core
        [granjure.primitive :exclude [id compose]]
        granjure.control
        granjure.control.category
        granjure.control.arrow
        [granjure.data.either :exclude [left right]]))

(def arrow-choice-rule
  (merge-rule
    (infixr 2 '+++)
    (infixr 2 '|||)
    arrow-rule))

(defprotocol ArrowChoice
  (left-arrow [arr]))

(defrecord Lft [f])

(extend-protocol TypeClass
  Lft
    (specialize [this cxt] (left-arrow (:f this))))

(def left (cfn [f] (specialize-when [f] (Lft. f))))
(def right (cfn [f] (>>> (arr mirror) (>>> (left f) (arr mirror)))))
(def +++ (cfn [f g] (>>> (left f) (right g))))
(def ||| (cfn [f g] (>>> (+++ f g) (cfn [e] (e identity identity)))))
(ns granjure.control.arrow.plus
  (:use infixing.core
        [granjure.primitive :exclude [id compose]]
        granjure.control
        granjure.control.category
        granjure.control.arrow
        granjure.control.arrow.zero))

(defprotocol ArrowPlus
  (plus-arrow [v u]))

(defrecord Plus [v u])

(extend-protocol TypeClass
  Plus
    (specialize [this cxt] (plus-arro (try-specialize (:v this) cxt) (try-specialize (:u this) cxt))))

(def arrplus (cfn [v u] (specialize-when [v u] (Plus. v u))))
(def <+> arrplus)
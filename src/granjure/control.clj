(ns granjure.control)

(defprotocol TypeClass
  (infer-context [this])
  (specialize [this cxt]))

(extend-protocol TypeClass
  Object
    (infer-context [this] this)
    (specialize [this cxt] this))
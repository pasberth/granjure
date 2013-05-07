(ns granjure.control)

(defprotocol TypeClass
  (infer-context [this])
  (specialize [this cxt]))

(extend-protocol TypeClass
  Object
    (infer-context [this] this)
    (specialize [this cxt] this))

(defn specialize-when [inferences abstract-data]
  (let [cxt (reduce (fn [cxt inf] (or cxt (infer-context inf))) nil inferences)] (if cxt
    (specialize abstract-data cxt)
    abstract-data)))
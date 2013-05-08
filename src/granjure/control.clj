(ns granjure.control)

(defprotocol TypeClass
  (infer-context [this])
  (specialize [this cxt]))

(def type-class? #(satisfies? TypeClass %))

(defn try-specialize [abstract-data context]
  (if (type-class? abstract-data) (specialize abstract-data context) abstract-data))

(defn specialize-when [contexts abstract-data]
  (let [contexts (filter #(not (type-class? %)) contexts)] (if (empty? contexts)
    abstract-data
    (specialize abstract-data (first contexts)))))
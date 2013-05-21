(ns granjure.data.either
  (:use granjure.primitive
        granjure.data.tuple))

(defmacro eithern [n]
  (let [ eithers
         (for [i (range n)]
           (let [ fs (for [j (range n)] (symbol (str "f" j)))
                  go (symbol (str "f" i)) ]
             `(cfn [~'a ~@fs] (~go ~'a)))) ]
  `(tuple ~@eithers)))

(def ethleft (first (eithern 2)))
(def ethright (second (eithern 2)))

(defrecord Left [a]
  clojure.lang.IFn
    (invoke [_ f] (ethleft a f))
    (invoke [_ f g] (ethleft a f g))
    (applyTo [_ args] (apply ethleft a args)))

(defrecord Right [a]
  clojure.lang.IFn
    (invoke [_ f] (ethright a f))
    (invoke [_ f g] (ethright a f g))
    (applyTo [_ args] (apply ethright a args)))
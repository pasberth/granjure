(ns granjure.data.either
  (:use granjure.primitive))

(defmacro eithern [n]
  (let [ fs (for [i (range n)] (symbol (str "f" i))) 
         go (symbol (str "f" (dec n))) ]
    `(cfn [~'a] (fn [~@fs & ~'_] (~go ~'a)))))

(defrecord Left [a]
  clojure.lang.IFn
    (invoke [_ f] ((eithern 1) a f))
    (invoke [_ f g] ((eithern 1) a f g))
    (applyTo [_ args] (apply (eithern 1) a args)))

(defrecord Right [a]
  clojure.lang.IFn
    (invoke [_ f] ((eithern 2) a f))
    (invoke [_ f g] ((eithern 2) a f g))
    (applyTo [_ args] (apply (eithern 2) a args)))

(def left (cfn [f e] (e (cfn [a] (Left. (f a))) (cfn [a] (Right. a)))))
(def right (cfn [f e] (e (cfn [a] (Left. a)) (cfn [a] (Right. (f a))))))
(def mirror (cfn [e] (e (cfn [a] (Right. a)) (cfn [a] (Left. a)))))
(ns granjure.control.monad.zip
  (:refer-clojure :exclude [when])
  (:use infixing.core
        granjure.primitive
        granjure.control
        granjure.control.monad
        granjure.control.monad.plus))

(def mzip-rule
  (rules
    (infixr-map -2 :when  (fn [v a] (fn [m] (v `(>> (guard ~a) ~m)))))
    (infix-map  -5 '|     (fn [a v] (v `(return ~a))))
    do-rule))


(defmacro do-mzip [code]
  (infixing mzip-rule code))
(ns granjure.primitive
  (:use infixing.core))

(def primitive-rule
  (merge-rule
    (infix-map  -1 :=     (fn [x b] (fn [a] `(let [~x ~b] ~a))))
    (infixr-map -3 :.     (fn [v u] (fn [a] (v (u a)))))
    (infixr-map -4 :in    (fn [v a] (v a)))
    (infix-map  -5 :where (fn [a v] (v a)))))

(defn uncurry [f]
  (fn ([x]      (f x))
      ([x & xs] (apply (f x) xs))))

(defmacro cfn [params body]
  (reduce (fn [body param] `(uncurry (fn [~param] ~body))) body (reverse params)))

(def id      (cfn [x] x))
(def const   (cfn [x _] x))
(def $       (cfn [f x] (f x)))
(def flip    (cfn [f a b] (f b a)))
(def compose (cfn [f g x] (f (g x))))

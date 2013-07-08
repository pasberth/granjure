(ns granjure.statically.typed
  (:use [clojure.core.match :only (match)]
        infixing.core))

(defrecord TypeSystem [ast cxt])
(defrecord &&&        [first second])
(defrecord |||        [left right])
(defrecord Arr        [src tgt])
(defrecord Var        [sym])

(defmethod print-method &&& [v w]
  (if (condp instance? (:first v) Arr true ||| true false) (do
    (.write w "(")
    (print-method (:left v) w)
    (.write w ")")))
  (.write w " * ")
  (if (condp instance? (:second v) Arr true ||| true false) (do
    (.write w "(")
    (print-method (:second v) w)
    (.write w ")"))))

(defmethod print-method ||| [v w]
  (if (condp instance? (:left v) Arr true &&& true false) (do
    (.write w "(")
    (print-method (:left v) w)
    (.write w ")")))
  (.write w " | ")
  (if (condp instance? (:right v) Arr true &&& true false) (do
    (.write w "(")
    (print-method (:right v) w)
    (.write w ")"))))

(defmethod print-method Arr [v w]
  (if (instance? Arr (:src v)) (do
    (.write w "(")
    (print-method (:src v) w)
    (.write w ")"))
    (print-method (:src v) w))
  (.write w " -> ")
  (print-method (:tgt v) w))

(defmethod print-method Var [v w]
  (print-method (:sym v) w))

(defmulti  variables (fn [t] (type t)))
(defmethod variables &&& [t] (vec (distinct (concat (variables (:first t)) (variables (:second t))))))
(defmethod variables ||| [t] (vec (distinct (concat (variables (:left t)) (variables (:right t))))))
(defmethod variables Arr [t] (vec (distinct (concat (variables (:src t)) (variables (:tgt t))))))
(defmethod variables Var [t] [(:sym t)])
(defmethod variables :default [t] [])

(defmulti  replace-variable (fn [t src dest] (type t)))
(defmethod replace-variable &&& [t src dest] (&&&. (replace-variable (:first t) src dest) (replace-variable (:second t) src dest)))
(defmethod replace-variable ||| [t src dest] (|||. (replace-variable (:left t) src dest) (replace-variable (:right t) src dest)))
(defmethod replace-variable Arr [t src dest] (Arr. (replace-variable (:src t) src dest) (replace-variable (:tgt t) src dest)))
(defmethod replace-variable Var [t src dest] (if (= src t) dest t))

(defmulti  subst-variable          (fn [t src dest] [(type src), (type dest)]))
(defmethod subst-variable [||| Object] [t src dest] (or (subst-variable t (:left src) dest) (subst-variable t (:right src) dest)))
(defmethod subst-variable [Object |||] [t src dest] (or (subst-variable t src (:left dest)) (subst-variable t src (:right dest))))
(defmethod subst-variable [&&& &&&]    [t src dest] (subst-variable (subst-variable t (:first src) (:first dest)) (:second src) (:second dest)))
(defmethod subst-variable [Arr Arr]    [t src dest] (subst-variable (subst-variable t (:src src) (:src dest)) (:tgt src) (:tgt dest)))
(defmethod subst-variable [Var Object] [t src dest] (replace-variable t src dest))
(defmethod subst-variable :default     [t src dest] nil)

(defn rename-conflicting-variables [t r]
  (reduce
    (fn [t v] (loop [ t t i 0 ] (if (not (some #(= v %) (variables t))) t

        (recur (replace-variable t (Var. v) (Var. (keyword (str (name v) i)))) (inc i)))))
    t
    (variables r)))


(defn assume [type-system sym type]
  (TypeSystem. (:ast type-system) (conj (:cxt type-system) [sym type])))

(declare syntactic-type)
(declare lookup-type-of)

(defn statically-type [type-system ast] (cond 
  (seq?    ast) (syntactic-type type-system ast)
  (symbol? ast) (lookup-type-of type-system ast)
  :else         (type ast)))

(defn lookup-type-of [type-system sym]
  ((:cxt type-system) sym)
  ; TODO: もし存在しなければ type-system の ast で def されていないかを調べる
  )

(defmulti  syntactic-type                 (fn [type-system ast] (first ast)))
(defmethod syntactic-type 'fn*                [type-system ast] (letfn
  [ (typing [params expr] (let
      [ src        (reduce (fn [a b] (&&&. b a)) (map (fn [a] (Var. (keyword a))) (reverse params)))
      , assumption (reduce (fn [a b] (assume a b (Var. (keyword b)))) type-system params)
      , tgt        (statically-type assumption expr) ]
      (Arr. src tgt)))
  ] (match [(count ast)]
    [2] (let [ [ _ params-expr-pair ] ast ] (apply typing params-expr-pair))
    [3] (cond
      (symbol? (fnext ast)) (let [ [ _ name params-expr-pair ] ast ] (apply typing params-expr-pair))
      (vector? (fnext ast)) (let [ [ _ params expr ]           ast ] (typing params expr))
      (seq?    (fnext ast)) (let [ [ _ px-pair1 px-pair2 ]     ast ] (|||. (apply typing px-pair1) (apply typing px-pair2)))
      :else                 (throw (IllegalArgumentException.)))
    :else (throw (IllegalArgumentException.)))))
(defmethod syntactic-type :default [type-system ast] (let
  [ ast'   (macroexpand ast)
  , macro? (not= ast ast')
  , apply? (not macro?)
  ] (letfn
  [ (syntactic-type-macro [] (statically-type type-system ast'))
  , (syntactic-type-apply [] (let
      [ [ f & xs ] (map #(statically-type type-system %) ast)
      , xs (reduce (fn [a b] (&&&. b a)) (reverse xs))
      , xs (rename-conflicting-variables xs f)
      ] (condp instance? f
      Arr (:tgt (subst-variable f (:src f) xs))
      nil)))
  ] (cond
    macro? (syntactic-type-macro)
    apply? (syntactic-type-apply)))))

(def constraint-rule (merge-rule
  (infixr-map 7 '* (fn [a b] `(&&&. ~a ~b)))
  (infixr-map 6 '| (fn [a b] `(|||. ~a ~b)))
  (infixr-map 5 '-> (fn [a b] `(Arr. ~a ~b)))))

(defmacro constraint [& code]
  (letfn [ (expr [code] (infixing constraint-rule
                          (for [x code] (cond
                            (keyword? x) `(Var. ~x)
                            (seq? x) (expr x)
                            :else x)))) ]
    (expr code)))
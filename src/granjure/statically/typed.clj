(ns granjure.statically.typed
  (:use [clojure.core.match :only (match)]))

(defrecord TypeSystem [ast cxt])
(defrecord &&&        [first second])
(defrecord |||        [left right])
(defrecord Arr        [src tgt])
(defrecord Var        [sym])

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
      [ src        (reduce (fn [a b] (&&&. a b)) (map (fn [a] (Var. a)) (reverse params)))
      , assumption (reduce (fn [a b] (assume a b (Var. b))) type-system params)
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
  [ (syntactic-type-macro [] (syntactic-type  type-system ast'))
  , (syntactic-type-apply [] nil)
  ] (cond
    macro? (syntactic-type-macro)
    apply? (syntactic-type-apply)))))
(ns granjure.statically.typed
  (:use [clojure.core.match :only (match)]
        infixing.core))

(defrecord TypeSystem [ast cxt])
(defrecord Hold       [expected actual])
(defrecord &&&        [first second])
(defrecord |||        [left right])
(defrecord Arr        [src tgt])
(defrecord Tag        [t tag])
(defrecord Var        [sym])

(defmethod print-method &&& [v w]
  (if (condp instance? (:first v) Arr true ||| true false) (do
    (.write w "(")
    (print-method (:first v) w)
    (.write w ")"))
    (print-method (:first v) w))
  (.write w " * ")
  (if (condp instance? (:second v) Arr true ||| true false) (do
    (.write w "(")
    (print-method (:second v) w)
    (.write w ")"))
    (print-method (:second v) w)))

(defmethod print-method ||| [v w]
  (if (condp instance? (:left v) Arr true &&& true false) (do
    (.write w "(")
    (print-method (:left v) w)
    (.write w ")"))
    (print-method (:left v) w))
  (.write w " | ")
  (if (condp instance? (:right v) Arr true &&& true false) (do
    (.write w "(")
    (print-method (:right v) w)
    (.write w ")"))
    (print-method (:right v) w)))

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

(defmethod print-method Tag [v w]
  (if (contains? #{Arr &&& |||} (type (:tag v))) (do
    (.write w "(")
    (print-method (:t v) w)
    (.write w ")"))
    (print-method (:t v) w))
  (.write w " ")
  (if (contains? #{Arr &&& ||| Tag} (type (:tag v))) (do
    (.write w "(")
    (print-method (:tag v) w)
    (.write w ")"))
    (print-method (:tag v) w)))

(defmulti  variables (fn [t] (type t)))
(defmethod variables &&& [t] (vec (distinct (concat (variables (:first t)) (variables (:second t))))))
(defmethod variables ||| [t] (vec (distinct (concat (variables (:left t)) (variables (:right t))))))
(defmethod variables Arr [t] (vec (distinct (concat (variables (:src t)) (variables (:tgt t))))))
(defmethod variables Tag [t] (vec (distinct (concat (variables (:t t)) (variables (:tag t))))))
(defmethod variables Var [t] [(:sym t)])
(defmethod variables :default [t] [])

(defmulti  replace-variable (fn [t src dest] (type t)))
(defmethod replace-variable &&& [t src dest] (&&&. (replace-variable (:first t) src dest) (replace-variable (:second t) src dest)))
;(defmethod replace-variable ||| [t src dest] (|||. (replace-variable (:left t) src dest) (replace-variable (:right t) src dest)))
(defmethod replace-variable Arr [t src dest] (Arr. (replace-variable (:src t) src dest) (replace-variable (:tgt t) src dest)))
(defmethod replace-variable Tag [t src dest] (Tag. (replace-variable (:t t) src dest) (replace-variable (:tag t) src dest)))
(defmethod replace-variable Var [t src dest] (if (= src t) dest t))
(defmethod replace-variable Class [t src dest] (if (= src t) dest t))

(defmulti  subst-variable          (fn [t src dest] [(type src), (type dest)]))
;(defmethod subst-variable [||| Object] [t src dest] (or (subst-variable t (:left src) dest) (subst-variable t (:right src) dest)))
;(defmethod subst-variable [Object |||] [t src dest] (or (subst-variable t src (:left dest)) (subst-variable t src (:right dest))))
(defmethod subst-variable [&&& &&&]    [t src dest] (subst-variable (subst-variable t (:first src) (:first dest)) (subst-variable (:second src) (:first src) (:first dest)) (:second dest)))
(defmethod subst-variable [Arr Arr]    [t src dest] (subst-variable (subst-variable t (:src src) (:src dest)) (subst-variable (:tgt src) (:src src) (:src dest)) (:tgt dest)))
(defmethod subst-variable [Tag Tag]    [t src dest] (subst-variable (subst-variable t (:t src) (:t dest)) (subst-variable (:tag src) (:t src) (:t dest)) (:tag dest)))
(defmethod subst-variable [Var Var]    [t src dest] (replace-variable t src dest))
(defmethod subst-variable [Var Arr]    [t src dest] (replace-variable t src dest))
(defmethod subst-variable [Var Tag]    [t src dest] (replace-variable t src dest))
(defmethod subst-variable [Var Class]  [t src dest] (replace-variable t src dest))
(defmethod subst-variable [Class Class] [t src dest] (cond
  (= src dest)                     t
  (contains? (ancestors dest) src) (replace-variable t src dest)
  :else                            nil))
(defmethod subst-variable :default     [t src dest] nil)

(defmulti  application (fn [f x] (type f)))
(defmethod application Arr [f x] (:tgt (subst-variable f (:src f) x)))
(defmethod application ||| [f x] (or (application (:left f) x) (application (:right f) x)))
(defmethod application :default [f x] nil)

(defn rename-conflicting-variables [t r]
  (reduce
    (fn [t v] (loop [ t t i 0 ] (if (not (some #(= v %) (variables t))) t

        (recur (replace-variable t (Var. v) (Var. (keyword (str (name v) i)))) (inc i)))))
    t
    (variables r)))

(def empty-system (TypeSystem. nil {}))

(defn assume [type-system sym type]
  (TypeSystem. (:ast type-system) (conj (:cxt type-system) [sym (Hold. type nil)])))

(declare syntactic-type)
(declare lookup-type-of)

(defn statically-type [type-system ast] (cond 
  (seq?    ast) (syntactic-type type-system ast)
  (symbol? ast) (lookup-type-of type-system ast)
  (vector? ast) (let [ types (distinct (map #(statically-type type-system %) ast))
                       tag   (if (empty? types) (Var. :a) (reduce (fn [a b] (|||. b a)) (reverse types)))
                     ]
                  (Tag. (type ast) tag))
  :else         (type ast)))

(defn lookup-type-of [type-system sym]
  (let [ hold ((:cxt type-system) sym) ] (or (:expected hold) (:actual hold)))
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
      ] (application f xs)))
  ] (cond
    macro? (syntactic-type-macro)
    apply? (syntactic-type-apply)))))

(declare syntactic-type-system)

(defn statically-type-system [type-system ast] (cond
  (seq? ast) (syntactic-type-system type-system ast)
  :else      type-system))

(defmulti  syntactic-type-system  (fn [type-system ast] (first ast)))
(defmethod syntactic-type-system 'do  [type-system ast]
  (reduce (fn [ts ast] (statically-type-system ts ast))
          type-system
          (rest ast)))
(defmethod syntactic-type-system 'def [type-system ast] (let
  [ cxt  (:cxt type-system)
  , id   (fnext ast)
  , hold (cxt id)
  ]
  (TypeSystem. (:ast type-system)
               (conj cxt
                     [ id
                     , (Hold. (:expected hold) (statically-type type-system (first (nnext ast)))) ]))))
(defmethod syntactic-type-system :default [type-system ast] (let
  [ ast'   (macroexpand ast)
  , macro? (not= ast ast')
  ] (cond
    macro? (statically-type-system type-system ast')
    :else  type-system)))

(defn hold-contradiction? [hold] (or
  (not (:actual hold))
  (not= (subst-variable (:expected hold) (:expected hold) (:expected hold)) (subst-variable (:actual hold) (:actual hold) (:expected hold)))))

(defn contradiction? [type-system]
  (some hold-contradiction? (vals (:cxt type-system))))

(def constraint-rule (merge-rule
  (infixl-space 9     (fn [a b] `(Tag. ~a ~b)))
  (infixr-map   7 '*  (fn [a b] `(&&&. ~a ~b)))
  (infixr-map   6 '|  (fn [a b] `(|||. ~a ~b)))
  (infixr-map   5 '-> (fn [a b] `(Arr. ~a ~b)))))

(defmacro constraint [& code]
  (letfn [ (expr [code] (infixing constraint-rule
                          (for [x code] (cond
                            (keyword? x) `(Var. ~x)
                            (seq? x) (expr x)
                            :else x)))) ]
    (expr code)))

(defmacro hold [& code]
  `(Hold. (constraint ~@code) (constraint ~@code)))

(defmacro typing-with [type-system & ast]
  (let [ type-system `(TypeSystem. '(do ~@ast) (:cxt ~type-system)) ]

    `(do ~@ast
      (syntactic-type-system ~type-system '(do ~@ast)))))
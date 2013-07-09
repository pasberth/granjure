(ns granjure.statically.typed.systems
  (:use granjure.statically.typed)
  (:import [granjure.statically.typed TypeSystem]))

(def clojure-core-system (TypeSystem. nil
  { 'identity   (hold :a -> :a)
  , 'constantly (hold :a -> :b -> :a)
  }))

(ns granjure.statically.typed.systems
  (:use granjure.statically.typed)
  (:import [granjure.statically.typed TypeSystem]))

(def clojure-core-system (TypeSystem. nil
  { 'identity   (hold :a -> :a)
  , 'constantly (hold :a -> :b -> :a)
  , 'reduce     (hold ((:a * :b -> :a) * :a * clojure.lang.Seqable :b -> :a)
                    | ((:a * :b -> :a) *      clojure.lang.Seqable :b -> :a))
  , 'if         (hold Boolean * :a * :a -> :a)
  , '=          (hold :a * :b -> Boolean)
  }))

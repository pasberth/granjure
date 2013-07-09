(ns granjure.statically.typed.systems
  (:use granjure.statically.typed)
  (:import [granjure.statically.typed TypeSystem]))

(def clojure-core-system (TypeSystem. nil
  { 'identity   (hold :a -> :a)
  , 'constantly (hold :a -> :b -> :a)
  , 'reduce     (hold ((:a * :b -> :a) * :a * clojure.lang.Seqable -> :a)
                    | ((:a * :b -> :a) *      clojure.lang.Seqable -> :a))
  }))

(ns granjure.data.tuple
  (:use granjure.primitive))

(def tuple2 (cfn [a b] (list a b)))
(def tuple3 (cfn [a b c] (list a b c)))
(def tuple4 (cfn [a b c d] (list a b c d)))
(def tuple5 (cfn [a b c d e] (list a b c d e)))
(def tuple6 (cfn [a b c d e f] (list a b c d e f)))
(def tuple7 (cfn [a b c d e f g] (list a b c d e f g)))
(def tuple8 (cfn [a b c d e f g h] (list a b c d e f g h)))
(def tuple9 (cfn [a b c d e f g h i] (list a b c d e f g h i)))
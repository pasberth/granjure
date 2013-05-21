(ns granjure.data.tuple
  (:use granjure.primitive))

(deftype Tuple [xs]
  clojure.lang.Seqable
    (seq   [_] xs)
  clojure.lang.IPersistentCollection
    (cons  [_ a] (Tuple. (cons xs a)))
    (count [_] (count xs))
    (empty [_] (Tuple. []))
    (equiv [_ a] (= xs a))
  clojure.lang.ISeq
    (first [_] (first xs))
    (next  [_] (next xs))
    (more  [_] (rest xs)))

(defn tuple [& xs]
  (Tuple. xs))

(def tuple2 (cfn [a b] (tuple a b)))
(def tuple3 (cfn [a b c] (tuple a b c)))
(def tuple4 (cfn [a b c d] (tuple a b c d)))
(def tuple5 (cfn [a b c d e] (tuple a b c d e)))
(def tuple6 (cfn [a b c d e f] (tuple a b c d e f)))
(def tuple7 (cfn [a b c d e f g] (tuple a b c d e f g)))
(def tuple8 (cfn [a b c d e f g h] (tuple a b c d e f g h)))
(def tuple9 (cfn [a b c d e f g h i] (tuple a b c d e f g h i)))
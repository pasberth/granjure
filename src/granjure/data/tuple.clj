(ns granjure.data.tuple
  (:use granjure.primitive))

(deftype Tuple [xs]
  clojure.lang.Seqable
    (seq   [_] xs)
  clojure.lang.IPersistentCollection
    (cons  [_ a] (Tuple. (cons xs a)))
    (count [_] (count xs))
    (empty [_] (Tuple. []))
    (equiv [_ a] (= a xs))
  clojure.lang.ISeq
    (first [_] (first xs))
    (next  [_] (next xs)))

(defn tuple [& xs]
  (Tuple. xs))

(defmacro tuplen [n & args]
  (let [vars (for [i (range n)] (symbol (str "x" i)))]
    (if (empty? args)
      `(cfn [~@vars] (tuple ~@vars))
      `((cfn [~@vars] (tuple ~@vars)) ~@args))))

(def tuple2 (tuplen 2))
(def tuple3 (tuplen 3))
(def tuple4 (tuplen 4))
(def tuple5 (tuplen 5))
(def tuple6 (tuplen 6))
(def tuple7 (tuplen 7))
(def tuple8 (tuplen 8))
(def tuple9 (tuplen 9))
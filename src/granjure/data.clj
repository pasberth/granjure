(ns granjure.data)

(defmacro definductive [name [& fields] & opts+specs]
  `(deftype ~name [~@fields]
     clojure.lang.Seqable
       (seq [_] (seq (list '~name ~@fields)))
     clojure.lang.IPersistentCollection
       (count [_] ~(inc (count fields)))
       (empty [_] nil)
       (equiv [~'d ~'a] (= ~'a (seq ~'d)))
     clojure.lang.ISeq
       (first [_] ~name)
       (next [_] (list ~@fields))
     clojure.lang.IPersistentVector
       (nth [~'d ~'n] (nth (vec (seq ~'d)) ~'n))
      ~@opts+specs))
(ns granjure.control.applicative_test
  (:refer-clojure :exclude [empty])
  (:use clojure.test
        granjure.control
        granjure.control.applicative))

(defn testing-applicative [v]
  (testing "identity : pure id <*> v = v"
    (is (= v (<*> (pure identity) v))))
  (testing "composition : pure (.) <*> u <*> v <*> w = u <*> (v <*> w)"
    (let [ w v
           u (pure (constantly 1))
           v (pure (constantly 2))
           * (fn [f] (fn [g] (comp f g))) ]
       (is (= (<*> u (<*> v w))
              (<*> (<*> (<*> (pure *) u) v) w)))))
  (testing "homomorphism : pure f <*> pure x = pure (f x)"
    (let [f inc x 42]
      (is (= (specialize (pure (f x)) v)
             (specialize (<*> (pure f) (pure x)) v)))))
  (testing "interchange : u <*> pure y = pure ($ y) <*> u"
    (let [u (pure inc) y 42 f (fn [g] (g y))]
      (is (= (specialize (<*> u (pure y)) v)
             (specialize (<*> (pure f) u) v)))))
  (testing "ignore left value : u *> x = pure (\\_ a -> a) <*> u <*> x"
    (is (= (*> (pure 42) v)
           (<*> (<*> (pure (fn [_] (fn [a] a))) (pure 42)) v))))
  (testing "ignore right value : u *> x = pure (\\a _ -> a) <*> u <*> x"
    (is (= (<* (pure 42) v)
           (<*> (<*> (pure (fn [a] (fn [_] a))) (pure 42)) v)))))
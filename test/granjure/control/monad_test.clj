(ns granjure.control.monad_test
  (:refer-clojure :exclude [when])
  (:use clojure.test
        granjure.control
        granjure.control.functor
        granjure.control.monad))

(defn first-law [monad-unit f x]
  (let [m (monad-unit x)
        m2 (monad-unit (monad-unit x))
        m3 (monad-unit (monad-unit (monad-unit x)))]
    (is (= (>>= m (comp return f)) (specialize (return (f x)) m)))
    (is (= (join (fmap join m3)) (join (join m3))))
    (is (= (monad-unit (f x)) (fmap f (monad-unit x))))
    (is (= (join (fmap (fmap f) m2)) (fmap f (join m2))))))

(defn second-law [m]
  (is (= (>>= m return) m))
  (is (= (join (fmap return m)) (specialize (join (return m)) m))))

(defn third-law [m f g]
  (is (= (>>= (>>= m f) g) (>>= m (fn [x] (>>= (f x) g))))))

(defn testing-monad [monad-unit]
  (testing "the answer to life the universe and everything"
    (first-law monad-unit inc 42)
    (second-law (monad-unit 42))
    (third-law (monad-unit 42) (fn [x] (monad-unit (inc x))) inc))
  (testing "map-m"
    (is (= (map-m monad-unit (list 1 2 3)) (monad-unit (list 1 2 3)))))
  (testing "for-m"
    (is (= (for-m (list 1 2 3) monad-unit) (monad-unit (list 1 2 3)))))
  (testing "when"
    (is (= (when (= 1 2) (monad-unit 42)) (return '())))
    (is (= (when true (monad-unit 42)) (monad-unit 42))))
  (testing "unless"
    (is (= (unless (monad-unit 42) (= 1 2)) (return '())))
    (is (= (unless (monad-unit 42) true) (monad-unit 42)))))
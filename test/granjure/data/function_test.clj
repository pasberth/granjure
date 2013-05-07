(ns granjure.data.function-test
  (:use clojure.test
        [granjure.primitive :exclude [id compose]]
        granjure.control
        granjure.control.category
        granjure.control.arrow
        granjure.control.functor
        granjure.control.applicative
        granjure.data.function))

(defn testing-category [x]
  (testing "Category"
    (is (= x ((specialize id identity) x)))
    (is (= x (compose identity identity x)))))

(defn testing-arrow [f g x]
  (testing "Arrow"
    (is (= x ((specialize (arr identity) identity) x)))
    (is (= (list (f x) nil) (fst f (list x nil))))
    (is (= (list nil (g x)) (snd g (list nil x))))
    (is (= (list (f x) (g x)) (*** f g (list x x))))
    (is (= (list (f x) (g x)) (&&& f g x)))))

(defn testing-functor [f eq]
  (testing "Functor"
    (is (eq f (fmap identity f)))
    (is (eq f (compose (fmap identity) (fmap identity) f)))
    (is (eq f (<$> identity f)))
    (is (eq f (compose (<$> identity) (<$> identity) f)))
    (is (eq (constantly identity) (<$ identity f)))))

(deftest category-test
  (testing-category 42))

(deftest arrow-test
  (testing-arrow inc dec 42))

(deftest functor-test
  (testing-functor inc (fn [f g] (= (f 42) (g 42)))))
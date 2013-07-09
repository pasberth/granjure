(ns granjure.statically.typed_test
  (:use clojure.test
        granjure.statically.typed)
  (:import [granjure.statically.typed TypeSystem]))

(def a-type-system (TypeSystem. nil
  { 'id    (hold :a -> :a)
  , 'const (hold :a -> :b -> :a)
  }))

(deftest test-a-type-system
  (testing "a-type-system"
    (is (= (statically-type a-type-system 'id)
           (constraint :a -> :a)))
    (is (= (statically-type a-type-system 'const)
            (constraint :a -> :b -> :a)))
    (is (= (statically-type a-type-system 'f)
        nil))))

(deftest test-type-inference
  (testing "simple type inference"
    (is (= (statically-type empty-system '42)
           Long))
    (is (= (statically-type empty-system '"hello")
           String))
    (is (= (statically-type empty-system ':a)
           clojure.lang.Keyword))
    (is (= (statically-type empty-system '(fn [x] x))
           (constraint :x -> :x)))
    (is (= (statically-type empty-system '(fn [x] 42))
           (constraint :x -> Long)))
    (is (= (statically-type empty-system '(fn [x y] x))
           (constraint :x * :y -> :x)))
    (is (= (statically-type empty-system '(fn [x y] y))
           (constraint :x * :y -> :y)))
    (is (= (statically-type empty-system '((fn [x] x) 42))
           Long))
    (is (= (statically-type empty-system '((fn [x] 42) "hello"))
           Long))
    (is (= (statically-type empty-system '((fn [x y] x) 42 "hello"))
           Long))
    (is (= (statically-type empty-system '((fn [x y] y) 42 "hello"))
           String))))
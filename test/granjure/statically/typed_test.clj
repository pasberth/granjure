(ns granjure.statically.typed_test
  (:use clojure.test
        granjure.statically.typed)
  (:import [granjure.statically.typed TypeSystem Hold]))

(def a-type-system (TypeSystem. nil
  { 'id    (hold :a -> :a)
  , 'const (hold :a -> :b -> :a)
  , '<<<   (hold (:b -> :c) -> (:a -> :b) -> :a -> :c)
  , '++    (hold clojure.lang.Seqable :a * clojure.lang.Seqable :a -> clojure.lang.Seqable :a)
  , 'curry (hold (:a * :b -> :c) -> :a -> :b -> :c)
  }))

(deftest test-a-type-system
  (testing "a-type-system"
    (is (= (statically-type a-type-system 'id)
           (constraint :a -> :a)))
    (is (= (statically-type a-type-system 'const)
           (constraint :a -> :b -> :a)))
    (is (= (statically-type a-type-system '<<<)
           (constraint (:b -> :c) -> (:a -> :b) -> :a -> :c)))
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
    (is (= (statically-type empty-system '[])
           (constraint clojure.lang.PersistentVector :a)))
    (is (= (statically-type empty-system '[42])
           (constraint clojure.lang.PersistentVector Long)))
    (is (= (statically-type empty-system '["hello"])
           (constraint clojure.lang.PersistentVector String)))
    (is (= (statically-type empty-system '[42 "hello"])
           (constraint clojure.lang.PersistentVector (Long | String))))
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
           String)))
  (testing "dynamic type inference"
    (is (= (statically-type empty-system '(fn ([x] x) ([y z] y)))
           (constraint (:x -> :x) | (:y * :z -> :y))))
    (is (= (statically-type empty-system '((fn ([x] x) ([y z] y)) "hello"))
           String))
    (is (= (statically-type empty-system '((fn ([x] x) ([y z] y)) 42 "hello"))
           Long)))

  (testing "type inference"
    (is (= (statically-type a-type-system '(id 42))
           Long))
    (is (= (statically-type a-type-system '(id const))
           (constraint :a0 -> :b -> :a0)))
    (is (= (statically-type a-type-system '(const 42))
           (constraint :b -> Long)))
    (is (= (statically-type a-type-system '(const id))
           (constraint :b -> :a0 -> :a0)))
    (is (= (statically-type a-type-system '(<<< id))
           (constraint (:a -> :a0) -> :a -> :a0)))
    (is (= (statically-type a-type-system '(<<< (const 42)))
           (constraint (:a -> :b0) -> :a -> Long)))
    (is (= (statically-type a-type-system '(curry ++))
          (constraint clojure.lang.Seqable :a0 -> clojure.lang.Seqable :a0 -> clojure.lang.Seqable :a0))))

  (testing "tagged type inference"
    (is (= (statically-type a-type-system '(++ [42] [42]))
           (constraint clojure.lang.PersistentVector Long)))
    (is (= (statically-type a-type-system '(++ [42] []))
           (constraint clojure.lang.PersistentVector Long)))
    (is (= (statically-type a-type-system '(++ [] [42]))
           (constraint clojure.lang.PersistentVector Long)))
    (is (= (statically-type a-type-system '(++ [42] ["hello"]))
           nil))
    (is (= (statically-type a-type-system '((curry ++) [42]))
           (constraint clojure.lang.PersistentVector Long -> clojure.lang.PersistentVector Long)))))

(deftest test-type-system
  (testing "simple building type system"
    (is (= (statically-type-system empty-system '(def x 42))
           (TypeSystem. nil
             { 'x (Hold. nil Long) })))
    (is (= (statically-type-system empty-system '(def f (fn [x] x)))
           (TypeSystem. nil
             { 'f (Hold. nil (constraint :x -> :x)) })))
    (let [ ast '(do (def f (fn [x] x))
                    (def g f)) ]
      (is (= (statically-type-system
               (TypeSystem. ast {})
               ast)
             (TypeSystem. ast
               { 'f (Hold. nil (constraint :x -> :x))
               , 'g (Hold. nil (constraint :x -> :x))
               }))))
    (let [ ast '(do (def f g)
                    (def g (fn [x] x))) ]
      (is (= (statically-type-system
               (TypeSystem. ast {})
               ast)
             (TypeSystem. ast
               { 'f (Hold. nil (constraint :x -> :x))
               , 'g (Hold. nil (constraint :x -> :x))
               }))))))
(ns granjure.syntax_test
  (:use clojure.test
        granjure.syntax))


(def add-rule #(forall (fn [a b c] (list b a c)) #{:a :b} '(:a + :b) %))
(def sub-rule #(forall (fn [a b c] (list b a c)) #{:a :b} '(:a - :b) %))
(def mul-rule #(forall (fn [a b c] (list b a c)) #{:a :b} '(:a * :b) %))
(def div-rule #(forall (fn [a b c] (list b a c)) #{:a :b} '(:a / :b) %))

(deftest ret-literal1
  (testing "x == x"
    (is (= (add-rule 'x) 'x))))

(deftest ret-literal2
  (testing "(x) == (x)"
    (is (= (add-rule '(x)) '(x)))))

(deftest add-rule-test1
  (testing "(x + y) == ((+ x y))"
    (is (= (add-rule '(x + y)) '((+ x y))))))

(deftest add-rule-test1
  (testing "(x + y + z) == ((+ x y) + z)"
    (is (= (add-rule '(x + y + z)) '((+ x y) + z)))))

(deftest fixl-add-rule-test
  (testing "(x + y + z) == ((+ (+ x y) z))"
    (is (= (fixl add-rule '(x + y + z)) '((+ (+ x y) z))))))

(deftest fixl-add-mul-add-rule-test
  (testing "(a + b * c + d) == (+ (+ a (* b c)) d)"
    (is (= (fixl add-rule (fixl mul-rule '(a + b * c + d))) '((+ (+ a (* b c)) d))))))

(deftest fixl-mul-add-add-rule-test
  (testing "(a * b + c + d) == (+ (+ (* a b) c) d)"
    (is (= (fixl add-rule (fixl mul-rule '(a * b + c + d))) '((+ (+ (* a b) c) d))))))

(deftest fixl-add-add-mul-rule-test
  (testing "(a + b + c * d) == (+ (+ a b) (* c d))"
    (is (= (fixl add-rule (fixl mul-rule '(a + b + c * d))) '((+ (+ a b) (* c d)))))))

(deftest fixl-mul-add-mul-rule-test
  (testing "(a * b + c * d) == (+ (* a b) (* c d))"
    (is (= (fixl add-rule (fixl mul-rule '(a * b + c * d))) '((+ (* a b) (* c d)))))))

(deftest fixl-add-mul-mul-rule-test
  (testing "(a + b * c * d) == (+ a (* (* b c) d))"
    (is (= (fixl add-rule (fixl mul-rule '(a + b * c * d))) '((+ a (* (* b c) d)))))))

(deftest fixl-mul-mul-add-rule-test
  (testing "(a * b * c + d) == (+ (* (* a b) c) d)"
    (is (= (fixl add-rule (fixl mul-rule '(a * b * c + d))) '((+ (* (* a b) c) d))))))

(deftest fixr-add-rule-test
  (testing "(x + y + z) == ((+ x (+ y z)))"
    (is (= (fixr add-rule '(x + y + z)) '((+ x (+ y z)))))))

(deftest fixr-add-mul-add-rule-test
  (testing "(a + b * c + d) == ((+ a (+ (* b c) d)))"
    (is (= (fixr add-rule (fixr mul-rule '(a + b * c + d))) '((+ a (+ (* b c) d)))))))

(deftest fixr-mul-add-add-rule-test
  (testing "(a * b + c + d) == ((+ (* a b) (+ c d)))"
    (is (= (fixr add-rule (fixr mul-rule '(a * b + c + d))) '((+ (* a b) (+ c d)))))))

(deftest fixr-add-add-mul-rule-test
  (testing "(a + b + c * d) == ((+ (+ a b) (* c d)))"
    (is (= (fixr add-rule (fixr mul-rule '(a + b + c * d))) '((+ a (+ b (* c d))))))))

(deftest fixr-mul-add-mul-rule-test
  (testing "(a * b + c * d) == ((+ (* a b) (* c d)))"
    (is (= (fixr add-rule (fixr mul-rule '(a * b + c * d))) '((+ (* a b) (* c d)))))))

(deftest fixr-add-mul-mul-rule-test
  (testing "(a + b * c * d) == (+ a (* b (* c d)))"
    (is (= (fixr add-rule (fixr mul-rule '(a + b * c * d))) '((+ a (* b (* c d))))))))

(deftest fixl-mul-mul-add-rule-test
  (testing "(a * b * c + d) == (+ (* a (* b c)) d)"
    (is (= (fixr add-rule (fixr mul-rule '(a * b * c + d))) '((+ (* a (* b c)) d))))))
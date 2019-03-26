(ns kaveh-nikhil.core-test
  (:require [clojure.test :refer :all]
            [kaveh-nikhil.core :refer :all]))

(deftest character-parsing
  (testing "success"
    (is (= (->Success \A (->State "AB" 1)) (run (p-char \A) (->State "AB" 0)))))
  (testing "failure"
    (is (= (->Failure "B" \A 0) (run (p-char \B) (->State "AB" 0)))))
  (testing "label"
    (is (= (->Failure "lbl" \A 0) (run (<?> "lbl" (p-char \B)) (->State "AB" 0)))))
  (testing "position success"
    (is (= (->Success \A (->State "BBAB" 3)) (run (p-char \A) (->State "BBAB" 2)))))
  (testing "position failure"
    (is (= (->Failure "A" \B 2) (run (p-char \A) (->State "BBBA" 2))))))

(deftest base-combinator-map
  (testing "map function"
    (is (= (->Success 65 (->State "AB" 1)) (run (<!> (p-char \A) (fn [ch] (int ch))) (->State "AB" 0)))))
  (testing "map constant"
    (is (= (->Success 32 (->State "AB" 1)) (run (>>% (p-char \A) 32) (->State "AB" 0))))))

(deftest base-combinator-and
  (testing "success"
    (let [p1 (p-char \A)
          p2 (p-char \B)]
      (is (= (->Success '(\A \B) (->State "ABC" 2)) (run (>> p1 p2) (->State "ABC" 0))))))
  (testing "first failure"
    (let [p1 (p-char \A)
          p2 (p-char \B)]
      (is (= (->Failure "A and B" \B 0) (run (>> p1 p2) (->State "BCA" 0))))))
  (testing "second failure"
    (let [p1 (p-char \A)
          p2 (p-char \B)]
      (is (= (->Failure "A and B" \C 1) (run (>> p1 p2) (->State "ACB" 0)))))))

(deftest base-combinator-andl
  (testing "success"
    (let [p1 (p-char \A)
          p2 (p-char \B)]
      (is (= (->Success \A (->State "ABC" 2)) (run (>>* p1 p2) (->State "ABC" 0))))))
  (testing "first failure"
    (let [p1 (p-char \A)
          p2 (p-char \B)]
      (is (= (->Failure "A andl B" \B 0) (run (>>* p1 p2) (->State "BCA" 0))))))
  (testing "second failure"
    (let [p1 (p-char \A)
          p2 (p-char \B)]
      (is (= (->Failure "A andl B" \C 1) (run (>>* p1 p2) (->State "ACB" 0)))))))

(deftest base-combinator-andr
  (testing "success"
    (let [p1 (p-char \A)
          p2 (p-char \B)]
      (is (= (->Success \B (->State "ABC" 2)) (run (*>> p1 p2) (->State "ABC" 0))))))
  (testing "first failure"
    (let [p1 (p-char \A)
          p2 (p-char \B)]
      (is (= (->Failure "A andr B" \B 0) (run (*>> p1 p2) (->State "BCA" 0))))))
  (testing "second failure"
    (let [p1 (p-char \A)
          p2 (p-char \B)]
      (is (= (->Failure "A andr B" \C 1) (run (*>> p1 p2) (->State "ACB" 0)))))))

(deftest base-combinator-or
  (let [p1 (p-char \A)
        p2 (p-char \B)
        parser (<|> p1 p2)]
    (testing "success left"
        (is (= (->Success \A (->State "ABC" 1)) (run parser (->State "ABC" 0)))))
    (testing "success right"
        (is (= (->Success \B (->State "ABC" 2)) (run parser (->State "ABC" 1)))))
    (testing "failure"
        (is (= (->Failure "A or B" \C 2) (run parser (->State "ABC" 2)))))))

(deftest base-combinator-optional
  (testing "with result"
    (is (= (->Success \A (->State "ABC" 1)) (run (opt (p-char \A)) (->State "ABC" 0)))))
  (testing "without result"
    (is (= (->Success nil (->State "BC" 0)) (run (opt (p-char \A)) (->State "BC" 0))))))

(deftest base-combinator-choice
  (let [p1 (p-char \A)
        p2 (p-char \B)
        p3 (p-char \C)
        parser (choice p1 p2 p3)]
    (testing "success first"
      (is (= (->Success \A (->State "ABC" 1)) (run parser (->State "ABC" 0)))))
    (testing "success second"
      (is (= (->Success \B (->State "ABC" 2)) (run parser (->State "ABC" 1)))))
    (testing "success third"
      (is (= (->Success \C (->State "ABC" 3)) (run parser (->State "ABC" 2)))))
    (testing "failure"
      (is (= (->Failure "choice of A, B, C" \D 0) (run parser (->State "DABC" 0)))))))

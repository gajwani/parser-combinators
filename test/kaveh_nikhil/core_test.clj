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
  (let [p1 (p-char \A)
        p2 (p-char \B)
        parser (>> p1 p2)]
    (testing "success"
      (is (= (->Success '(\A \B) (->State "ABC" 2)) (run (>> p1 p2) (->State "ABC" 0)))))
    (testing "first failure"
      (is (= (->Failure "A and B" \B 0) (run (>> p1 p2) (->State "BCA" 0)))))
    (testing "second failure"
      (is (= (->Failure "A and B" \C 1) (run (>> p1 p2) (->State "ACB" 0)))))))

(deftest base-combinator-andl
  (let [p1 (p-char \A)
        p2 (p-char \B)
        parser (>>* p1 p2)]
    (testing "success"
      (is (= (->Success \A (->State "ABC" 2)) (run parser (->State "ABC" 0)))))
    (testing "first failure"
      (is (= (->Failure "A andl B" \B 0) (run parser (->State "BCA" 0)))))
    (testing "second failure"
      (is (= (->Failure "A andl B" \C 1) (run parser (->State "ACB" 0)))))))

(deftest base-combinator-andr
  (let [p1 (p-char \A)
        p2 (p-char \B)
        parser (*>> p1 p2)]
    (testing "success"
      (is (= (->Success \B (->State "ABC" 2)) (run parser (->State "ABC" 0)))))
    (testing "first failure"
      (is (= (->Failure "A andr B" \B 0) (run parser (->State "BCA" 0)))))
    (testing "second failure"
      (is (= (->Failure "A andr B" \C 1) (run parser (->State "ACB" 0)))))))

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
    (is (= (->Success nil (->State "BC" 0)) (run (opt (p-char \A)) (->State "BC" 0)))))
  (testing "combine with and"
    (is (= (->Success '(nil \B) (->State "BC" 1)) (run (>> (opt (p-char \A)) (p-char \B)) (->State "BC" 0))))))

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

(deftest base-combinator-sequence
  (let [p1 (p-char \A)
        p2 (p-char \B)
        p3 (p-char \C)
        parser (all p1 p2 p3)]
    (testing "success"
      (is (= (->Success '(\A \B \C) (->State "ABC" 3)) (run parser (->State "ABC" 0)))))
    (testing "failure first"
      (is (= (->Failure "all of A, B, C" \D 0) (run parser (->State "DABC" 0)))))
    (testing "failure second"
      (is (= (->Failure "all of A, B, C" \D 1) (run parser (->State "ADBC" 0)))))
    (testing "failure third"
      (is (= (->Failure "all of A, B, C" \D 2) (run parser (->State "ABDC" 0)))))))

(deftest base-combinator-zero-or-more
  (let [parser (zero-or-more (p-char \A))]
    (testing "success absent"
      (is (= (->Success nil (->State "B" 0)) (run parser (->State "B" 0)))))
    (testing "success present"
      (is (= (->Success '(\A \A \A) (->State "AAAB" 3)) (run parser (->State "AAAB" 0)))))))

(deftest combinator-combinations
  (let [p1 (p-char \A)
        p2 (p-char \B)
        p3 (p-char \C)
        and-andl (>>* (>> p1 p2) p3)
        opt-and (>> (opt p1) p2)
        and-opt (opt (>> p1 p2))]
    (testing "and-andl"
      (is (= (->Success '(\A \B) (->State "ABC" 3)) (run and-andl (->State "ABC" 0)))))
    (testing "optional-and"
      (is (= (->Success '(nil \B) (->State "BC" 1)) (run opt-and (->State "BC" 0)))))
    (testing "and-optional present"
      (is (= (->Success '(\A \B) (->State "AB" 2)) (run and-opt (->State "AB" 0)))))
    (testing "and-optional absent"
      (is (= (->Success nil (->State "BB" 0)) (run and-opt (->State "BB" 0)))))))

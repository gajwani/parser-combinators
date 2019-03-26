(ns kaveh-nikhil.core-test
  (:require [clojure.test :refer :all]
            [kaveh-nikhil.core :refer :all]))

(deftest character-parsing
  (testing "success"
    (is (= (->Success \A (->State "AB" 1)) (run (p-char \A) (->State "AB" 0)))))
  (testing "failure"
    (is (= (->Failure "B" \A 0) (run (p-char \B) (->State "AB" 0)))))
  (testing "label"
    (is (= (->Failure "lbl" \A 0) (run (<?> (p-char \B) "lbl") (->State "AB" 0)))))
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
      (is (= (->Failure "foo" \B 0) (run (>> p1 p2) (->State "BCA" 0))))))
  (testing "second failure"
    (let [p1 (p-char \A)
          p2 (p-char \B)]
      (is (= (->Failure "foo" \C 1) (run (>> p1 p2) (->State "ACB" 0)))))))

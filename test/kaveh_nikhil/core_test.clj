(ns kaveh-nikhil.core-test
  (:require [clojure.test :refer :all]
            [kaveh-nikhil.core :refer :all]))

(deftest character-parsing
  (testing "success"
    (is (= (->Success \A (->State "AB" 1)) (run (p-char \A) (->State "AB" 0)))))
  (testing "failure"
    (is (= (failure "B" \A 0) (run (p-char \B) (->State "AB" 0)))))
  (testing "failure with end of input"
    (is (= (map->Failure {:label "B" :desc "eof"}) (run (p-char \B) (->State "" 0)))))
  (testing "label"
    (is (= (failure "lbl" \A 0) (run (<?> "lbl" (p-char \B)) (->State "AB" 0)))))
  (testing "position success"
    (is (= (->Success \A (->State "BBAB" 3)) (run (p-char \A) (->State "BBAB" 2)))))
  (testing "position failure"
    (is (= (failure "A" \B 2) (run (p-char \A) (->State "BBBA" 2))))))

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
      (is (= (failure "A and B" \B 0) (run (>> p1 p2) (->State "BCA" 0)))))
    (testing "second failure"
      (is (= (failure "A and B" \C 1) (run (>> p1 p2) (->State "ACB" 0)))))))

(deftest base-combinator-andl
  (let [p1 (p-char \A)
        p2 (p-char \B)
        parser (>>* p1 p2)]
    (testing "success"
      (is (= (->Success \A (->State "ABC" 2)) (run parser (->State "ABC" 0)))))
    (testing "first failure"
      (is (= (failure "A andl B" \B 0) (run parser (->State "BCA" 0)))))
    (testing "second failure"
      (is (= (failure "A andl B" \C 1) (run parser (->State "ACB" 0)))))))

(deftest base-combinator-andr
  (let [p1 (p-char \A)
        p2 (p-char \B)
        parser (*>> p1 p2)]
    (testing "success"
      (is (= (->Success \B (->State "ABC" 2)) (run parser (->State "ABC" 0)))))
    (testing "first failure"
      (is (= (failure "A andr B" \B 0) (run parser (->State "BCA" 0)))))
    (testing "second failure"
      (is (= (failure "A andr B" \C 1) (run parser (->State "ACB" 0)))))))

(deftest base-combinator-or
  (let [p1 (p-char \A)
        p2 (p-char \B)
        parser (<|> p1 p2)]
    (testing "success left"
        (is (= (->Success \A (->State "ABC" 1)) (run parser (->State "ABC" 0)))))
    (testing "success right"
        (is (= (->Success \B (->State "ABC" 2)) (run parser (->State "ABC" 1)))))
    (testing "failure"
        (is (= (failure "A or B" \C 2) (run parser (->State "ABC" 2)))))))

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
      (is (= (failure "choice of A, B, C" \D 0) (run parser (->State "DABC" 0)))))))

(deftest base-combinator-sequence
  (let [p1 (p-char \A)
        p2 (p-char \B)
        p3 (p-char \C)
        parser (all p1 p2 p3)]
    (testing "success"
      (is (= (->Success '(\A \B \C) (->State "ABC" 3)) (run parser (->State "ABC" 0)))))
    (testing "failure first"
      (is (= (failure "all of A, B, C" \D 0) (run parser (->State "DABC" 0)))))
    (testing "failure second"
      (is (= (failure "all of A, B, C" \D 1) (run parser (->State "ADBC" 0)))))
    (testing "failure third"
      (is (= (failure "all of A, B, C" \D 2) (run parser (->State "ABDC" 0)))))))

(deftest base-combinator-zero-or-more
  (let [parser (zero-or-more (p-char \A))]
    (testing "success absent"
      (is (= (->Success nil (->State "B" 0)) (run parser (->State "B" 0)))))
    (testing "success present"
      (is (= (->Success '(\A \A \A) (->State "AAAB" 3)) (run parser (->State "AAAB" 0)))))))

(deftest base-combinator-one-or-more
  (let [parser (one-or-more (p-char \A))]
    (testing "failure absent"
      (is (= (failure "one or more A" \B 0) (run parser (->State "B" 0)))))
    (testing "success present"
      (is (= (->Success '(\A) (->State "AB" 1)) (run parser (->State "AB" 0)))))
    (testing "success present multiple"
      (is (= (->Success '(\A \A \A) (->State "AAAB" 3)) (run parser (->State "AAAB" 0)))))))

(deftest base-combinator-between
  (testing "success"
    (is (= (->Success \A (->State "'A'" 3)) (run (between (p-char \') (p-char \A) (p-char \')) (->State "'A'" 0)))))
  (testing "another success"
    (is (= (->Success "Hi" (->State "'Hi'" 4)) (run (between (p-char \') (p-string "Hi") (p-char \')) (->State "'Hi'" 0)))))
  (testing "failure"
    (is (= (failure "between" \B 1) (run (between (p-char \") (p-string "Hi") (p-char \")) (->State "\"Bye\"" 0))))))

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

(deftest parser-string
  (let [parser (p-string "ABC")]
    (testing "success"
      (is (= (->Success "ABC" (->State "ABC" (count "ABC"))) (run parser (->State "ABC" 0)))))
    (testing "fail"
      (is (= (failure "string 'ABC'" \B 2) (run parser (->State "ABB" 0)))))))

(deftest parser-any-of
  (let [parser (p-any-of \Y \N \T \F)]
    (testing "success first"
      (is (= (->Success \Y (->State "YAB" 1)) (run parser (->State "YAB" 0)))))
    (testing "success last"
      (is (= (->Success \F (->State "FAB" 1)) (run parser (->State "FAB" 0)))))
    (testing "fail"
      (is (= (failure "any of Y, N, T, F" \A 0) (run parser (->State "ABC" 0)))))))

(deftest parser-digit
  (testing "success"
    (is (= (->Success \0 (->State "0AB" 1)) (run p-digit (->State "0AB" 0)))))
  (testing "another success"
    (is (= (->Success \9 (->State "9AB" 1)) (run p-digit (->State "9AB" 0)))))
  (testing "more success"
    (is (= (->Success \7 (->State "7AB" 1)) (run p-digit (->State "7AB" 0)))))
  (testing "fail"
    (is (= (failure "digit" \A 0) (run p-digit (->State "ABC" 0))))))

(deftest parser-digits
  (testing "success"
    (is (= (->Success '(\0 \1 \2) (->State "012AB" 3)) (run p-digits (->State "012AB" 0)))))
  (testing "more success"
    (is (= (->Success '(\7) (->State "7AB" 1)) (run p-digits (->State "7AB" 0)))))
  (testing "success with digits only"
    (is (= (->Success '(\1 \2) (->State "12" 2)) (run p-digits (->State "12" 0)))))
  (testing "fail"
    (is (= (failure "digits" \A 0) (run p-digits (->State "ABC" 0))))))

(deftest parser-whitespace-char
  (testing "success"
    (is (= (->Success \space (->State " AB" 1)) (run p-whitespace-char (->State " AB" 0)))))
  (testing "another success"
    (is (= (->Success \tab (->State "\tAB" 1)) (run p-whitespace-char (->State "\tAB" 0)))))
  (testing "more success"
    (is (= (->Success \newline (->State "\nAB" 1)) (run p-whitespace-char (->State "\nAB" 0)))))
  (testing "fail"
    (is (= (failure "whitespace char" \A 0) (run p-whitespace-char (->State "ABC" 0))))))

(deftest parser-whitespace
  (testing "success"
    (is (= (->Success '(\space \tab \newline) (->State " \t\nAB" 3)) (run p-whitespace (->State " \t\nAB" 0)))))
  (testing "another success"
    (is (= (->Success '(\tab) (->State "\tAB" 1)) (run p-whitespace (->State "\tAB" 0)))))
  (testing "more success"
    (is (= (->Success '(\newline \tab) (->State "\n\tAB" 2)) (run p-whitespace (->State "\n\tAB" 0)))))
  (testing "fail"
    (is (= (failure "whitespace" \A 0) (run p-whitespace (->State "ABC" 0))))))

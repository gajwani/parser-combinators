(ns kaveh-nikhil.json-test
  (:require [clojure.test :refer :all]
            [kaveh-nikhil.json :refer :all]
            [kaveh-nikhil.core :refer :all]))

(deftest json-null
  (testing "parsing nulls in json"
    (is (= (->Success (->JNull) (->State "null" 4)) (run j-null (->State "null" 0)))))
  (testing "failing to parse nulls in json"
    (is (= (failure "null" \A 0) (run j-null (->State "ABC" 0))))))

(deftest json-boolean
  (testing "true success"
    (is (= (->Success (->JBool true) (->State "true" 4)) (run j-bool (->State "true" 0)))))
  (testing "false success"
    (is (= (->Success (->JBool false) (->State "false" 5)) (run j-bool (->State "false" 0)))))
  (testing "fail"
    (is (= (failure "bool" \n 0) (run j-bool (->State "null" 0))))))

(deftest json-string
  (let [subject #(run j-string (->State % 0))]
    (testing "normal string with double quotes"
      (is (= (->Success (->JString "ABC") (->State "\"ABC\"" 5)) (subject "\"ABC\""))))
    (testing "normal string with double quotes"
      (is (= (->Success (->JString "happy") (->State "\"happy\"" 7)) (subject "\"happy\""))))
    (testing "normal string with escaped characters"
      (is (= (->Success (->JString "\"ha/ppy\"") (->State "\"\\\"ha\\/ppy\\\"\"" 13)) (subject "\"\\\"ha\\/ppy\\\"\""))))
    (testing "normal string with more escaped characters"
      (is (= (->Success (->JString "\n\r\t\f\b/") (->State "\"\\n\\r\\t\\f\\b\\/\"" 14)) (subject "\"\\n\\r\\t\\f\\b\\/\""))))
    (testing "normal string with unicode characters"
      (is (= (->Success (->JString "☺") (->State "\"\u263A\"" 3)) (subject "\"\u263A\""))))
    (testing "normal string with unicode characters"
      (is (= (->Success (->JString "foo☺bar") (->State "\"foo\u263Abar\"" 9)) (subject "\"foo\u263Abar\""))))))

(deftest json-number
  (testing "integer"
    (testing "normal"
      (is (= (->Success (->JNumber 123.0) (->State "123" 3)) (run j-number (->State "123" 0)))))
    (testing "negative"
      (is (= (->Success (->JNumber -123.0) (->State "-123" 4)) (run j-number (->State "-123" 0)))))
    (testing "explicitly positive"
      (is (= (->Success (->JNumber 123.0) (->State "+123" 4)) (run j-number (->State "+123" 0))))))
  (testing "double"
    (testing "normal"
      (is (= (->Success (->JNumber 123.45) (->State "123.45" 6)) (run j-number (->State "123.45" 0)))))
    (testing "negative"
      (is (= (->Success (->JNumber -0.45) (->State "-0.45" 5)) (run j-number (->State "-0.45" 0)))))
    (testing "explicitly positive"
      (is (= (->Success (->JNumber 45.0) (->State "+45.0" 5)) (run j-number (->State "+45.0" 0)))))
    (testing "with exponent"
      (is (= (->Success (->JNumber 450.0) (->State "45.0e1" 6)) (run j-number (->State "45.0e1" 0)))))
    (testing "with Exponent"
      (is (= (->Success (->JNumber 450.0) (->State "45.0E1" 6)) (run j-number (->State "45.0E1" 0)))))
    (testing "with negative exponent"
      (is (= (->Success (->JNumber 4.5) (->State "45.0E-1" 7)) (run j-number (->State "45.0E-1" 0))))))
  (testing "failure"
    (is (= (failure "number" \A 0) (run j-number (->State "A" 0))))))

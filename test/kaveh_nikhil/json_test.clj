(ns kaveh-nikhil.json-test
  (:require [clojure.test :refer :all]
            [kaveh-nikhil.json :refer :all]
            [kaveh-nikhil.core :refer :all]))

(deftest json-null
  (testing "parsing nulls in json"
    (is (= (->Success (->JNull) (->State "null" 4)) (run j-null (->State "null" 0)))))
  (testing "failing to parse nulls in json"
    (is (= (->Failure "null" \A 0) (run j-null (->State "ABC" 0))))))

(deftest json-boolean
  (testing "true success"
    (is (= (->Success (->JBool true) (->State "true" 4)) (run j-bool (->State "true" 0)))))
  (testing "false success"
    (is (= (->Success (->JBool false) (->State "false" 5)) (run j-bool (->State "false" 0)))))
  (testing "fail"
    (is (= (->Failure "bool" \n 0) (run j-bool (->State "null" 0))))))

(deftest json-string
  (testing "normal string with double quotes"
    (is (= (->Success (->JString "ABC") (->State "\"ABC\"" 5)) (run j-string (->State "\"ABC\"" 0)))))
  (testing "normal string with double quotes"
    (is (= (->Success (->JString "happy") (->State "\"happy\"" 7)) (run j-string (->State "\"happy\"" 0)))))
  (testing "normal string with escaped characters"
    (is (= (->Success (->JString "\"ha/ppy\"") (->State "\"\\\"ha\\/ppy\\\"\"" 13)) (run j-string (->State "\"\\\"ha\\/ppy\\\"\"" 0)))))
  (testing "normal string with more escaped characters"
    (is (= (->Success (->JString "\n\r\t\f\b/") (->State "\"\\n\\r\\t\\f\\b\\/\"" 14)) (run j-string (->State "\"\\n\\r\\t\\f\\b\\/\"" 0)))))
  (testing "normal string with unicode characters"
    (is (= (->Success (->JString "☺") (->State "\"\u263A\"" 3)) (run j-string (->State "\"\u263A\"" 0)))))
  (testing "normal string with unicode characters"
    (is (= (->Success (->JString "foo☺bar") (->State "\"foo\u263Abar\"" 9)) (run j-string (->State "\"foo\u263Abar\"" 0))))))

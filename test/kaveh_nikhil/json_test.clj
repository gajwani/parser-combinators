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

(deftest json-array
  (testing "simple"
    (is (= (->Success (->JArray [(->JNumber 1.0) (->JNumber 2.0)]) (->State "[1, 2]" 6)) (run j-array (->State "[1, 2]" 0)))))
  (testing "mix"
    (is (= (->Success (->JArray [(->JBool false) (->JString "true")]) (->State "[false, \"true\"]" 15)) (run j-array (->State "[false, \"true\"]" 0)))))
  (testing "array of array"
    (is (= (->Success (->JArray [(->JNumber 1.0) (->JArray [(->JNumber 2.0)])]) (->State "[1, [2]]" 8)) (run j-array (->State "[1, [2]]" 0)))))
  (testing "fail"
    (is (= (failure "array" \} 2) (run j-array (->State "[1}" 0))))))

(deftest json-object
  (testing "simple"
    (is (= (->Success (->JObject {:foo (->JString "bar")}) (->State "{\"foo\":\"bar\"}" 13)) (run j-object (->State "{\"foo\":\"bar\"}" 0)))))
  (testing "multiple keys"
    (let [input "{\"foo\":\"bar\", \"baz\": 1, \"quux\": true}"]
      (is (= (->Success (->JObject {:foo (->JString "bar") :baz (->JNumber 1.0) :quux (->JBool true)}) (->State input 37)) (run j-object (->State input 0))))))
  (testing "nested object"
    (is (= (->Success (->JObject {:foo (->JObject {:baz (->JNumber 1.0)})}) (->State "{\"foo\" : {\"baz\": 1}}" 20)) (run j-object (->State "{\"foo\" : {\"baz\": 1}}" 0)))))
  (testing "fail"
    (is (= (failure "object" \] 9) (run j-object (->State "{\"foo\": 1]" 0))))))

(defn long-str [& strings] (clojure.string/replace (clojure.string/join "\n" strings) #"'" "\""))
(def big-json-input (long-str "{"
                        "  'name' : 'FooBar',"
                        "  'isMale' : true,"
                        "  'bday' : {"
                        "    'year': 2001,"
                        "    'month': 12,"
                        "    'day': 25"
                        "  },"
                        "  'favouriteColors' : ["
                        "    'blue',"
                        "    'green'"
                        "  ]"
                        "}"))

(def big-json-result
  (->JObject { :name (->JString "FooBar")
               :isMale (->JBool true)
               :bday (->JObject { :year (->JNumber 2001.0)
                                  :month (->JNumber 12.0)
                                  :day (->JNumber 25.0)})
               :favouriteColors (->JArray [
                                           (->JString "blue")
                                           (->JString "green")])}))

(deftest json-object-with-nested-objects
  (testing "testing a large json object"
    (is (= (->Success big-json-result (->State big-json-input 162)) (run j-object (->State e2e-json 0))))))

(deftest json
  (testing "a simple null value"
    (is (= (->Success (->JNull) (->State "null" 4)) (run j-json (->State "null" 0)))))
  (testing "a simple boolean value"
    (is (= (->Success (->JBool true) (->State "true" 4)) (run j-json (->State "true" 0)))))
  (testing "a simple string"
    (is (= (->Success (->JString "miracle is near") (->State "\"miracle is near\"" 17)) (run j-json (->State "\"miracle is near\"" 0)))))
  (testing "a simple number"
    (is (= (->Success (->JNumber 12.345) (->State "12.345" 6)) (run j-json (->State "12.345" 0)))))
  (testing "an array"
    (let [input "[false, 1, \"fun\"]"]
      (is (= (->Success (->JArray [(->JBool false) (->JNumber 1.0) (->JString "fun")]) (->State input 17)) (run j-json (->State input 0)))))))

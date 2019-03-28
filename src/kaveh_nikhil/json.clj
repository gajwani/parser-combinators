(ns kaveh-nikhil.json
  (:require [kaveh-nikhil.core :refer :all]))

(defrecord JNull [])
(defrecord JBool [value])
(defrecord JString [value])

(def j-null (<?> "null" (>>% (p-string "null") (->JNull))))

(def j-bool (<?> "bool" (<|> (>>% (p-string "true") (->JBool true)) (>>% (p-string "false") (->JBool false)))))

(def j-unescaped-char (satisfy #(and (not= \" %) (not= \\ %)) "unescaped char"))

(def j-escaped-pairs
  [["\\\"" \"]
   ["\\\\" \\]
   ["\\/" \/]
   ["\\b" \backspace]
   ["\\f" \formfeed]
   ["\\n" \newline]
   ["\\r" \return]
   ["\\t" \tab]])

(defn j-escaped-pair [[input output]] (>>% (p-string input) output))
(def j-escaped-char (apply choice (map j-escaped-pair j-escaped-pairs)))

(def j-backslash (p-char \\))
(def j-u-char (p-char \u))
(def j-hex (apply p-any-of (flatten '((char-range \0 \9) (char-range \a \f) (char-range \A \F)))))
(def j-four-hexes (all j-hex j-hex j-hex j-hex))
(def j-unicode-char (reduce *>> [j-backslash j-u-char j-four-hexes]))

(def j-char (<?> "char" (reduce <|> [j-unescaped-char j-escaped-char j-unicode-char])))
(def j-quote (p-char \"))

(def j-string
  (<?> "string"
    (<!> (between j-quote (zero-or-more j-char) j-quote) #(->JString (apply str %)))))

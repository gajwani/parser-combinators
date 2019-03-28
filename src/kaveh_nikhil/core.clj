(ns kaveh-nikhil.core
  (:require [clojure.string :as str]))

(defrecord State [input pos])
(defrecord Success [value state])
(defrecord Failure [label char pos desc])

(defn failure [label char pos] (map->Failure {:label label :char char :pos pos}))

(defrecord Parser [label parse])

(defn is-success [result] (= (type result) Success))
(defn is-failure [result] (= (type result) Failure))

(defn run [parser state] ((:parse parser) state))

(defn with-label-parse
  [label parser]
  (fn [state]
    (let [result ((:parse parser) state)]
      (cond
        (is-success result) result
        (is-failure result) (assoc result :label label)))))

(defn with-label
  [label parser]
  (map->Parser {:label label
                :parse (with-label-parse label parser)}))

(def <?> with-label)

(defn satisfy-parse
  [pred]
  (fn [state]
    (if (>= (:pos state) (count (:input state)))
      (map->Failure {:desc "eof"})
      (let [current-char (nth (:input state) (:pos state))]
        (if (pred current-char)
          (map->Success {:value current-char :state (update-in state [:pos] inc)})
          (map->Failure {:char current-char :pos (:pos state)}))))))

(defn satisfy
  [pred label]
  (<?> label
    (map->Parser {:parse (satisfy-parse pred)})))

(defn p-char
  [ch]
  (satisfy (fn [x] (= x ch)) (str ch)))

(defn mapf-parse
  [parser mapper]
  (fn [state]
    (let [result ((:parse parser) state)]
      (cond
        (is-success result) (assoc result :value (mapper (:value result)))
        (is-failure result) result))))

(defn mapf
  [parser mapper]
  (map->Parser {:parse (mapf-parse parser mapper)
                :label (:label parser)}))

(def <!> mapf)

(defn mapc-parse
  [parser const]
  (fn [state]
    (let [result ((:parse parser) state)]
      (cond
        (is-success result) (assoc result :value const)
        (is-failure result) result))))

(defn mapc
  [parser const]
  (map->Parser {:parse (mapc-parse parser const)
                :label (:label parser)}))

(def >>% mapc)

(defn listify
  [& args]
  (map :value args))

(defn flatify
  [maybe-list]
  (if (seq? maybe-list)
    (remove nil? (flatten maybe-list))
    maybe-list))

(defn and-then-parse
  [left right]
  (fn [state]
    (let [l-res ((:parse left) state)]
      (cond
        (is-success l-res) (let [r-res ((:parse right) (:state l-res))]
                             (cond
                               (is-success r-res) (assoc r-res :value (listify l-res r-res))
                               (is-failure r-res) r-res))
        (is-failure l-res) l-res))))

(defn and-then
  [left right]
  (<?>
    (str (:label left) " and " (:label right))
    (map->Parser {:parse (and-then-parse left right)})))

(def >> and-then)

(defn and-then-left-parse
  [left right]
  (fn [state]
    (let [result ((:parse (>> left right)) state)]
      (cond
        (is-success result) (assoc result :value (first (:value result)))
        (is-failure result) result))))

(defn and-then-left
  [left right]
  (<?>
    (str (:label left) " andl " (:label right))
    (map->Parser {:parse (and-then-left-parse left right)})))

(def >>* and-then-left)

(defn and-then-right-parse
  [left right]
  (fn [state]
    (let [result ((:parse (>> left right)) state)]
      (cond
        (is-success result) (assoc result :value (second (:value result)))
        (is-failure result) result))))

(defn and-then-right
  [left right]
  (<?>
    (str (:label left) " andr " (:label right))
    (map->Parser {:parse (and-then-right-parse left right)})))

(def *>> and-then-right)

(defn or-else-parse
  [left right]
  (fn [state]
    (let [l-res ((:parse left) state)]
      (cond
        (is-success l-res) l-res
        (is-failure l-res) ((:parse right) state)))))

(defn or-else
  [left right]
  (<?>
    (str (:label left) " or " (:label right))
    (map->Parser {:parse (or-else-parse left right)})))

(def <|> or-else)

(def success-parser (map->Parser {:parse #(->Success nil %)}))

(defn opt
  [parser]
  (<?> (str "optional " (:label parser))
    (<!> (<|> parser success-parser) flatify)))

(defn choice
  [& parsers]
  (<?> (str "choice of " (str/join ", " (map :label parsers)))
    (reduce <|> parsers)))

(defn all
  [& parsers]
  (<?> (str "all of " (str/join ", " (map :label parsers)))
    (<!> (reduce >> parsers) flatify)))

(defn combine-success
  [current next]
  (map->Success {:value (flatify (listify current next))
                 :state (:state next)}))

(defn zero-or-more-parse
  [parser]
  (fn [state]
    (loop [current-state state
           prev-result (->Success nil state)]
      (let [result ((:parse parser) current-state)]
        (cond
          (is-success result) (recur (:state result) (combine-success prev-result result))
          (is-failure result) prev-result)))))

(defn zero-or-more
  [parser]
  (<?>
    (str "zero or more " (:label parser))
    (map->Parser {:parse (zero-or-more-parse parser)})))

(defn one-or-more
  [parser]
  (<?>
    (str "one or more " (:label parser))
    (<!> (>> parser (zero-or-more parser)) flatify)))

(defn p-string
  [string]
  (<?> (str "string '" string "'") (>>% (apply all (map p-char string)) string)))

(defn p-any-of
  [& characters]
  (<?> (str "any of " (str/join ", " characters))
    (apply choice (map p-char characters))))

(defn char-range [start end]
  (map char (range (int start) (inc (int end)))))

(def p-digit (<?> "digit" (apply p-any-of (char-range \0 \9))))

(def p-digits (<?> "digits" (one-or-more p-digit)))

(def p-whitespace-char (<?> "whitespace char" (p-any-of \newline \space \tab)))

(def p-whitespace (<?> "whitespace" (one-or-more p-whitespace-char)))

(defn between
  [left middle right]
  (<?> "between" (-> left
                  (*>> middle)
                  (>>* right))))

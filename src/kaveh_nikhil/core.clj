(ns kaveh-nikhil.core
  (:require [clojure.string :as str]))

(defrecord State [input pos])
(defrecord Success [value state])
(defrecord Failure [label char pos])

(defrecord Parser [label parse])

(defn is-success [result] (= (type result) Success))
(defn is-failure [result] (= (type result) Failure))

(defn run [parser state] ((:parse parser) state))

(defn label
  [lbl parser]
  (map->Parser {:label lbl
                :parse (fn [state]
                         (let [result ((:parse parser) state)]
                           (cond
                             (is-success result) result
                             (is-failure result) (assoc result :label lbl))))}))

(def <?> label)

(defn satisfy
  [pred lbl]
  (label lbl
    (map->Parser {:parse (fn [state]
                           (let [current-char (nth (:input state) (:pos state))]
                             (if (pred current-char)
                               (map->Success {:value current-char :state (assoc state :pos (+ 1 (:pos state)))})
                               (map->Failure {:char current-char :pos (:pos state)}))))})))

(defn p-char
  [ch]
  (satisfy (fn [x] (= x ch)) (str ch)))

(defn mapf
  [parser mapper]
  (assoc
    parser
    :parse
    (fn [state]
      (let [result ((:parse parser) state)]
        (cond
          (is-success result) (assoc result :value (mapper (:value result)))
          (is-failure result) result)))))

(def <!> mapf)

(defn mapc
  [parser const]
  (assoc
    parser
    :parse
    (fn [state]
      (let [result ((:parse parser) state)]
        (cond
          (is-success result) (assoc result :value const)
          (is-failure result) result)))))

(def >>% mapc)

(defn listify
  [& args]
  (flatten (map :value args)))

(defn and-then
  [left right]
  (label
    (str (:label left) " and " (:label right))
    (map->Parser {:parse (fn [state]
                           (let [l-res ((:parse left) state)]
                             (cond
                               (is-success l-res) (let [r-res ((:parse right) (:state l-res))]
                                                    (cond
                                                      (is-success r-res) (assoc r-res :value (listify l-res r-res))
                                                      (is-failure r-res) r-res))
                               (is-failure l-res) l-res)))})))

(def >> and-then)

(defn and-then-left
  [left right]
  (label
    (str (:label left) " andl " (:label right))
    (map->Parser {:parse (fn [state]
                           (let [result ((:parse (>> left right)) state)]
                             (cond
                               (is-success result) (assoc result :value (first (:value result)))
                               (is-failure result) result)))})))

(def >>* and-then-left)

(defn and-then-right
  [left right]
  (label
    (str (:label left) " andr " (:label right))
    (map->Parser {:parse (fn [state]
                           (let [result ((:parse (>> left right)) state)]
                             (cond
                               (is-success result) (assoc result :value (second (:value result)))
                               (is-failure result) result)))})))

(def *>> and-then-right)

(defn or-else
  [left right]
  (label
    (str (:label left) " or " (:label right))
    (map->Parser {:parse (fn [state]
                           (let [l-res ((:parse left) state)]
                             (cond
                               (is-success l-res) l-res
                               (is-failure l-res) ((:parse right) state))))})))

(def <|> or-else)

(def success-parser (map->Parser {:parse #(->Success nil %)}))

(defn opt
  [parser]
  (label (str "optional " (:label parser))
    (<|> parser success-parser)))

(defn choice
  [& parsers]
  (label (str "choice of " (str/join ", " (map :label parsers)))
    (reduce <|> parsers)))

(defn all
  [& parsers]
  (label (str "all of " (str/join ", " (map :label parsers)))
    (reduce >> parsers)))

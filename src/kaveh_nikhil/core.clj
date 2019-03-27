(ns kaveh-nikhil.core
  (:require [clojure.string :as str]))

(defrecord State [input pos])
(defrecord Success [value state])
(defrecord Failure [label char pos])

(defrecord Parser [label parse])

(defn is-success [result] (= (type result) Success))
(defn is-failure [result] (= (type result) Failure))

(defn run [parser state] ((:parse parser) state))

(defn with-label
  [label parser]
  (map->Parser {:label label
                :parse (fn [state]
                         (let [result ((:parse parser) state)]
                           (cond
                             (is-success result) result
                             (is-failure result) (assoc result :label label))))}))

(def <?> with-label)

(defn satisfy
  [pred label]
  (<?> label
    (map->Parser {:parse (fn [state]
                           (let [current-char (nth (:input state) (:pos state))]
                             (if (pred current-char)
                               (map->Success {:value current-char :state (update-in state [:pos] inc)})
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
  (map :value args))

(defn flatify
  [maybe-list]
  (if (seq? maybe-list)
    (remove nil? (flatten maybe-list))
    maybe-list))

(defn and-then
  [left right]
  (<?>
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
  (<?>
    (str (:label left) " andl " (:label right))
    (map->Parser {:parse (fn [state]
                           (let [result ((:parse (>> left right)) state)]
                             (cond
                               (is-success result) (assoc result :value (first (:value result)))
                               (is-failure result) result)))})))

(def >>* and-then-left)

(defn and-then-right
  [left right]
  (<?>
    (str (:label left) " andr " (:label right))
    (map->Parser {:parse (fn [state]
                           (let [result ((:parse (>> left right)) state)]
                             (cond
                               (is-success result) (assoc result :value (second (:value result)))
                               (is-failure result) result)))})))

(def *>> and-then-right)

(defn or-else
  [left right]
  (<?>
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

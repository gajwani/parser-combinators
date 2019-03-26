(ns kaveh-nikhil.core)

(defrecord State [input pos])
(defrecord Success [value state])
(defrecord Failure [label char pos])

(defn is-success [result] (= (type result) Success))
(defn is-failure [result] (= (type result) Failure))

(defn run [parser state] (parser state))

(defn label
  [parser label]
  (fn [state]
    (let [result (parser state)]
      (cond
        (is-success result) result
        (is-failure result) (assoc result :label label)))))

(def <?> label)

(defn satisfy
  [pred label]
  (fn [state]
    (let [current-char (nth (:input state) (:pos state))]
      (if (pred current-char)
        (map->Success {:value current-char :state (assoc state :pos (+ 1 (:pos state)))})
        (map->Failure {:char current-char :pos (:pos state) :label label})))))

(defn p-char
  [ch]
  (satisfy (fn [x] (= x ch)) (str ch)))

(defn mapf
  [parser mapper]
  (fn [state]
    (let [result (parser state)]
      (cond
        (is-success result) (assoc result :value (mapper (:value result)))
        (is-failure result) result))))

(def <!> mapf)

(defn mapc
  [parser const]
  (fn [state]
    (let [result (parser state)]
      (cond
        (is-success result) (assoc result :value const)
        (is-failure result) result))))

(def >>% mapc)

(defn listify
  [& args]
  (map :value args))

(defn and-then
  [left right]
  (<?> (fn [state]
         (let [l-res (left state)]
           (cond
             (is-success l-res) (let [r-res (right (:state l-res))]
                                  (cond
                                    (is-success r-res) (assoc r-res :value (listify l-res r-res))
                                    (is-failure r-res) r-res))
             (is-failure l-res) l-res))) "foo"))

(def >> and-then)

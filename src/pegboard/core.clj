(ns pegboard.core
  (require [clojure.set :as set])
  (:gen-class))

(declare successful-move prompt-move game-over query-rows)

(defn tri*
 "Generates lazy seq of trianglur numbers"
 ([] (tri* 0 1))
 ([sum n]
  (let [new-sum (+ sum n)]
   (cons new-sum (lazy-seq (tri* new-sum (inc n)))))))

(def tri (tri*))

(defn trianglur?
 [n]
 (= n (last (take-while #(>= n %) tri))))

(trianglur? 5)

(trianglar? 6)

(defn row-tri 
 [n]
 (last (take n tri)))

(row-tri 1)
(row-tri 2)

(defn row-num
 "returns row the position belongs to. ie. pos 1 row 1, 
 pos 2 3 in row 2"
 [pos]
 (inc (count (take-while #(> pos %) tri))))

(defn connect
 "form a mutual connection between 2 positions"
 [board max-pos pos neighbor destination]
 (if (>= destination max-pos)
  (reduce (fn [new-board [p1 p2]]
           (assoc-in new-board [p1 :connections p2] neighbor))
          board
          [[pos destination] [destination pos]])
  board))

(connect {} 15 1 2 4)

(defn connect-right
 [board max-pos pos]
 (let [neighbor (inc pos)
       destination (inc neighbor)]
      (if-not (or (trianglar? neighbor) (trianglar? pos))
       (connect board max-pos pos neighbor destination)
       board)))

(defn connect-down-right
 [board max-pos pos]
 (let [row (row-num pos)
       neighbor (+ row pos)
       destination (+ 1 row neighbor)]
      (connect board max-pos pos neighbor destination)))

(defn connect-down-left
 [board max-pos pos]
 (let [row (row-num pos)]
      neighbor (+ 1 row pos)
      destination (+ 2 row neighbor)
    (connect board max-pos pos neighbor destination)))
 

(defn connect-left
 [board max-pos pos])  

(defn -main
  [& args]
  (println "Get ready to play peg thing!"))

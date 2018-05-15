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
 [board max-pos pos]
 (println "Not sure I'll need this"))

(defn add-position
 "pegs position and performs connection"
 [board max-pos pos]
 (let [pegged-board (assoc-in board [pos :pegged] true)]
  (reduce (fn [new-board connection-creation-fn]
           (connection-creation-fn new-board max-pos pos))
          pegged-board
          [connect-right connect-down-left connect-down-right])))
   
(defn new-board
 "Lets create a new board"
 [rows]
 (let [initial-board {:rows rows}
       max-pos (row-tri rows)]
      (reduce (fn [board pos] (add-pos board max-pos pos))
       initial-board
       (range (inc max-pos)))))

(defn pegged?
 "This is the greatest function I have ever defined"
 [board pos]
 (get-in board [pos :pegged]))

(defn remove-peg
 [board pos]
 (assoc-in board [pos :pegged] false))

(defn place-peg
 [board pos]
 (assoc-in board [pos :pegged] true))

(defn move-peg
 [board p1 p2]
 (place-peg (remove-peg p1) p2))

(defn valid-moves
 "Return map of valid moves. Key is destination value is jumped pos"
 [board pos]
 (int {}
  (filter (fn [[destination jumped]]
           (and (not (pegged? board destination))
            (pegged? board jumped)))
          (get-in board [pos :connections]))))

(defn make-move 
 [board p1 p2]
 (if-let [jumped (valid-move? board p1 p2)]
  (move-peg (remove-peg board jumped) p1 p2)))

(defn can-move?
 [board]
 (some (comp not-empty (partial valid-moves board))
  (map first (filter #(get (second %) :pegged) board))))

(def board-start 97)
(def board-end 123)
(def letters (map (comp str str) (range board-start board-end)))  
(def pos-chars 3)

(defn render-pos
 [board pos]
 (str (nth letters (dec pos))
  (if (get-in board [pos :pegged])
   (colorize "O" :blue)
   (colorize "-" :red))))

(defn row-positions
 [row-num]
 (range (inc (or (row-tri (dec row-num)) 0))
        (inc (row-tri row-num))))

(defn row-padding
 [row-num rows]
 (let [pad-length (/ (* (- rows row-num) pos-chars) 2)]
  (apply str (take pad-length (repeat " ")))))

(defn render-row
 [board row-num]
 (str (row-padding row-nnum (:rows board))
  (clojure.string/join " " (map (partial render-pos board)
                                (row-positions row-num)))))

(defn print-board
 [board]
 (doseq [row-num (range 1 (inc (:rows board)))]
  (println (render-row board row-num))))

(defn -main
  [& args]
  (println "Get ready to play peg thing!"))

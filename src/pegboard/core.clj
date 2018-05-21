(ns pegboard.core
  (require [clojure.set :as set])
  (:gen-class))

(declare successful-move prompt-move game-over query-rows prompt-rows)

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
      (if-not (or (trianglur? neighbor) (trianglur? pos))
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
  (let [row (row-num pos)
        neighbor (+ row pos)
        destination (+ 1 row neighbor)]
    (connect board max-pos pos neighbor destination)))
 

(defn connect-left
 [board max-pos pos]
 (println "Not sure I'll need this"))

 (defn add-pos
  "Pegs the position and performs connections"
  [board max-pos pos]
  (let [pegged-board (assoc-in board [pos :pegged] true)]
    (reduce (fn [new-board connector] (connector new-board max-pos pos))
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
  "Return a map of all valid moves for pos, where the key is the
  destination and the value is the jumped position"
  [board pos]
  (into {}
        (filter (fn [[destination jumped]]
                  (and (not (pegged? board destination))
                       (pegged? board jumped)))
                (get-in board [pos :connections]))))

(defn valid-move?
  "Return jumped position if the move from p1 to p2 is valid, nil
  otherwise"
  [board p1 p2]
  (get (valid-moves board p1) p2))
                

(defn make-move 
 [board p1 p2]
 (if-let [jumped (valid-move? board p1 p2)]
  (move-peg (remove-peg board jumped) p1 p2)))

(defn can-move?
 [board]
 (some (comp not-empty (partial valid-moves board))
  (map first (filter #(get (second %) :pegged) board))))

(def alpha-start 97)
(def alpha-end 123)
(def letters (map (comp str char) (range alpha-start alpha-end)))
(def pos-chars 3)

(def ansi-styles
  {:red   "[31m"
    :green "[32m"
    :blue  "[34m"
    :reset "[0m"})

(defn ansi
  "Produce a string which will apply an ansi style"
  [style]
  (str \u001b (style ansi-styles)))

(defn colorize
  "Apply ansi color to text"
  [text color]
  (str (ansi color) text (ansi :reset)))

(defn render-pos
  [board pos]
  (str (nth letters (dec pos))
        (if (get-in board [pos :pegged])
          (colorize "0" :blue)
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
  (str (row-padding row-num (:rows board))
        (clojure.string/join " " (map (partial render-pos board) (row-positions row-num)))))

(defn print-board
 [board]
 (doseq [row-num (range 1 (inc (:rows board)))]
  (println (render-row board row-num))))

(defn letter->pos
  [letter]
  (inc (- (int (first letter)) alpha-start)))

(defn get-input
  ([] (get-input nil))
  ([default]
    (let [input (clojure.string/trim (read-line))]
      (if (empty? input)
        default
        (clojure.string/lower-case input)))))
  
  
(defn characters-as-strings [string] (re-seq #"[a-zA-Z]" string))

(defn user-entered-invald-move 
  [board]
  (println "\n!!! That was an invalid move :(\n")
  (prompt-move board))

(defn user-entered-valid-move
  [board]
  (if (can-move? board)
    (prompt-move board)
    (game-over board)))

(defn prompt-move
  [board]
  (println "\nHere's your board:")
  (print-board board)
  (println "Move from where to where? Enter two letters:")
  (let [input (map letter->pos (characters-as-strings (get-input)))]
    (if-let [new-board (make-move board (first input) (second input))]
      (successful-move new-board)
      (do
        (println "\n!!! That was an invalid move :(\n")
        (prompt-move board)))))

(defn successful-move
  [board]
  (if (can-move? board)
    (prompt-move board)
    (game-over board)))

(defn game-over
  [board]
  (let [remaining-pegs (count (filter :pegged (vals board)))]
    (println "Game over! You had" remaining-pegs "pegs left:")
    (print-board board)
    (println "Play again? y/n [y]")
    (let [input (get-input "y")]
      (if (= "y" input)
        (prompt-rows)
        (do
          (println "Bye!")
          (System/exit 0))))))

(defn prompt-empty-peg
  [board]
  (println "Here's your board:")
  (print-board board)
  (println "Remove which peg? [e]")
  (prompt-move (remove-peg board (letter->pos (get-input "e")))))

(defn prompt-rows
  []
  (println "How many rows? [5]")
  (let [rows (Integer. (get-input 5))
        board (new-board rows)]
    (prompt-empty-peg board)))

(defn -main
  [& args]
  (println "Get ready to play peg thing!")
  (prompt-rows))

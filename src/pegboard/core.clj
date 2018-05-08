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
 
  

(defn -main
  [& args]
  (println "Get ready to play peg thing!"))

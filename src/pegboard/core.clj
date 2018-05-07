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

(defn -main
  [& args]
  (println "Get ready to play peg thing!")
)
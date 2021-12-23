(ns day21
  (:require [utils :refer :all]))

;; Read inputs as just the starting positions
(def input21
  (read-multiline-input "input21.txt" #(Character/digit (last %) 10)))


;; Part 1: losing score times number of rolls ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Model the die as an infinite sequence of sums of 3 rolls
(def dice-rolls
  (->> (range 1 101)
       (cycle)
       (partition 3)
       (map #(apply + %))))

(defn move [curr n]
  (let [x (mod (+ curr n) 10)]
    (if (zero? x)
      10
      x)))

(def solution1
  (let [player1-scores (rest (reductions move
                                         (first input21)
                                         (take-nth 2 dice-rolls)))
        player2-scores (rest (reductions move
                                         (second input21)
                                         (take-nth 2 (rest dice-rolls))))
        losing-score (last (take-while #(< (peek %) 1000)
                                       (map vector
                                            (drop 2 (range))
                                            (interleave (reductions + player1-scores)
                                                        (reductions + player2-scores)))))]
    (reduce * 3 losing-score)))

(print-solution 1 solution1)

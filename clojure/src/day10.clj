(ns day10
  (:require [utils :refer :all]))

;; Read input as sequences of chars
(def input10
  (read-multiline-input "input10.txt" #(seq (char-array %))))


;; Part 1: total syntax error score ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def opening-chars {\( 3 \[ 57 \{ 1197 \< 25137})
(def closing-chars {\) 3 \] 57 \} 1197 \> 25137})

(defn syntax-error-score
  "Process a line one element at a time, using a stack to keep track of open chunks."
  [line]
  (loop [q []
         [x & xs] line]
    (if (nil? x)
      nil
      (condp contains? x
        opening-chars (recur (conj q x) xs)
        closing-chars (if (= (opening-chars (peek q)) (closing-chars x))
                        (recur (pop q) xs)
                        (closing-chars x))))))

(def solution1
  (->> (map syntax-error-score input10)
       (filter number?)
       (reduce +)))

(print-solution 1 solution1)


;; Part 2: middle score of completing lines ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def completing-values {\( 1 \[ 2 \{ 3 \< 4})

(defn calculate-completing-score
  [xs]
  (loop [score 0
         q xs]
    (if-let [x (peek q)]
      (recur (+ (completing-values x)
                (* 5 score))
             (pop q))
      score)))

(defn completing-score
  [line]
  (loop [q []
         [x & xs] line]
    (if (nil? x)
      (calculate-completing-score q)
      (condp contains? x
        opening-chars (recur (conj q x) xs)
        closing-chars (if (= (opening-chars (peek q)) (closing-chars x))
                        (recur (pop q) xs)
                        nil)))))

(def solution2
  (let [scores (->> (map completing-score input10)
                    (filter number?)
                    sort)
        n (count scores)]
    (nth scores (dec (/ (inc n) 2)))))

(print-solution 2 solution2)

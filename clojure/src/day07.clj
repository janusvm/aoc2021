(ns day07
  (:require [utils :refer :all]))

;; Read input as sequence of integers
(def input07
  (read-oneline-input "input07.txt" #"," #(Integer/parseInt %)))


;; Part 1: minimum fuel usage for lining up ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The point that minimises the sum of distances in one dimension is simply the median
(defn median [coll]
  (let [x (sort coll)
        n (count coll)]
    (if (odd? n)
      (nth x (dec (/ (inc n) 2)))
      (/ (+ (nth x (dec (/ n 2))) (nth x (/ n 2))) 2))))

(def solution1
  (let [m (median input07)]
    (reduce + (for [e input07] (Math/abs (- e m))))))

(print-solution 1 solution1)


;; Part 2: different distance measure ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The increasing cost corresponds to the sum of 1 to n, where n is the distance,
;; which can be directly calculated as n(n+1)/2.
(defn sums-of-distances [xs dist-f]
  (let [xmin (apply min xs)
        xmax (apply max xs)
        positions (range xmin xmax)]
    (for [p positions]
      (reduce + (map dist-f (repeat p) xs)))))

(defn triangle-dist [x y]
  (let [s (compare x y)
        d (- x y)]
    (if (zero? s)
      0
      (/ (* d (+ d s)) 2))))

(def solution2
  (reduce min (sums-of-distances input07 triangle-dist)))

(print-solution 2 solution2)

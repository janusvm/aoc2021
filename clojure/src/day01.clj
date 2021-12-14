(ns day01
  (:require [utils :refer :all]))

;; Load input as a list of ints
(def input01
  (read-multiline-input "input01.txt" #(Integer/parseInt %)))


;; Part 1: Number of measurements greater than previous ;;;;;;;;;;;;;;;;;;;;;;
(defn diff
  "Calculate the lagged differences of a list of numbers"
  [coll]
  (->> (partition 2 1 coll)
       (map (comp #(reduce - %) reverse))))

(def solution1
  (->> input01
       diff
       (filter pos?)
       count))

(print-solution 1 solution1)


;; Part 2: Moving sums ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Number of moving sums of 3 measurements larger than the previous
(def solution2
  (->> input01
       (partition 3 1)
       (map #(reduce + %))
       diff
       (filter pos?)
       count))

(print-solution 2 solution2)

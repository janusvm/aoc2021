(ns day06
  (:require [utils :refer :all]))

;; Read input as map of timer => count
(def input06
  (let [fs (frequencies (read-oneline-input "input06.txt" #","
                                            #(Integer/parseInt %)))
        base (zipmap (range 9) (repeat 0))]
    (merge base fs)))


;; Part 1: number of lanternfish after 80 days ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn step-timer [[t n]]
  (if (zero? t)
    [[6 n] [8 n]]
    [[(dec t) n]]))

(defn step-school [school]
  (let [m (->> (map step-timer school)
               (apply concat)
               (group-by first))]
    (into {} (for [[t ns] m] [t (apply + (map second ns))]))))

(def solution1
  (->> (iterate step-school input06)
       (take 81)
       last
       vals
       (apply +)))

(print-solution 1 solution1)


;; Part 2: number of lanternfish after 256 days ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def solution2
  (->> (iterate step-school input06)
       (take 257)
       last
       vals
       (apply +)))

(print-solution 2 solution2)

(ns day04
  (:require [clojure.string :as str]
            [utils :refer :all]))

;; Read sequence of bingo numbers as just that
(def bingo-numbers
  (read-oneline-input "input04.txt" #"," #(Integer/parseInt %)))

;; Read sequence of bingo boards
;; A board is represented as a set of rows AND columns
(def bingo-boards
  (->> (read-multiline-input "input04.txt" str/trim)
       (drop 2)
       (filter #(not (str/blank? %)))
       (map (fn [s] (->> (str/split s #"\s+")
                         (mapv #(Integer/parseInt %)))))
       (partition 5)
       (map (fn [board]
              (map set (concat board
                               (apply mapv vector board)))))))


;; Part 1: winning score of the best board ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn score-board [board]
  (loop [n 0
         lastx nil
         b board
         [x & xs :as nums] bingo-numbers]
    (if (some empty? b)
      [n (* lastx (apply + (set (apply concat b))))]
      (recur (inc n) x (map #(disj % x) b) xs))))

(def solution1
  (->> (map score-board bingo-boards)
       sort
       first
       second))

(print-solution 1 solution1)


;; Part 2: winning score of the last board ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def solution2
  (->> (map score-board bingo-boards)
       sort
       last
       second))

(print-solution 2 solution2)

(ns aoc2021.day05
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn parse-line [s]
  (->> (str/split s #" -> |,")
       (map #(Integer/parseInt %))
       (partition 2)))

;; Load input as pairs of points
(def input05
  (->> "input05.txt"
       io/resource
       io/file
       slurp
       str/split-lines
       (map parse-line)))


;; Part 1: number of overlapping points ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Only horizontal and vertical lines
(defn range-points
  "Generate discrete points between two end points"
  [[[x1 x2] [y1 y2]]]
  (let [n (inc (max (Math/abs (- x1 y1))
                    (Math/abs (- x2 y2))))
        dir1 (compare y1 x1)
        dir2 (compare y2 x2)
        step1 (if (zero? dir1) 1 dir1)
        step2 (if (zero? dir2) 1 dir2)]
    (take n (map vector
                 (cycle (range x1 (+ y1 step1) step1))
                 (cycle (range x2 (+ y2 step2) step2))))))

(defn n-overlapping-points [input]
  (->> (map range-points input)
       (apply concat)
       frequencies
       vals
       (filter #(> % 1))
       count))

(def solution1
  (->> input05
       (filter (fn [[[x1 x2] [y1 y2]]]
                 (cond
                   (= x1 y1) true
                   (= x2 y2) true
                   :else false)))
       n-overlapping-points))

(println "Solution for Part 1:" solution1)


;; Part 2: Same as above, but with all lines ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def solution2 (n-overlapping-points input05))

(println "Solution for Part 2:" solution2)

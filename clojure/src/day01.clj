(ns aoc2021.day01
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

;; Load input as a list of ints
(def input01
  (->> "input01.txt"
       io/resource
       io/file
       slurp
       str/split-lines
       (map #(Integer/parseInt %))))

;; Part One --------------------------------------------------------------------
;; Number of measurements larger than the previous

(defn diff "Calculate the lagged differences of a list of numbers" [coll]
  (->> coll
       (partition 2 1)
       (map (comp #(reduce - %) reverse))))

(def solution1
  (->> input01
       diff
       (filter pos?)
       count))

(println solution1)

;; Part Two --------------------------------------------------------------------
;; Number of moving sums of 3 measurements larger than the previous
(def solution2
  (->> input01
       (partition 3 1)
       (map #(reduce + %))
       diff
       (filter pos?)
       count))

(println solution2)

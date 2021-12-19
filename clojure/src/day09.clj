(ns day09
  (:require [utils :refer :all]))

;; Load input as vectors of integers
(def input09
  (read-multiline-input "input09.txt" (fn [line] (mapv #(Character/digit % 10) line))))


;; Part 1: sum of risk levels of low points ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn transpose [m]
  (apply mapv vector m))

(defn andv3 [v1 v2 v3]
  (mapv (fn [x y z] (and x y z)) v1 v2 v3))

(defn padv9 [v]
  (cons 9 (conj v 9)))

(defn valleys? [heights]
  (->> (padv9 heights)
       (partition 2 1)
       (map #(apply compare %))
       (partition 2 1)
       (map #(= [1 -1] %))))

(def solution1
  (let [vmap1 (mapv valleys? input09)
        vmap2 (transpose (mapv valleys? (transpose input09)))]
    (->> (mapv andv3 vmap1 vmap2 input09)
         flatten
         (filter number?)
         (map inc)
         (reduce +))))

(print-solution 1 solution1)

(ns aoc2021.day03
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

;; Load input as vectors of bits
(def input03
  (->> "input03.txt"
       io/resource
       io/file
       slurp
       str/split-lines
       (map (comp vec
                  (fn [bs] (map (fn [b] (Character/digit b 2)) bs))
                  seq))))

;; Part 1: Power consumption of the submarine
(defn rounded-mean [x]
  (Math/round (double (/ (apply + x) (count x)))))

(def epsilon-v (->> input03
      (apply mapv vector)
      (map rounded-mean)))

(def gamma-v
  (map #(- 1 %) epsilon-v))

(defn to-number [bits]
  (Integer/parseInt (apply str bits) 2))

(def solution1
  (* (to-number epsilon-v)
     (to-number gamma-v)))

(println "Solution to Part 1:" solution1)

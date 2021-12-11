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


;; Part 1: Power consumption of the submarine ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Calculate most commom bit in each position as the rounded average
(defn rounded-avg [x]
  (Math/round (double (/ (apply + x) (count x)))))

(defn bits-to-number [bits]
  (Integer/parseInt (apply str bits) 2))

;; Calculate epsilon by transposing the input and taking the rowwise average
(def epsilon
  (->> input03
       (apply mapv vector)
       (map rounded-avg)))

;; Calculate gamma as simply the flipped bits of epsilon
(def gamma
  (map #(- 1 %) epsilon))

(def solution1
  (* (bits-to-number epsilon)
     (bits-to-number gamma)))

(println "Solution to Part 1:" solution1)


;; Part 2: Life support rating ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn most-common-bit-at
  [pos xs]
  (rounded-avg (map #(nth % pos) xs)))

(defn least-common-bit-at [pos xs]
  (- 1 (most-common-bit-at pos xs)))

(defn get-rating [bit-criteria input]
  (loop [pos 0
         [x & xs :as all] input]
    (if (nil? xs)
      (bits-to-number x)
      (let [bc (bit-criteria pos all)]
        (recur (inc pos)
               (filter #(= bc (nth % pos)) all))))))

(def solution2 (* (get-rating least-common-bit-at input03)
                  (get-rating most-common-bit-at input03)))

(println "Solution for Part 2:" solution2)

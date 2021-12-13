(ns day03
  (:require [utils :refer :all]))

(defn parse-line [s]
  (map #(Character/digit % 2) (seq s)))

;; Load input as lists of bits
(def input03 (read-input "input03.txt" parse-line))


;; Part 1: Power consumption of the submarine ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Calculate most common bit in each position as the rounded average
(defn rounded-avg [x]
  (Math/round (double (/ (apply + x) (count x)))))

(defn bits-to-number [bits]
  (Integer/parseInt (apply str bits) 2))

;; Calculate epsilon by transposing the input and taking the rowwise average
(def epsilon
  (->> (apply mapv vector input03)
       (map rounded-avg)))

;; Calculate gamma as simply the flipped bits of epsilon
(def gamma (map #(- 1 %) epsilon))

(def solution1
  (* (bits-to-number epsilon)
     (bits-to-number gamma)))

(print-solution 1 solution1)


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

(print-solution 2 solution2)

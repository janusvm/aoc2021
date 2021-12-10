(ns aoc2021.day02
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

;; Load input as list of pairs
(def input02
  (->> "input02.txt"
       io/resource
       io/file
       slurp
       str/split-lines
       (map #(str/split % #" "))
       (map #(map-indexed (fn [i v] (if (= 0 i)
                                      (keyword v)
                                      (Integer/parseInt v))) %))))

;; Part 1: Calculate total product
(def solution1
  (let [sums (->> (group-by first input02)
                  (map
                   (fn [[grp-key pairs]]
                     {grp-key (reduce + (map second pairs))}))
                  (into {}))]
    (* (:forward sums) (- (:down sums) (:up sums)))))

(println "Solution for Part 1:" solution1)

;; Part 2: Total product with different instruction logic
(def solution2
  (loop [pos 0
         depth 0
         aim 0
         [x & xs] input02]
    (if (nil? x)
      (* pos depth)
      (let [[dir n] x]
        (case dir
          :down (recur pos depth (+ aim n) xs)
          :up (recur pos depth (- aim n) xs)
          :forward (recur (+ pos n) (+ depth (* aim n)) aim xs))))))

(println "Solution for Part 2:" solution2)

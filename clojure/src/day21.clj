(ns day21
  (:require [utils :refer :all]))

;; Read inputs as just the starting positions
(def input21
  (read-multiline-input "input21.txt" #(Character/digit (last %) 10)))


;; Part 1: losing score times number of rolls ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Model the die as an infinite sequence of sums of 3 rolls
(def dice-rolls
  (->> (range 1 101)
       (cycle)
       (partition 3)
       (map #(apply + %))))

(defn move [curr n]
  (let [x (mod (+ curr n) 10)]
    (if (zero? x)
      10
      x)))

(def solution1
  (let [player1-scores (rest (reductions move
                                         (first input21)
                                         (take-nth 2 dice-rolls)))
        player2-scores (rest (reductions move
                                         (second input21)
                                         (take-nth 2 (rest dice-rolls))))
        losing-score (last (take-while #(< (peek %) 1000)
                                       (map vector
                                            (drop 2 (range))
                                            (interleave (reductions + player1-scores)
                                                        (reductions + player2-scores)))))]
    (reduce * 3 losing-score)))

(print-solution 1 solution1)


;; Part 2: number of winning universes ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Key observation: the players' turns can be modelled independently from each
;; other, and the final totals can later be obtained by multiplying numbers of
;; universes together and summing up.
(def moves-in-a-turn
  (->> (for [x [1 2 3]
             y [1 2 3]
             z [1 2 3]] [x y z])
       (map #(reduce + %))
       frequencies))

(defn play
  "Given a game state (score, current tile, number of turns played, number of
  universes at state) returns a collection of the possible game states for the
  next turn."
  [[score tile n-turns n-universes]]
  (for [[k v] moves-in-a-turn
        :let [s (move tile k)]]
    [(+ score s) s (inc n-turns) (* n-universes v)]))

(defn play-to-end
  "Recursively play turns until reaching the winning score (21), then return a
  vector of the number of turns it took along with the corresponding number of
  universes."
  [[score tile n-turns n-universes :as game-state]]
  (if (>= score 21)
    [[n-turns n-universes]]
    (mapcat play-to-end (play game-state))))

(defn get-win-distribution
  "Given a starting position returns the distribution of the number of turns taken
  to reach a winning score."
  [pos]
  (let [winning-turns (->> (play-to-end [0 pos 0 1])
                           (group-by first)
                           (map (fn [[k v]] [k (reduce + (map peek v))]))
                           (into {}))
        max-turns (apply max (keys winning-turns))
        base (zipmap (range 1 (inc max-turns)) (repeat 0))]
    (into (sorted-map) (merge base winning-turns))))

(defn get-nonwin-distribution
  "Given the win distribution of universes, calculate the corresponding
  distribution of non-winning universes."
  [win-dist]
  (loop [turn 1
         [d & ds] win-dist
         universes (sorted-map 0 1)]
    (if (nil? d)
      universes
      (recur (inc turn)
             ds
             (assoc universes turn (- (* 27 (universes (dec turn)))
                                      (peek d)))))))

(def solution2
  (let [p1-wins (get-win-distribution (first input21))
        p1-nonwins (get-nonwin-distribution p1-wins)
        p2-wins (get-win-distribution (second input21))
        p2-nonwins (get-nonwin-distribution p2-wins)]
    (max
     (->> (for [[t n] p1-wins
                :let [p2 (p2-nonwins (dec t))]]
            (* n p2))
          (reduce +))
     (->> (for [[t n] p2-wins
                :let [p1 (p1-nonwins t)]]
            (* n p1))
          (reduce +)))))

(print-solution 2 solution2)

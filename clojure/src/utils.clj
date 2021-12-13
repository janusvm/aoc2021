(ns utils
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn read-input
  "Read an input file from resources and parse lines using a given function.
  Suitable for input that's a homogeneous stream of one input per line."
  [filename line-parser]
  (->> (io/resource filename)
       io/file
       slurp
       str/split-lines
       (map line-parser)))

(defmacro print-solution
  "Print the solution for a puzzle to the console in a nice format"
  [part solution]
  `(printf "Solution for Day %s, Part %s: %s\n"
           (subs (str (ns-name ~*ns*)) 3) ~part ~solution))

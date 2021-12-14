(ns utils
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn read-multiline-input
  "Read an input file from resources and parse lines using a given function.
  Suitable for input that's a homogeneous stream of one input per line."
  [filename line-parser]
  (->> (io/resource filename)
       io/file
       slurp
       str/split-lines
       (map line-parser)))

(defn read-oneline-input
  "Read an input file from resources, split on a delimiter and parse each element
  using a given function. Suitable for a single-line file with delimited input."
  [filename del el-parser]
  (let [s (->> (io/resource filename)
               io/file
               slurp
               str/split-lines
               first)]
    (->> (str/split s del)
         (map el-parser))))

(defmacro print-solution
  "Print the solution for a puzzle to the console in a nice format"
  [part solution]
  `(printf "Solution for Day %s, Part %s: %s\n"
           (subs (str (ns-name ~*ns*)) 3) ~part ~solution))

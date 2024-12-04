;; # [🎄 Advent of Clerk: 2024: Day 1: Historian Hysteria](https://adventofcode.com/2024/day/1)
(ns advent-of-clerk.day-01
  (:require [nextjournal.clerk :as clerk]
            [clojure.string :as str]))

(def example "
3   4
4   3
2   5
1   3
3   9
3   3
")

(defn transpose [m]
  (apply mapv vector m))

(defn parse-input [input]
  (->> input
       str/split-lines
       (remove str/blank?)
       (map #(->> %
                  (re-seq #"\d+")
                  (map parse-long)))
       transpose))

(def input (slurp "input/01"))

(def puzzle (parse-input input #_example))

;; Part I: Total Distance
(->> puzzle
     (map sort)
     transpose
     (map (fn [[l r]]
            (abs (- l r))))
     (reduce +))

;; Part II: Similartiy Score
(def left  (first  puzzle))
(def right (second puzzle))

(def right-freqs (frequencies right))

(->> left
     (map #(* % (get right-freqs % 0)))
     (reduce +))

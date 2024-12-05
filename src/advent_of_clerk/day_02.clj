;; # [🎄 Advent of Clerk 2024: Day 2: Red-Nosed Reports](https://adventofcode.com/2024/day/2)
(ns advent-of-clerk.day-02
  (:require [nextjournal.clerk :as clerk]
            [clojure.string :as str]))

(def example "
7 6 4 2 1
1 2 7 8 9
9 7 6 2 1
1 3 2 4 5
8 6 4 4 1
1 3 6 7 9
")

(def input (slurp "input/02"))

(defn parse-input [input]
  (->> input
       str/split-lines
       (remove str/blank?)
       (map #(->> % (re-seq #"\d+") (map parse-long)))))

(def puzzle (parse-input #_example input))

;; ## Part I: Report Safety
(partition 2 1 [7 6 4 2 1])

(defn safe? [reports]
  (let [pairs (partition 2 1 reports)
        diffs (map (partial apply -) pairs)]
    (and (or (->> diffs (every? pos?))
             (->> diffs (every? neg?)))
         (->> diffs
              (map abs)                 ; don't forget there's negatives!
              (every? #(and (>= % 1)
                            (<= % 3)))))))

(->> puzzle
     (filter safe?)
     count)

;; ## Part II: Problem Dampener
(defn dampen [reports n]
  (->> reports
       (map-indexed (fn [i r]
                      (if-not (= i n)
                        r)))
       (keep identity)))

(defn dampened-levels [reports]
  (->> reports
       count
       range
       (map #(dampen reports %))))

(dampened-levels [1 2 3 4])

(->> puzzle
     (map dampened-levels)
     (keep (partial some safe?))
     count)

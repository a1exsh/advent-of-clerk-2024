;; # [🎄 Advent of Clerk 2024: Day 3: Mull it Over](https://adventofcode.com/2024/day/3)
(ns advent-of-clerk.day-03
  (:require [nextjournal.clerk :as clerk]
            [clojure.string :as str]))

(def example
  "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))")

(def input (slurp "input/03"))

(defn parse-input [input]
  (->> (re-seq #"mul\((\d{1,3}),(\d{1,3})\)" input)
       (map rest)
       (map (partial map parse-long))))

(def puzzle (parse-input #_example input))

;; ## Part I
(->> puzzle
     (map (partial reduce *))
     (reduce +))

;; ## Part II
(def example-2
  "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))")

(defn parse-input-2 [input]
  (->> (re-seq #"mul\((\d{1,3}),(\d{1,3})\)|do\(\)|don't\(\)" input)
       (reduce (fn [state [what & args]]
                 (cond
                   (= what "do()")    (assoc state :do? true)
                   (= what "don't()") (assoc state :do? false)
                   (str/starts-with? what "mul(") (if (state :do?)
                                                    (update state :muls conj
                                                            (map parse-long args))
                                                    state)
                   :else (throw (Exception. (format "unexpected: %s" what)))))
               {:do? true})))

(def puzzle-2 (parse-input-2 #_example-2 input))

(->> puzzle-2
     :muls
     (map (partial reduce *))
     (reduce +))

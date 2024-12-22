;; # [🎄 Advent of Clerk 2024: Day 7: Bridge Repair](https://adventofcode.com/2024/day/7)
(ns advent-of-clerk.day-07
  (:require [nextjournal.clerk :as clerk]
            [clojure.string :as str]))

(def example "
190: 10 19
3267: 81 40 27
83: 17 5
156: 15 6
7290: 6 8 6 15
161011: 16 10 13
192: 17 8 14
21037: 9 7 18 13
292: 11 6 16 20
")

`(* ~(+ 81 40) 27)
`(+ ~(* 81 40) 27)

(defn parse-input [input]
  (->> input
       str/split-lines
       (remove str/blank?)
       (map #(let [[_ l r] (re-matches #"(\d+): (.*)" %)]
               [(parse-long l)
                (->> (re-seq #"\d+" r)
                     (map parse-long))]))))

(def input (slurp "input/07"))

(def puzzle (parse-input input #_example))

;; ## Part I: Calibration Result

'(((11 6) 16) 20)

(defn make-tree [xs]
  (reduce (fn [acc x]
            (cons acc (list x)))
          (take 2 xs)
          (drop 2 xs)))

(make-tree '(11 6 16 20))

(defn insert-ops [[l r]]
  (for [op '(+ *)
        l_ (if (seq? l)
             (insert-ops l)
             [l])]
    (list op l_ r)))

(def tree1 (make-tree '(11 6 16 20)))

(insert-ops tree1)

(defn eval-form [form]
  (if (seq? form)
    (let [[op l r] form]
      ((case op + + * *) (eval-form l) (eval-form r)))
    form))

(eval (first (insert-ops tree1)))
(eval-form (first (insert-ops tree1)))

(def plus-trees
  (->> puzzle
       (map (fn [[l rs]]
              [l (make-tree rs)]))
       (into [])))

(def plus-forms
  (->> plus-trees
       (map (fn [[l tree]]
              [l (insert-ops tree)]))
       (into [])))

(def plus-vals
  (->> plus-forms
       (map (fn [[l forms]]
              [l (->> forms
                      (map eval-form)
                      (into []))]))
       (into [])))

(def calibrations
  (->> plus-vals
       (filter (fn [[l values]]
                 (some #{l} values)))))

(def correct-test-values
  (map first calibrations))

(def total-calibration-result
  (reduce + correct-test-values))

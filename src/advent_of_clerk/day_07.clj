;; # [🎄 Advent of Clerk 2024: Day 7: Bridge Repair](https://adventofcode.com/2024/day/7)
(ns advent-of-clerk.day-07
  (:require [nextjournal.clerk :as clerk]
            [clojure.string :as str]
            [clojure.math :as math]))

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

(defn make-forms [ops [l r]]
  (for [op ops
        l_ (if (seq? l)
             (make-forms ops l)
             [l])]
    (list op l_ r)))

(def ops1 '(+ *))

(def tree1 (make-tree '(11 6 16 20)))

(def forms1 (make-forms ops1 tree1))

(defn eval-form1 [form]
  (if (seq? form)
    (let [[op l r] form]
      ((case op + + * *) (eval-form1 l) (eval-form1 r)))
    form))

(eval (first forms1))
(eval-form1 (first forms1))

(time
 (def plus-trees
   (->> puzzle
        (map (fn [[l rs]]
               [l (make-tree rs)]))
        (into []))))

(time
 (def plus-forms1
   (->> plus-trees
        (map (fn [[l tree]]
               [l (->> tree
                       (make-forms ops1)
                       (into []))]))
        (into []))))

(time
 (def plus-vals1
   (->> plus-forms1
        (map (fn [[l forms]]
               [l (->> forms
                       (map eval-form1)
                       (into []))]))
        (into []))))

(time
 (def calibrations1
   (->> plus-vals1
        (filter (fn [[l values]]
                  (some #{l} values))))))

(def correct-test-values1
  (map first calibrations1))

(def total-calibration-result1
  (reduce + correct-test-values1))

;; ## Part II: Long Cat
(defn long-cat [& xs]
  (parse-long (apply str xs)))

(def ops2 '(+ * |))

(def forms2 (make-forms ops2 tree1))

(defn eval-form2 [form]
  (if (seq? form)
    (let [[op l r] form]
      ((case op + + * * | long-cat) (eval-form2 l) (eval-form2 r)))
    form))

(eval-form2 (last forms2))

(def longest-equation
  (->> puzzle
       (sort-by (fn [[_l rs]] (count rs)))
       last))

(def longest-equation-length
  (->> longest-equation
       second
       count))

;; With 2 operators it's still reasonable:
(math/pow 2 (dec longest-equation-length))

;; But with 3...
(math/pow 3 (dec longest-equation-length))

(def longest-tree
  (->> longest-equation
       second
       make-tree))

(make-forms ops1 longest-tree)
(make-forms ops2 longest-tree)

;; ### Interlude
;;
;; We need to explore an alternative approach.  Let's do all things in one go.
;;
(defn correct-equation?
  ([op-fns test-value numbers]
   (correct-equation? op-fns test-value (first numbers) (rest numbers)))

  ([op-fns test-value acc numbers]
   (if-not (seq numbers)
     (= test-value acc)
     (if (< test-value acc)
       false                            ; acc is already bigger
       (some #(correct-equation? op-fns
                                 test-value
                                 (% acc (first numbers))
                                 (rest numbers))
             op-fns)))))

(def op-fns1 [+ *])

(def correct-equations1
  (filter #(apply correct-equation? op-fns1 %)
          puzzle))

(def calibration-result1
  (->> correct-equations1
       (map first)
       (reduce +)))

(= calibration-result1 total-calibration-result1)

(def op-fns2 [+ * long-cat])

(def correct-equations2
  (filter #(apply correct-equation? op-fns2 %)
          puzzle))

(def calibration-result2
  (->> correct-equations2
       (map first)
       (reduce +)))

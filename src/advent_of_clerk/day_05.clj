;; # [🎄 Advent of Clerk 2024: Day 5: Print Queue](https://adventofcode.com/2024/day/5)
(ns advent-of-clerk.day-05
  (:require [nextjournal.clerk :as clerk]
            [clojure.string :as str]
            [datascript.core :as d]
            [arrowic.core :as arr]))

(def example
  "47|53
97|13
97|61
97|47
75|29
61|13
75|53
29|13
97|29
53|29
61|53
97|53
61|29
47|13
75|47
97|75
47|61
75|61
47|29
75|13
53|13

75,47,61,53,29
97,61,53,29,13
75,29,13
75,97,47,61,53
61,13,29
97,13,75,29,47")

(def input (slurp "input/05"))

(defn parse-input [input]
  (let [lines (str/split-lines input)
        [rule-lines _ update-lines] (partition-by str/blank? lines)]
    {:rules (->> rule-lines
                 (map #(str/split % #"\|"))
                 (into #{}))
     :updates (->> update-lines
                   (map #(str/split % #","))
                   (into []))}))

(def puzzle (parse-input #_input example))

;; ## Graph viz
(-> (clerk/html
     (arr/as-svg
      (arr/with-graph (arr/create-graph)
        (let [vertices (volatile! {})
              vertex!  (fn [k]
                         (if-let [v (get @vertices k)]
                           v
                           (let [v (arr/insert-vertex! k)]
                             (vswap! vertices assoc k v)
                             v)))]
          (doseq [[page-before page-after] (puzzle :rules)]
            (arr/insert-edge! (vertex! page-before) (vertex! page-after)))))))
    (assoc :nextjournal/width :full))

;; ## Datascript
(def conn (d/create-conn))

(->> puzzle
     :rules
     (map (fn [[page-before page-after]]
            {:db/id (format "%s|%s" page-before page-after)
             :page-before page-before
             :page-after page-after}))
     (d/transact! conn))

(d/q '[:find ?page-before
       :where [_ :page-before ?page-before]]
     @conn)

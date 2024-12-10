;; # [🎄 Advent of Clerk 2024: Day 5: Print Queue](https://adventofcode.com/2024/day/5)
(ns advent-of-clerk.day-05
  (:require [nextjournal.clerk :as clerk]
            [clojure.string :as str]
            [clojure.set :as set]
            #_[clojure.walk :as walk]
            #_[datascript.core :as d]
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

(def puzzle (parse-input #_example input))

;; ## Graph viz
(defn render-graph [edges]
  (clerk/html
   (arr/as-svg
    (arr/with-graph (arr/create-graph)
      (let [vertices (volatile! {})
            vertex!  (fn [k]
                       (if-let [v (get @vertices k)]
                         v
                         (let [v (arr/insert-vertex! k)]
                           (vswap! vertices assoc k v)
                           v)))]
        (doseq [[page-before page-after] edges]
          (arr/insert-edge! (vertex! page-before)
                            (vertex! page-after)
                            :label (format "%s -> %s" page-before page-after))))))))

#_(-> (render-graph (puzzle :rules))
    #_(assoc :nextjournal/width :full))

;; ## Walking the graph
;;
;; Let's make a mapping from each page that should be printed "before" to all
;; pages that should be printed "after" it:
;;
(def before->after
  (->> puzzle
       :rules
       (reduce (fn [m [b a]]
                 (update m b (fnil conj #{}) a))
               {})))
;;
;; Now let's resolve all transitive relationships, such that if `a` is after
;; `b` and `c` is after `a`, then `b` should also have `c` in its "after".
;;
;; To achieve that we shall iterate over the mapping until it no longer leads
;; to an extension (becomes "saturated").
;;
;; Let's begin by defining a single step of such iteration:
;;
(defn saturate-step [m]
  (->> m
       (map (fn [[b as]]
              [b (reduce #(set/union %1 (m %2)) as as)]))
       (into {})))
;;
;; For example:
;;
(saturate-step {:a #{:b} :b #{:c} :c #{:d :e}})
((comp saturate-step saturate-step) {:a #{:b} :b #{:c} :c #{:d :e}})

;;
;; Saturation may take many steps until it doesn't lead to a different result:
;;
(defn saturate [m]
  (let [next (saturate-step m)]
    (if (= m next)
      m
      (recur next))))

(saturate {:a #{:b} :b #{:c} :c #{:d :e}})

;;
;; Lo and behold, it's already saturated!
;;
(def saturated (saturate before->after))

(= before->after saturated)

;; Or is it?.. 🤔

(def saturated-edges
  (->> saturated
       (mapcat (fn [[b as]]
                 (map (fn [a] [b a]) as)))
       (into #{})))

#_
(-> (render-graph saturated-edges)
    #_(assoc :nextjournal/width :full))

;;
;; Let's look at the difference between saturated and the initial edges:
;;
(set/difference saturated-edges
                (puzzle :rules))

#_(-> (render-graph (set/difference saturated-edges
                                  (puzzle :rules)))
    (assoc :nextjournal/width :full))

;; ## Datascript
(comment
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
       @conn))

;; ## Part I
;;
;; Looks like we don't even need to saturate the graph for the first part.
;;
(defn correctly-ordered? [pages]
  (->> pages
       (partition 2 1)
       (map vec)
       (every? (puzzle :rules))))

(defn middle-page [pages]
  (nth pages (quot (count pages) 2)))

(middle-page ["1" "2" "3"])

(def correctly-ordered-updates
  (->> puzzle
       :updates
       (filter correctly-ordered?)))

(def middle-pages
  (map middle-page correctly-ordered-updates))

(->> middle-pages
     (map parse-long)
     (reduce +))

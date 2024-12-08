;; # [🎄 Advent of Clerk 2024: Day 4: Ceres Search](https://adventofcode.com/2024/day/4)
(ns advent-of-clerk.day-04
  (:require [nextjournal.clerk :as clerk]
            [clojure.string :as str]))

(def example
  "
MMMSXXMASM
MSAMXMSMSA
AMXSXMAAMM
MSAMASMSMX
XMASAMXAMM
XXAMMXXAMA
SMSMSASXSS
SAXAMASAAA
MAMMMXMMMM
MXMXAXMASX")

(def input (slurp "input/04"))

(defn parse-input [input]
  (->> input
       (str/split-lines)
       (remove str/blank?)
       (map vec)
       vec))

(def puzzle (parse-input #_example input))

(def height (count puzzle))
(def width  (count (first puzzle)))

#_(defn within? [rows x y]
  (and (<= 0 y (dec height))
       (<= 0 x (dec width))))

(defn letter-at [rows x y]
  (get-in rows [y x]))

;; No need for explicit `within?` checks if we can handle `nil`s:
(letter-at puzzle -2 0)

;; ## Part I
(defn search-word [rows x y dx dy word]
  (tap> [x y word])
  (if-not (seq word)
    true
    (if-not (= (first word) (letter-at rows x y))
      false
      (search-word rows (+ x dx) (+ y dy) dx dy (rest word)))))

;; (add-tap prn)

;; Search for a word where it does exist:
(search-word puzzle 5 0 1 0 "XMAS")

(->> (for [y (range height)
           x (range width)
           dy [-1 0 1]
           dx [-1 0 1]
           :when (not= 0 dx dy)]           ; ensure we're moving somewhere
       (search-word puzzle x y dx dy "XMAS"))
     (filter true?)
     count)

;; ## Part II: X-MAS
(defn x-mas? [rows x y]
  (and (or (search-word rows x y 1 1 "MAS")
           (search-word rows x y 1 1 "SAM"))
       (or (search-word rows x (+ 2 y) 1 -1 "MAS")
           (search-word rows x (+ 2 y) 1 -1 "SAM"))))

(x-mas? puzzle 1 0)

(->> (for [y (range height)
           x (range width)]
       (x-mas? puzzle x y))
     (filter true?)
     count)

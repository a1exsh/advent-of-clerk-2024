;; # [🎄 Advent of Clerk 2024: Day 8: Resonant Collinearity](https://adventofcode.com/2024/day/8)
(ns advent-of-clerk.day-08
  (:require [nextjournal.clerk :as clerk]
            [clojure.string :as str]))

(def example "
............
........0...
.....0......
.......0....
....0.......
......A.....
............
............
........A...
.........A..
............
............
")

(defn parse-input [input]
  (->> input
       str/split-lines
       (remove str/blank?)
       (map vec)
       vec))

(def input (slurp "input/08"))

(def puzzle (parse-input input #_example))

;; ## Part I
;;
;; We will have to check each pair of antennas with the same frequency.  To
;; speed up the search, let's collect all coordinates per frequency first:
;;
(def antenna-coords-by-freq
  (->> puzzle
       (map-indexed (fn [y row]
                      (map-indexed (fn [x c]
                                     [c [y x]])
                                   row)))
       (apply concat)
       (remove #(-> % first (= \.)))
       (group-by first)
       (map (fn [[f as]]
              [f (->> as
                      (map second)
                      (into #{}))]))
       (into {})))

;;
;; Now we need to consider each pair.  The easiest way to achieve that seems
;; to be to use a `for` list comprehension:
;;
(defn pairs [xs]
  (let [v (vec xs)                      ; make it a vector to use `get` later
        n (count v)]
    (for [i (range n)                   ; 0 ≤ i < n
          j (range (inc i) n)]          ; i < j < n
      [(get v i)
       (get v j)])))

(pairs "abc")

(->> antenna-coords-by-freq
     (map (fn [[f coords]]
            [f (pairs coords)])))

;; Now it's time to look at a pair of antenna coordinates.
;;
;; First, we make a vector from a pair of coordinates:
;;
(defn coord-diff [[y1 x1] [y2 x2]]
  [(- y2 y1)
   (- x2 x1)])

;; The antinodes are to be found by extending the vector in both directions
;; from the antennas:
;;
(defn antinode-coords [[y1 x1] [y2 x2]]
  (let [[dy dx] (coord-diff [y1 x1] [y2 x2])]
    [[(+ y2 dy) (+ x2 dx)]
     [(- y1 dy) (- x1 dx)]]))

;; ```
;; ..........
;; ...#......
;; ..........
;; ....a.....
;; ..........
;; .....a....
;; ..........
;; ......#...
;; ..........
;; ..........
;; ```
(coord-diff [3 4] [5 5])
(antinode-coords [3 4] [5 5])

(defn coords-within-bounds? [maxy maxx [y x]]
  (and (<= 0 y maxy)
       (<= 0 x maxx)))

(def maxy (->> puzzle count dec))
(def maxx (->> puzzle first count dec))

;; Finally, we can apply all of the above tools to find the answer:
(def distinct-antinode-coords
  (->> antenna-coords-by-freq
       (map (fn [[_ coords]]
              (pairs coords)))
       (mapcat (fn [coord-pairs]
                 (mapcat (partial apply antinode-coords) coord-pairs)))
       (filter (partial coords-within-bounds? maxy maxx))
       distinct))

(count distinct-antinode-coords)

;; Let's check the picture:
(def board-with-antinodes
  (reduce (fn [board yx]
            (update-in board yx #(if (= \. %) \# %)))
          puzzle
          distinct-antinode-coords))

(defn render-board [board]
  (->> board
       (map (partial apply str))
       (str/join "\n")))

(clerk/html
 [:pre (render-board board-with-antinodes)])

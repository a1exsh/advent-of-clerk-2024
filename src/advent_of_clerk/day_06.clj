;; # [🎄 Advent of Clerk 2024: Day 6: Guard Gallivant](https://adventofcode.com/2024/day/6)
(ns advent-of-clerk.day-06
  (:require [nextjournal.clerk :as clerk]
            [nextjournal.clerk.viewer :as v]
            [clojure.string :as str]))

(def example
  "
....#.....
.........#
..........
..#.......
.......#..
..........
.#..^.....
........#.
#.........
......#...")

(def input (slurp "input/06"))

(defn parse-input [input]
  (->> input
       str/split-lines
       (drop-while str/blank?)
       (map vec)
       (into [])))

(def puzzle (parse-input #_example input))

(def height (count puzzle))
(def width  (count (first puzzle)))

(def directions "^>v<")

;;
;; We are going to use `get-in` and similar to access the board cells at
;; coordinates `[y x]`.
;;
;; First we need to be able to find the guard's position and direction:
;;
(defn find-guard [board]
  (->> board
       (map-indexed (fn [y row]
                      (->> row
                           (map-indexed (fn [x c]
                                          [[y x] c])))))
       (apply concat)
       (filter (fn [[_ c]]
                 (some #{c} directions))) ; NB: checking all directions
       (map (fn [[pos c]] {:pos pos :dir c}))
       first))

(def guard (find-guard puzzle))

;;
;; To avoid confusion in case the guard stumbles upon her own initial position
;; we denote it as an empty space:
;;
(def clean-board (assoc-in puzzle (guard :pos) \.))

;; For each direction, define the next move's dy and dx:
(def dir->dyx
  {\^ [-1  0]
   \> [ 0  1]
   \v [ 1  0]
   \< [ 0 -1]})

;; And the next direction in case we have to turn:
(def next-dir
  {\^ \>
   \> \v
   \v \<
   \< \^})

;;
;; Now, given the current state of the board, including the guard's position
;; `[y x]` and direction `dir`, we should be able to derive the state of the
;; board after one move, according to the rules.
;;
(defn move-guard [board {:keys [pos dir]}]
  (let [[y x]   pos
        [dy dx] (dir->dyx dir)
        next-pos [(+ y dy) (+ x dx)]
        c (get-in board next-pos)]
    (case c
      nil nil                                 ; gone
      \#  {:pos pos      :dir (next-dir dir)} ; blocked, turn
      \.  {:pos next-pos :dir dir}            ; free, move there
      (throw (Exception. (format "Something unexpected found: %c" c))))))

;; ## Part I
;;
;; Finally, we are ready to move the guard a step, or two.  NB: using the
;; clean board to avoid stumbling upon our own initial position!
;;
(def guard-path
  (->> guard
       (iterate (partial move-guard clean-board))
       (take-while #(not (nil? %)))))

;; The actual answer to the first part of the puzzle:
(->> guard-path
     (map :pos)
     (cons (guard :pos))                ; don't forget the initial position!
     distinct
     count)

;;
;; Let's try to draw the path that the guard has taken by commencing some 🐢
;; graphics.  Using `reductions` to also capture all intermediate frames:
;;
(def guard-path-frames
  (->> guard-path
       (reductions (fn [board guard]
                     (assoc-in board (guard :pos) (guard :dir)))
                   puzzle)))

(defn render-board [board]
  (->> board
       (map (partial apply str))
       (str/join "\n")))

(def board-viewer
  {:transform-fn (clerk/update-val #(-> (clerk/html [:pre (render-board %)])
                                        (assoc :nextjournal/width :full)))})


;; Magic: https://github.com/a1exsh/advent-of-clerk-2022/blob/38eef2ed8c921d673a7775503f144c0121c45c50/src/advent_of_clerk/day_12.clj#L186
(defn slider-viewer [max-value]
  {:transform-fn (comp (clerk/update-val symbol)
                       clerk/mark-presented)
   :render-fn `(fn [x]
                 [:input {:type :range
                          :value (:counter @@(resolve x))
                          :min 0
                          :max ~max-value
                          :on-change #(swap! @(resolve x)
                                             assoc
                                             :counter
                                             (int (.. % -target -value)))}])})

^::clerk/sync
(defonce frame* (atom {:counter 0}))
#_(reset! frame* {:counter 0})

(v/with-viewer (slider-viewer (dec (count guard-path-frames)))
  `frame*)

(def frame-number (:counter @frame*))
(def frame-to-render (nth guard-path-frames frame-number))


(v/with-viewer board-viewer
  frame-to-render)

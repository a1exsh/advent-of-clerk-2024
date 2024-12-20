;; # [🎄 Advent of Clerk 2024: Day 6: Guard Gallivant](https://adventofcode.com/2024/day/6)
(ns advent-of-clerk.day-06
  (:require [nextjournal.clerk :as clerk]
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

(def initial-guard (find-guard puzzle))

;;
;; To avoid confusion in case the guard stumbles upon her own initial position
;; we denote it as an empty space:
;;
(def clean-board (assoc-in puzzle (initial-guard :pos) \.))

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
      nil     nil                                 ; gone
      (\# \O) {:pos pos      :dir (next-dir dir)} ; blocked, turn
      \.      {:pos next-pos :dir dir}            ; free, move there
      (throw (Exception. (format "Something unexpected found: %c" c))))))

;; ## Part I
;;
;; Finally, we are ready to move the guard a step, or two.  NB: using the
;; clean board to avoid stumbling upon our own initial position!
;;
(def guard-path
  (->> initial-guard
       (iterate (partial move-guard clean-board))
       (take-while #(not (nil? %)))))

;; The actual answer to the first part of the puzzle:
(->> guard-path
     (map :pos)
     ;; initial position is implicitly included, due to how `iterate` works
     #_(cons (guard :pos))
     distinct
     count)

;;
;; Let's try to draw the path that the guard has taken by commencing some 🐢
;; graphics.  Using `reductions` to also capture all intermediate frames:
;;
(defn path-frames [board path]
  (->> path
       (reductions (fn [b step]
                     (assoc-in b (step :pos) (step :dir)))
                   board)
       (drop 1)                         ; nothing happens in the first frame
       ))

(def guard-path-frames
  (path-frames puzzle guard-path))

(defn render-board [board]
  (->> board
       (map (partial apply str))
       (str/join "\n")))

(def frames-viewer
  {:transform-fn clerk/mark-presented
   :render-fn '(fn [rendered-frames]
                 (reagent.core/with-let [frame* (reagent.core/atom 0)]
                   (let [max-value (dec (count rendered-frames))
                         value (min @frame* max-value)]
                     [:div
                      [:input {:type :range
                               :value value
                               :min 0
                               :max max-value
                               :on-change #(reset! frame*
                                                   (int (.. % -target -value)))}]
                      [:p "Frame: " value]
                      [:pre (get rendered-frames value)]])))})

(time
 (def rendered-frames
   (mapv render-board guard-path-frames)))

#_
^{::clerk/width :full}
(clerk/with-viewer frames-viewer
  rendered-frames)

;; ## Part II: Introducing Obstructions
;;
;; First we need to be able to detect a loop in the path.  Luckily, we have
;; all the information we need, that is: as soon as the guard returns into a
;; position that she has already visited *and* is facing the same direction as
;; before, we can assume she is going to be stuck in this loop forever.
;;
(defn determine-fate
  ([board guard]
   (determine-fate #{} [] board guard))

  ([steps path board next-step]
   (if (nil? next-step)
     {:fate :gone
      :path path}
     (if (contains? steps next-step)
       {:fate :stuck
        :path (conj path next-step)}
       (recur (conj steps next-step)
              (conj path next-step)
              board
              (move-guard board next-step))))))

(determine-fate clean-board initial-guard)

;; In the example, put an obstruction next to the guard's initial position:
(comment
  (def example-obstruction1-board
    (assoc-in clean-board [6 3] \O))

  (def example-obstruction1-fate
    (determine-fate example-obstruction1-board initial-guard))

  ^{::clerk/width :full}
  (clerk/with-viewer frames-viewer
    (->> example-obstruction1-fate
         :path
         (path-frames example-obstruction1-board)
         (mapv render-board))))

;;
;; It only makes sense to put the obstructions along the path taken by the
;; guard, which helps to reduce the search space dramatically.
;;
;; The trick is to track the positions of the obstructions so placed, so that
;; we can later count only the distinct positions (the guard may cross the
;; same position more than one time, as we know).
;;
(defn foresee-fates [board guard]
  (loop [fates []
         next-step guard]
    (if (nil? next-step)
      fates
      ;; always try to put an obstruction on the next step to be taken:
      (let [obstruction (move-guard board next-step)]
        (recur (if (and obstruction
                        ;; don't put obstruction on the initial guard position
                        (not= (:pos obstruction) (:pos guard)))
                 (let [obstructed-board (assoc-in board (:pos obstruction) \O)
                       ;; always start from the initial guard position!
                       fate (determine-fate obstructed-board guard)]
                   (conj fates
                         (assoc fate
                                :board obstructed-board
                                :obstruction obstruction)))
                 fates)
               (move-guard board next-step))))))

(time
 (def all-fates
   (foresee-fates clean-board initial-guard)))

(count all-fates)

(time
 (def all-stuck-fates
   (->> all-fates
        (filter #(-> % :fate (= :stuck)))
        (into []))))

(count all-stuck-fates)

;; Answer to the second part of the puzzle:
(->> all-stuck-fates
     (map (comp :pos :obstruction))
     distinct
     count)

;; ### All fates
(def slider-viewer
  {:transform-fn clerk/mark-presented
   :render-fn '(fn [sym]
                 (let [atom* @(resolve sym)
                       slider @atom*]
                   [:input {:type :range
                            :value (:value slider)
                            :min 0
                            :max (:max slider)
                            :on-change #(swap! atom*
                                               assoc
                                               :value
                                               (int (.. % -target -value)))}]))})

^::clerk/sync
(defonce fate-number*
  (atom {:value 0
         :max (dec (count all-fates))}))

#_(swap! fate-number* assoc :max (dec (count all-fates)))

(clerk/with-viewer slider-viewer
  `fate-number*)

^{::clerk/visibility {:code :hide}}
(clerk/html
 [:div "Fate: "
  (:value @fate-number*) " / " (:max @fate-number*)])

;;^{::clerk/auto-expand-results? true}
(def fate
  (nth all-fates (:value @fate-number*)))

^{::clerk/width :full}
(clerk/with-viewer frames-viewer
  (->> (fate :path)
       (path-frames (fate :board))
       (mapv render-board)))

;; ### All stuck fates
^::clerk/sync
(defonce stuck-fate-number*
  (atom {:value 0
         :max (dec (count all-stuck-fates))}))

#_(swap! stuck-fate-number* assoc :max (dec (count all-stuck-fates)))

(clerk/with-viewer slider-viewer
  `stuck-fate-number*)

^{::clerk/visibility {:code :hide}}
(clerk/html
 [:div "Stuck fate: "
  (:value @stuck-fate-number*) " / " (:max @stuck-fate-number*)])

;; ^{::clerk/auto-expand-results? true}
(def stuck-fate
  (nth all-stuck-fates (:value @stuck-fate-number*)))

^{::clerk/width :full}
(clerk/with-viewer frames-viewer
  (->> (stuck-fate :path)
       (path-frames (stuck-fate :board))
       (mapv render-board)))

;;
;; **Uncomment only for the _example_ input.**
;;
;; The for real puzzle it will try to use ~180 GB of memory to render...
(comment
  (->> all-stuck-fates
       (map (fn [fate]
              ^{::clerk/width :full}
              (clerk/with-viewer frames-viewer
                (->> (fate :path)
                     (path-frames (fate :board))
                     (mapv render-board)))))))

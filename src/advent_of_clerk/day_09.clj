;; # [🎄 Advent of Clerk 2024: Day 9: Disk Fragmenter](https://adventofcode.com/2024/day/9)
(ns advent-of-clerk.day-09
  (:require [nextjournal.clerk :as clerk]
            [clojure.string :as str]))

(def example "2333133121414131402")

(defn normalize [block-vec]
  {:pre [(vector? block-vec)]}          ; important for fast random access
  (loop [n (count block-vec)]
    (if (zero? n)
      []
      (if-not (neg? (nth block-vec (dec n)))
        (subvec block-vec 0 n)
        (recur (dec n))))))

(defn parse-input [input]
  (->> input
       (map-indexed (fn [i c]
                      (let [n (- (int c) (int \0))]
                        (repeat n (if (even? i)
                                    (quot i 2)
                                    -1)))))
       (apply concat)
       vec
       normalize))

(def input (slurp "input/09"))

(def puzzle (parse-input input #_example))

;; ## Part I
(defn first-gap-index [block-vec]
  (->> block-vec
       (map-indexed (fn [& x] x))
       (filter #(-> % second neg?))
       (map first)
       first))

(defn defrag1 [block-vec]
  {:pre [(vector? block-vec)]}
  (if-let [gap-index (first-gap-index block-vec)]
    (let [n (count block-vec)
          last-index (dec n)
          last-block (nth block-vec last-index)]
      (-> block-vec
          (subvec 0 last-index)
          (assoc gap-index last-block)
          normalize))
    block-vec))

(defrag1 [0 1 2 -1 -1 3 4 -1 5 -1 6 6])

;; This becomes too expensive with the real input, as it bears $O(n^2)$
;; complexity.
;;
(comment
  (def defragmented
    (->> (iterate defrag1 puzzle)
         (drop-while #(not (nil? (first-gap-index %))))
         first)))

;; Let's try to do it in a single loop instead:
;;
(defn defrag [block-vec]
  {:pre [(vector? block-vec)]}
  (loop [skip-left-count 0
         defraged-vec    block-vec]
    (let [gap-index (first-gap-index (subvec defraged-vec skip-left-count))]
      (if-not gap-index
        defraged-vec                    ; no more gaps, we're done
        (let [global-gap-index (+ skip-left-count gap-index)]
          (recur global-gap-index
                 (let [n (count defraged-vec)
                       last-index (dec n)
                       last-block (nth defraged-vec last-index)]
                   (-> defraged-vec
                       (subvec 0 last-index) ; shrink by 1 block
                       (assoc global-gap-index last-block)
                       normalize))))))))

(def defragmented
  (defrag puzzle))

(defn checksum [block-vec]
  (->> block-vec
       (map-indexed (fn [i b]
                      {:pre [(>= b 0)]} ; don't accidentally ignore gaps
                      (* i b)))
       (reduce +)))

(checksum defragmented)

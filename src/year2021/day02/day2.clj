(ns year2021.day02.day2
  (:require [ysera.test :refer [is is= is-not]]
            [clojure.string :as str :only [index-of]]))

; https://adventofcode.com/2021/day/2

(defn moves2pos
  {:doc  "
  Convert moves list to position and depth"
   :test (fn []
           (is= (moves2pos '([forward 5] [down 5] [forward 8] [up 3] [down 8] [forward 2])) [15 10])
           )}
  [m]
  (reduce #(let [
                 cur-pos (first %1)
                 cur-depth (second %1)
                 dir (first %2)
                 steps (second %2)
                 new-pos (+ cur-pos (if (= dir (symbol "forward")) steps 0))
                 new-depth (+ cur-depth
                              (if (= dir (symbol "down")) steps 0)
                              (if (= dir (symbol "up")) (- steps) 0))
                 ]
             (vector new-pos new-depth)
             ) [0 0] m)
  )

(defn input2list
  {:doc  "
  Converts input string to list of vectors"
   :test (fn []
           (is= (input2list "forward 5\ndown 5\nforward 8\nup 3\ndown 8\nforward 2")
                '([forward 5][down 5][forward 8][up 3][down 8][forward 2]))
           )}
  [diffs]
  (->> diffs
       (str/split-lines)
       (map #(str/split % #" "))
       (map #(map read-string %))
       )
  )

(defn part1
  [nums]
  (->> nums
       (input2list)
       (moves2pos)
       (reduce * 1)
       )
  )

(comment
  (def puzzle-input "forward 4\ndown 9\nforward 6\ndown 5\nup 2\nforward 5\nforward 7\nup 5\ndown 9\nup 6\ndown 6\ndown 1\ndown 1\nup 2\ndown 3\nup 3\nforward 8\nforward 7\ndown 6\ndown 7\nforward 6\nforward 9\nforward 7\nup 9\ndown 4\ndown 6\ndown 5\ndown 9\nforward 8\ndown 9\nforward 9\nforward 4\nforward 4\nup 3\nup 8\ndown 9\ndown 8\ndown 4\nforward 5\nforward 4\nup 6\nforward 6\nup 3\nup 8\nup 3\nup 4\ndown 3\ndown 5\ndown 5\nup 1\nforward 9\ndown 4\nforward 6\ndown 6\nup 2\nup 9\nforward 1\nforward 2\nforward 7\ndown 6\nup 6\nforward 1\nforward 7\ndown 7\nforward 9\nforward 4\nforward 6\ndown 5\nup 9\ndown 1\nup 5\nup 5\nup 9\ndown 5\nforward 7\ndown 1\nup 9\ndown 7\nforward 2\ndown 4\ndown 4\nforward 8\nforward 8\ndown 6\ndown 3\nup 7\ndown 3\nforward 9\ndown 7\nforward 2\ndown 1\nforward 5\nup 9\ndown 2\nup 2\ndown 3\nup 7\nforward 9\nforward 7\ndown 4\ndown 5\nup 3\ndown 3\ndown 5\nforward 9\ndown 3\nforward 9\ndown 3\nup 9\ndown 5\nforward 4\ndown 4\nup 8\nforward 7\nup 1\ndown 2\nforward 4\ndown 7\ndown 9\ndown 4\ndown 4\nforward 6\ndown 7\ndown 2\ndown 1\nforward 1\ndown 2\nforward 1\ndown 7\nforward 5\nup 3\nforward 6\nup 9\ndown 3\ndown 3\ndown 9\nforward 4\ndown 4\nforward 9\nforward 6\ndown 7\nup 9\nup 6\nforward 4\ndown 5\nforward 2\ndown 7\ndown 7\nforward 4\nforward 5\ndown 8\ndown 5\nup 4\nforward 7\nup 8\ndown 8\nforward 4\nforward 5\ndown 6\ndown 1\ndown 1\ndown 9\nforward 4\nup 1\ndown 8\nup 7\ndown 1\nup 2\nforward 4\ndown 7\ndown 7\ndown 2\nforward 7\ndown 2\nup 1\nup 4\ndown 6\nforward 5\nforward 2\nup 1\nforward 2\nforward 9\nup 9\nup 7\nforward 9\ndown 8\nup 5\ndown 6\ndown 6\nup 8\ndown 1\nforward 6\ndown 5\nforward 2\ndown 9\ndown 9\nup 4\nforward 4\nforward 2\nforward 7\nforward 3\ndown 1\nforward 8\nup 9\ndown 7\nforward 9\nforward 1\nforward 5\nup 6\ndown 6\nforward 6\nup 3\nforward 9\ndown 3\nforward 2\ndown 7\ndown 3\nup 9\ndown 2\ndown 3\nforward 5\ndown 9\nforward 8\ndown 2\nforward 1\ndown 9\ndown 7\nforward 2\nforward 6\nforward 4\nforward 5\ndown 5\ndown 1\nforward 5\nup 4\ndown 4\nup 8\ndown 4\nup 4\ndown 1\ndown 2\ndown 9\ndown 2\nup 4\ndown 1\nforward 2\nforward 1\nforward 9\ndown 5\nup 4\nup 1\nforward 8\nforward 6\nforward 9\nup 9\nforward 4\nforward 4\ndown 1\nforward 6\nforward 7\nforward 3\nup 5\nup 7\ndown 1\nforward 4\ndown 3\ndown 5\nup 7\ndown 4\nup 9\ndown 3\ndown 5\nforward 7\nforward 8\nup 5\nup 1\nforward 3\nup 8\nforward 3\ndown 2\nforward 1\nforward 9\nforward 1\ndown 2\nforward 7\ndown 5\nforward 6\ndown 9\nup 9\nforward 5\nforward 7\nforward 6\ndown 2\nup 2\nforward 3\nforward 4\nforward 3\ndown 5\nforward 1\nforward 2\nforward 6\ndown 4\nforward 2\nforward 6\nup 8\nforward 2\nup 4\nforward 7\ndown 2\nforward 1\nforward 7\ndown 6\nforward 4\ndown 3\ndown 2\ndown 2\nforward 4\ndown 8\nforward 6\nforward 6\ndown 2\nup 3\nup 1\nforward 1\ndown 5\ndown 2\nforward 4\nforward 7\nforward 3\ndown 3\nforward 9\ndown 1\ndown 7\nforward 6\nforward 1\nup 6\nforward 7\nforward 1\ndown 5\ndown 4\nforward 6\nup 1\ndown 1\nup 9\ndown 2\ndown 2\nforward 3\nup 4\ndown 5\ndown 5\ndown 3\ndown 6\nup 8\nforward 2\nforward 2\ndown 6\ndown 1\nup 4\nup 1\ndown 5\nup 4\nup 2\nforward 4\nforward 6\nforward 3\ndown 7\nforward 8\nup 5\nforward 5\ndown 1\nforward 2\nforward 6\ndown 8\nup 6\ndown 1\ndown 7\nforward 4\nforward 2\nup 1\ndown 6\nforward 3\nforward 1\nforward 5\nforward 9\nforward 9\ndown 4\nforward 2\ndown 1\nforward 1\nforward 7\nforward 5\ndown 9\ndown 8\ndown 1\ndown 6\ndown 1\nup 7\ndown 3\nforward 3\nup 6\nup 4\ndown 7\ndown 7\nforward 6\nup 7\ndown 7\nforward 9\ndown 9\ndown 3\nforward 6\nforward 9\nforward 1\ndown 4\nforward 5\ndown 4\ndown 2\ndown 3\nup 3\nforward 9\nforward 7\nforward 5\ndown 5\nforward 7\nup 4\ndown 1\nforward 3\ndown 3\nforward 4\ndown 9\nforward 2\ndown 5\ndown 1\nforward 8\ndown 3\nforward 7\nup 1\ndown 3\nforward 2\nup 8\ndown 2\nforward 4\nforward 4\nforward 4\ndown 5\nup 6\ndown 3\nforward 5\ndown 4\nup 5\nforward 1\nforward 6\nup 1\ndown 3\nforward 2\nforward 9\ndown 7\ndown 4\nforward 5\nup 3\nup 6\nup 1\nforward 4\nforward 1\nforward 1\ndown 7\nup 4\ndown 3\ndown 8\ndown 3\nforward 8\nforward 3\ndown 6\ndown 9\nforward 3\nforward 9\nforward 7\ndown 8\ndown 6\ndown 4\nforward 2\nup 4\nforward 8\ndown 1\nforward 9\nforward 1\ndown 9\nforward 2\ndown 7\ndown 2\nup 7\ndown 1\nup 8\nforward 8\ndown 7\nforward 1\ndown 1\nforward 3\nforward 1\nup 2\ndown 7\ndown 5\nforward 5\ndown 8\nforward 4\ndown 1\nup 2\nup 8\ndown 8\ndown 1\ndown 5\nup 3\nforward 3\nforward 5\ndown 2\nup 4\ndown 2\nforward 7\nforward 9\nup 9\nup 7\nforward 1\nup 4\nforward 3\nup 5\nforward 9\nforward 9\nforward 6\nforward 2\ndown 7\nforward 8\nforward 4\nforward 7\ndown 8\ndown 5\ndown 6\nforward 6\ndown 4\ndown 1\ndown 9\ndown 1\nforward 3\nforward 5\ndown 6\ndown 7\ndown 9\ndown 8\ndown 4\nup 5\nforward 7\ndown 9\nforward 6\ndown 7\nforward 5\ndown 5\nforward 1\ndown 5\ndown 3\nup 9\nup 3\nforward 2\nup 9\nforward 6\ndown 1\ndown 5\ndown 9\ndown 4\nup 6\nforward 9\ndown 4\ndown 9\ndown 5\ndown 8\ndown 5\ndown 4\nup 5\ndown 8\nup 8\nforward 5\ndown 9\nforward 2\nup 2\ndown 6\nforward 2\nforward 4\nforward 6\ndown 6\ndown 1\nforward 8\ndown 5\ndown 5\nforward 2\ndown 7\ndown 5\ndown 6\ndown 9\nforward 4\nup 9\ndown 3\ndown 7\nforward 3\ndown 5\nup 1\nforward 5\nup 2\ndown 2\nforward 2\nup 3\nup 6\nforward 2\nforward 7\ndown 8\nforward 8\nforward 7\nforward 6\ndown 5\ndown 6\ndown 6\ndown 9\nup 5\ndown 3\nup 1\nup 9\nup 5\ndown 4\ndown 4\ndown 8\nforward 8\nup 5\ndown 9\nforward 1\nup 1\nforward 2\ndown 9\nforward 5\nup 9\nforward 7\ndown 7\ndown 5\nup 1\nup 2\ndown 8\ndown 7\nup 4\nforward 9\ndown 4\nup 8\ndown 5\ndown 1\nforward 9\ndown 6\nup 8\ndown 6\nforward 7\nup 6\nup 5\nforward 2\nup 7\nforward 7\nforward 5\ndown 1\nforward 9\ndown 8\nforward 9\ndown 3\ndown 3\nforward 9\nup 1\ndown 2\nforward 9\ndown 7\nforward 4\nforward 3\nforward 4\ndown 5\nforward 9\nforward 9\ndown 5\nforward 4\ndown 5\ndown 2\ndown 6\nforward 5\nforward 8\nforward 6\nup 9\ndown 9\nforward 7\ndown 6\ndown 7\ndown 4\nforward 1\nforward 3\nforward 6\nforward 4\nforward 3\nforward 4\ndown 1\nforward 2\nforward 3\nforward 9\nup 8\nforward 6\ndown 1\nup 5\ndown 1\ndown 4\ndown 7\ndown 5\ndown 9\ndown 2\ndown 9\nforward 2\ndown 2\nup 5\nforward 2\nforward 3\nforward 5\nup 8\nup 1\ndown 9\nforward 2\ndown 4\ndown 9\ndown 6\ndown 5\ndown 8\nforward 3\nforward 8\nforward 7\nup 3\nup 5\ndown 9\ndown 5\nup 6\nforward 4\nforward 4\nforward 4\ndown 9\ndown 2\ndown 7\ndown 1\ndown 2\ndown 4\nforward 7\ndown 9\nforward 4\nforward 5\nup 5\nforward 4\nforward 9\nforward 1\nforward 5\ndown 3\nforward 1\nforward 5\nup 9\ndown 7\nforward 7\nforward 6\ndown 2\ndown 3\nforward 9\ndown 1\nforward 4\nforward 9\nup 7\nforward 7\ndown 5\nforward 9\nforward 2\nup 3\ndown 3\ndown 7\ndown 5\nup 7\nup 9\nup 7\nforward 3\nforward 3\nforward 8\nup 9\nforward 8\nforward 9\nforward 4\ndown 2\nforward 7\ndown 6\nup 3\nup 9\nforward 8\nforward 2\ndown 9\ndown 7\nforward 1\nup 4\nup 7\nforward 2\nup 4\nforward 4\nup 1\nforward 3\ndown 7\nforward 5\ndown 4\nforward 2\nforward 7\nup 4\ndown 1\ndown 6\nforward 1\nforward 9\nup 6\nforward 7\nforward 7\ndown 8\nforward 7\ndown 8\ndown 9\nup 3\nforward 3\nforward 3\ndown 8\nup 2\ndown 2\ndown 4\nup 3\ndown 3\nforward 7\ndown 4\nup 8\ndown 9\ndown 9\nup 7\ndown 1\nforward 2\nup 1\ndown 3\nup 9\ndown 6\nup 2\nforward 6\nup 8\nup 1\ndown 6\ndown 1\nup 6\nup 4\nup 2\nforward 6\ndown 6\ndown 1\nforward 7\nup 9\nup 1\nforward 4\nforward 5\nup 6\nforward 9\ndown 1\ndown 9\ndown 3\ndown 7\nforward 7\ndown 1\ndown 4\nforward 6\ndown 5\nup 4\nforward 9\nup 5\ndown 1\ndown 2\ndown 2\nup 4\nforward 1\nforward 3\ndown 7\nforward 4\ndown 4\ndown 8\ndown 5\nforward 3\nup 4\nforward 5\ndown 2\ndown 4\ndown 4\ndown 1\nforward 2\nforward 1\nforward 8\nforward 4\nup 4\ndown 9\nup 6\nforward 9\nup 5\ndown 5\nforward 3\nup 1\nforward 7\ndown 4\nforward 7\ndown 9\nup 8\ndown 5\nforward 1\ndown 5\ndown 8\nforward 3\nup 6\nforward 3\nup 7\nforward 6\nforward 9\nup 1\ndown 3\ndown 9\nup 4\nup 6\nforward 5\ndown 6\ndown 3\ndown 4\nup 1\nforward 5\ndown 5\ndown 2\nforward 6\ndown 8\ndown 3\nup 8\nforward 5\nforward 6\ndown 6\ndown 6\ndown 6\nforward 7\nup 4\nforward 7\nup 4\ndown 2\nforward 4\nforward 2\ndown 6\nup 1\ndown 1\ndown 4\nup 8\ndown 6\nforward 3\nforward 6\ndown 6\nforward 5\ndown 4\nup 2\nup 3\ndown 3\nup 1\nforward 2\nup 1\nforward 4\nup 5\nup 2\ndown 7\nforward 3\nup 2\nforward 5\ndown 1\ndown 3\ndown 2\nforward 5\ndown 1\nup 5\nforward 4\ndown 7\nup 8\nup 3\ndown 7\ndown 7\nforward 9\nforward 1\nup 6\ndown 4\ndown 7\nforward 1\ndown 4\nforward 9\nup 1\nforward 3\ndown 1\nup 3\ndown 6\ndown 8\ndown 6\nforward 6\nforward 6\nup 2\ndown 8\nforward 5")
  (time (part1 puzzle-input)) ; 1727835 (20 ms)
  )

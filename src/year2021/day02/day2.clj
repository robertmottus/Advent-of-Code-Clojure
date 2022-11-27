(ns year2021.day02.day2
  (:require [ysera.test :refer [is=]]
            [clojure.string :as str :only [index-of]]))

; https://adventofcode.com/2021/day/2


(def initial-position {:horizontal 0 :depth 0})
(defn update-position
  {:doc "forward X increases the horizontal position by X units.
         down X increases the depth by X units.
         up X decreases the depth by X units."
   :test (fn []
           (is= (update-position initial-position ["forward" 1]) {:horizontal 1 :depth 0})
           (is= (update-position initial-position ["up" 1]) {:horizontal 0 :depth -1})
           )}
  [position [action steps]]
  (condp = action
    "forward" (update position :horizontal + steps)
    "up" (update position :depth - steps)
    "down" (update position :depth + steps)
    ))


(def initial-position-aim {:horizontal 0 :depth 0 :aim 0})
(defn update-position-aim
  {:doc  "down X increases your aim by X units.
          up X decreases your aim by X units.
          forward X does two things:
            It increases your horizontal position by X units.
            It increases your depth by your aim multiplied by X."
   :test (fn []
           (is= (update-position-aim initial-position-aim ["forward" 1]) {:horizontal 1 :depth 0 :aim 0})
           (is= (update-position-aim initial-position-aim ["up" 1]) {:horizontal 0 :depth 0 :aim -1})
           (is= (update-position-aim {:horizontal 1 :depth 0 :aim 2} ["forward" 1]) {:horizontal 2 :depth 2 :aim 2})
           (is= (update-position-aim {:horizontal 1 :depth 0 :aim 2} ["up" 1]) {:horizontal 1 :depth 0 :aim 1})
           )}
  [state [action steps]]
  (condp = action
    "forward" (-> state
                  (update :horizontal + steps)
                  (update :depth + (* (:aim state) steps)))
    "up" (update state :aim - steps)
    "down" (update state :aim + steps)
    ))


(defn input->actions
  {:doc  "Converts input string to list of vectors with action and number"
   :test (fn []
           (is= (input->actions "forward 5\ndown 5\nforward 8\nup 3\ndown 8\nforward 2")
                '(["forward" 5] ["down" 5] ["forward" 8] ["up" 3] ["down" 8] ["forward" 2])))}
  [input]
  (->> input
       (str/split-lines)
       (map (fn [line] (str/split line #" ")))
       (map (fn [[action steps]] [action (read-string steps)]))
       )
  )


(defn part1
  {:test (fn []
           (is= (part1 "forward 5\ndown 5\nforward 8\nup 3\ndown 8\nforward 2") 150))}
  [input]
  (->> input
       (input->actions)
       (reduce update-position initial-position)
       (vals)
       (reduce *)
       )
  )

(defn part2
  {:test (fn []
           (is= (part2 "forward 5\ndown 5\nforward 8\nup 3\ndown 8\nforward 2") 900))}
  [input]
  (as-> input $
        (input->actions $)
        (reduce update-position-aim initial-position-aim $)
        (select-keys $ [:horizontal :depth])
        (vals $)
        (reduce * $)
        )
  )


(comment
  (def puzzle-input "forward 4\ndown 9\nforward 6\ndown 5\nup 2\nforward 5\nforward 7\nup 5\ndown 9\nup 6\ndown 6\ndown 1\ndown 1\nup 2\ndown 3\nup 3\nforward 8\nforward 7\ndown 6\ndown 7\nforward 6\nforward 9\nforward 7\nup 9\ndown 4\ndown 6\ndown 5\ndown 9\nforward 8\ndown 9\nforward 9\nforward 4\nforward 4\nup 3\nup 8\ndown 9\ndown 8\ndown 4\nforward 5\nforward 4\nup 6\nforward 6\nup 3\nup 8\nup 3\nup 4\ndown 3\ndown 5\ndown 5\nup 1\nforward 9\ndown 4\nforward 6\ndown 6\nup 2\nup 9\nforward 1\nforward 2\nforward 7\ndown 6\nup 6\nforward 1\nforward 7\ndown 7\nforward 9\nforward 4\nforward 6\ndown 5\nup 9\ndown 1\nup 5\nup 5\nup 9\ndown 5\nforward 7\ndown 1\nup 9\ndown 7\nforward 2\ndown 4\ndown 4\nforward 8\nforward 8\ndown 6\ndown 3\nup 7\ndown 3\nforward 9\ndown 7\nforward 2\ndown 1\nforward 5\nup 9\ndown 2\nup 2\ndown 3\nup 7\nforward 9\nforward 7\ndown 4\ndown 5\nup 3\ndown 3\ndown 5\nforward 9\ndown 3\nforward 9\ndown 3\nup 9\ndown 5\nforward 4\ndown 4\nup 8\nforward 7\nup 1\ndown 2\nforward 4\ndown 7\ndown 9\ndown 4\ndown 4\nforward 6\ndown 7\ndown 2\ndown 1\nforward 1\ndown 2\nforward 1\ndown 7\nforward 5\nup 3\nforward 6\nup 9\ndown 3\ndown 3\ndown 9\nforward 4\ndown 4\nforward 9\nforward 6\ndown 7\nup 9\nup 6\nforward 4\ndown 5\nforward 2\ndown 7\ndown 7\nforward 4\nforward 5\ndown 8\ndown 5\nup 4\nforward 7\nup 8\ndown 8\nforward 4\nforward 5\ndown 6\ndown 1\ndown 1\ndown 9\nforward 4\nup 1\ndown 8\nup 7\ndown 1\nup 2\nforward 4\ndown 7\ndown 7\ndown 2\nforward 7\ndown 2\nup 1\nup 4\ndown 6\nforward 5\nforward 2\nup 1\nforward 2\nforward 9\nup 9\nup 7\nforward 9\ndown 8\nup 5\ndown 6\ndown 6\nup 8\ndown 1\nforward 6\ndown 5\nforward 2\ndown 9\ndown 9\nup 4\nforward 4\nforward 2\nforward 7\nforward 3\ndown 1\nforward 8\nup 9\ndown 7\nforward 9\nforward 1\nforward 5\nup 6\ndown 6\nforward 6\nup 3\nforward 9\ndown 3\nforward 2\ndown 7\ndown 3\nup 9\ndown 2\ndown 3\nforward 5\ndown 9\nforward 8\ndown 2\nforward 1\ndown 9\ndown 7\nforward 2\nforward 6\nforward 4\nforward 5\ndown 5\ndown 1\nforward 5\nup 4\ndown 4\nup 8\ndown 4\nup 4\ndown 1\ndown 2\ndown 9\ndown 2\nup 4\ndown 1\nforward 2\nforward 1\nforward 9\ndown 5\nup 4\nup 1\nforward 8\nforward 6\nforward 9\nup 9\nforward 4\nforward 4\ndown 1\nforward 6\nforward 7\nforward 3\nup 5\nup 7\ndown 1\nforward 4\ndown 3\ndown 5\nup 7\ndown 4\nup 9\ndown 3\ndown 5\nforward 7\nforward 8\nup 5\nup 1\nforward 3\nup 8\nforward 3\ndown 2\nforward 1\nforward 9\nforward 1\ndown 2\nforward 7\ndown 5\nforward 6\ndown 9\nup 9\nforward 5\nforward 7\nforward 6\ndown 2\nup 2\nforward 3\nforward 4\nforward 3\ndown 5\nforward 1\nforward 2\nforward 6\ndown 4\nforward 2\nforward 6\nup 8\nforward 2\nup 4\nforward 7\ndown 2\nforward 1\nforward 7\ndown 6\nforward 4\ndown 3\ndown 2\ndown 2\nforward 4\ndown 8\nforward 6\nforward 6\ndown 2\nup 3\nup 1\nforward 1\ndown 5\ndown 2\nforward 4\nforward 7\nforward 3\ndown 3\nforward 9\ndown 1\ndown 7\nforward 6\nforward 1\nup 6\nforward 7\nforward 1\ndown 5\ndown 4\nforward 6\nup 1\ndown 1\nup 9\ndown 2\ndown 2\nforward 3\nup 4\ndown 5\ndown 5\ndown 3\ndown 6\nup 8\nforward 2\nforward 2\ndown 6\ndown 1\nup 4\nup 1\ndown 5\nup 4\nup 2\nforward 4\nforward 6\nforward 3\ndown 7\nforward 8\nup 5\nforward 5\ndown 1\nforward 2\nforward 6\ndown 8\nup 6\ndown 1\ndown 7\nforward 4\nforward 2\nup 1\ndown 6\nforward 3\nforward 1\nforward 5\nforward 9\nforward 9\ndown 4\nforward 2\ndown 1\nforward 1\nforward 7\nforward 5\ndown 9\ndown 8\ndown 1\ndown 6\ndown 1\nup 7\ndown 3\nforward 3\nup 6\nup 4\ndown 7\ndown 7\nforward 6\nup 7\ndown 7\nforward 9\ndown 9\ndown 3\nforward 6\nforward 9\nforward 1\ndown 4\nforward 5\ndown 4\ndown 2\ndown 3\nup 3\nforward 9\nforward 7\nforward 5\ndown 5\nforward 7\nup 4\ndown 1\nforward 3\ndown 3\nforward 4\ndown 9\nforward 2\ndown 5\ndown 1\nforward 8\ndown 3\nforward 7\nup 1\ndown 3\nforward 2\nup 8\ndown 2\nforward 4\nforward 4\nforward 4\ndown 5\nup 6\ndown 3\nforward 5\ndown 4\nup 5\nforward 1\nforward 6\nup 1\ndown 3\nforward 2\nforward 9\ndown 7\ndown 4\nforward 5\nup 3\nup 6\nup 1\nforward 4\nforward 1\nforward 1\ndown 7\nup 4\ndown 3\ndown 8\ndown 3\nforward 8\nforward 3\ndown 6\ndown 9\nforward 3\nforward 9\nforward 7\ndown 8\ndown 6\ndown 4\nforward 2\nup 4\nforward 8\ndown 1\nforward 9\nforward 1\ndown 9\nforward 2\ndown 7\ndown 2\nup 7\ndown 1\nup 8\nforward 8\ndown 7\nforward 1\ndown 1\nforward 3\nforward 1\nup 2\ndown 7\ndown 5\nforward 5\ndown 8\nforward 4\ndown 1\nup 2\nup 8\ndown 8\ndown 1\ndown 5\nup 3\nforward 3\nforward 5\ndown 2\nup 4\ndown 2\nforward 7\nforward 9\nup 9\nup 7\nforward 1\nup 4\nforward 3\nup 5\nforward 9\nforward 9\nforward 6\nforward 2\ndown 7\nforward 8\nforward 4\nforward 7\ndown 8\ndown 5\ndown 6\nforward 6\ndown 4\ndown 1\ndown 9\ndown 1\nforward 3\nforward 5\ndown 6\ndown 7\ndown 9\ndown 8\ndown 4\nup 5\nforward 7\ndown 9\nforward 6\ndown 7\nforward 5\ndown 5\nforward 1\ndown 5\ndown 3\nup 9\nup 3\nforward 2\nup 9\nforward 6\ndown 1\ndown 5\ndown 9\ndown 4\nup 6\nforward 9\ndown 4\ndown 9\ndown 5\ndown 8\ndown 5\ndown 4\nup 5\ndown 8\nup 8\nforward 5\ndown 9\nforward 2\nup 2\ndown 6\nforward 2\nforward 4\nforward 6\ndown 6\ndown 1\nforward 8\ndown 5\ndown 5\nforward 2\ndown 7\ndown 5\ndown 6\ndown 9\nforward 4\nup 9\ndown 3\ndown 7\nforward 3\ndown 5\nup 1\nforward 5\nup 2\ndown 2\nforward 2\nup 3\nup 6\nforward 2\nforward 7\ndown 8\nforward 8\nforward 7\nforward 6\ndown 5\ndown 6\ndown 6\ndown 9\nup 5\ndown 3\nup 1\nup 9\nup 5\ndown 4\ndown 4\ndown 8\nforward 8\nup 5\ndown 9\nforward 1\nup 1\nforward 2\ndown 9\nforward 5\nup 9\nforward 7\ndown 7\ndown 5\nup 1\nup 2\ndown 8\ndown 7\nup 4\nforward 9\ndown 4\nup 8\ndown 5\ndown 1\nforward 9\ndown 6\nup 8\ndown 6\nforward 7\nup 6\nup 5\nforward 2\nup 7\nforward 7\nforward 5\ndown 1\nforward 9\ndown 8\nforward 9\ndown 3\ndown 3\nforward 9\nup 1\ndown 2\nforward 9\ndown 7\nforward 4\nforward 3\nforward 4\ndown 5\nforward 9\nforward 9\ndown 5\nforward 4\ndown 5\ndown 2\ndown 6\nforward 5\nforward 8\nforward 6\nup 9\ndown 9\nforward 7\ndown 6\ndown 7\ndown 4\nforward 1\nforward 3\nforward 6\nforward 4\nforward 3\nforward 4\ndown 1\nforward 2\nforward 3\nforward 9\nup 8\nforward 6\ndown 1\nup 5\ndown 1\ndown 4\ndown 7\ndown 5\ndown 9\ndown 2\ndown 9\nforward 2\ndown 2\nup 5\nforward 2\nforward 3\nforward 5\nup 8\nup 1\ndown 9\nforward 2\ndown 4\ndown 9\ndown 6\ndown 5\ndown 8\nforward 3\nforward 8\nforward 7\nup 3\nup 5\ndown 9\ndown 5\nup 6\nforward 4\nforward 4\nforward 4\ndown 9\ndown 2\ndown 7\ndown 1\ndown 2\ndown 4\nforward 7\ndown 9\nforward 4\nforward 5\nup 5\nforward 4\nforward 9\nforward 1\nforward 5\ndown 3\nforward 1\nforward 5\nup 9\ndown 7\nforward 7\nforward 6\ndown 2\ndown 3\nforward 9\ndown 1\nforward 4\nforward 9\nup 7\nforward 7\ndown 5\nforward 9\nforward 2\nup 3\ndown 3\ndown 7\ndown 5\nup 7\nup 9\nup 7\nforward 3\nforward 3\nforward 8\nup 9\nforward 8\nforward 9\nforward 4\ndown 2\nforward 7\ndown 6\nup 3\nup 9\nforward 8\nforward 2\ndown 9\ndown 7\nforward 1\nup 4\nup 7\nforward 2\nup 4\nforward 4\nup 1\nforward 3\ndown 7\nforward 5\ndown 4\nforward 2\nforward 7\nup 4\ndown 1\ndown 6\nforward 1\nforward 9\nup 6\nforward 7\nforward 7\ndown 8\nforward 7\ndown 8\ndown 9\nup 3\nforward 3\nforward 3\ndown 8\nup 2\ndown 2\ndown 4\nup 3\ndown 3\nforward 7\ndown 4\nup 8\ndown 9\ndown 9\nup 7\ndown 1\nforward 2\nup 1\ndown 3\nup 9\ndown 6\nup 2\nforward 6\nup 8\nup 1\ndown 6\ndown 1\nup 6\nup 4\nup 2\nforward 6\ndown 6\ndown 1\nforward 7\nup 9\nup 1\nforward 4\nforward 5\nup 6\nforward 9\ndown 1\ndown 9\ndown 3\ndown 7\nforward 7\ndown 1\ndown 4\nforward 6\ndown 5\nup 4\nforward 9\nup 5\ndown 1\ndown 2\ndown 2\nup 4\nforward 1\nforward 3\ndown 7\nforward 4\ndown 4\ndown 8\ndown 5\nforward 3\nup 4\nforward 5\ndown 2\ndown 4\ndown 4\ndown 1\nforward 2\nforward 1\nforward 8\nforward 4\nup 4\ndown 9\nup 6\nforward 9\nup 5\ndown 5\nforward 3\nup 1\nforward 7\ndown 4\nforward 7\ndown 9\nup 8\ndown 5\nforward 1\ndown 5\ndown 8\nforward 3\nup 6\nforward 3\nup 7\nforward 6\nforward 9\nup 1\ndown 3\ndown 9\nup 4\nup 6\nforward 5\ndown 6\ndown 3\ndown 4\nup 1\nforward 5\ndown 5\ndown 2\nforward 6\ndown 8\ndown 3\nup 8\nforward 5\nforward 6\ndown 6\ndown 6\ndown 6\nforward 7\nup 4\nforward 7\nup 4\ndown 2\nforward 4\nforward 2\ndown 6\nup 1\ndown 1\ndown 4\nup 8\ndown 6\nforward 3\nforward 6\ndown 6\nforward 5\ndown 4\nup 2\nup 3\ndown 3\nup 1\nforward 2\nup 1\nforward 4\nup 5\nup 2\ndown 7\nforward 3\nup 2\nforward 5\ndown 1\ndown 3\ndown 2\nforward 5\ndown 1\nup 5\nforward 4\ndown 7\nup 8\nup 3\ndown 7\ndown 7\nforward 9\nforward 1\nup 6\ndown 4\ndown 7\nforward 1\ndown 4\nforward 9\nup 1\nforward 3\ndown 1\nup 3\ndown 6\ndown 8\ndown 6\nforward 6\nforward 6\nup 2\ndown 8\nforward 5")
  (time (part1 puzzle-input))                               ; 1727835 (10 ms)
  (time (part2 puzzle-input))                               ; => 1544000595 (5 ms)
  )

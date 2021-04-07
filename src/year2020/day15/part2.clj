(ns year2020.day15.part2
  (:require [ysera.test :refer [is is= is-not]]))

; https://adventofcode.com/2020/day/15

;In this game, the players take turns saying numbers. They begin by taking turns reading from a list of starting numbers (your puzzle input). Then, each turn consists of considering the most recently spoken number:
;If that was the first time the number has been spoken, the current player says 0.
;Otherwise, the number had been spoken before; the current player announces how many turns apart the number is from when it was previously spoken.
;So, after the starting numbers, each turn results in that player speaking aloud either 0 (if the last number is new) or an age (if the last number is a repeat).

(declare numbers->state)
(declare iterate-map)

(comment
  (time (get-number 2020 [1, 17, 0, 10, 18, 11, 6] ))    ; 595
  (time (get-number 30000000 [1, 17, 0, 10, 18, 11, 6])) ; 1708310
  (time (get-number 30000000 [14,8,16,0,1,17]))          ; 505
  )

(defn get-number
  {:doc "Returns number at 'pos', given 'init-numbers'"
   :test (fn []
           (is= (get-number 4 [1, 2, 3]) 0)
           ;(is= (get-number 30000000 [1, 17, 0, 10, 18, 11, 6]) 1708310)
           )}
  [pos init-numbers]
  (loop [{last-pos :last-pos last-num :last-num :as state} (numbers->state init-numbers)]
    (if (== last-pos pos)
      last-num
      (recur (iterate-map state))
      )
    )
  )

(defn numbers->state
  {:doc  "Initialzes position-map"
   :test (fn []
           (is= (numbers->state [5 6 7]) {:last-num 7 :last-pos 3 :pos-map {5 1 6 2}})
           (is= (numbers->state [1 2 2]) {:last-num 2 :last-pos 3 :pos-map {1 1 2 2}})
           )}
  [init-numbers]
  (let
    [numPos (into {} (map-indexed (fn [i n] [n (inc i)]) (drop-last init-numbers)))
     last-number (last init-numbers)
     last-pos (count init-numbers)
     ]
    {:last-num last-number :last-pos last-pos :pos-map numPos}
    ))

(defn iterate-map
  {:doc  "Adds one iteration to state"
   :test (fn []
           (is= (iterate-map (numbers->state [1 2 3])) (numbers->state [1 2 3 0]))
           (is= (iterate-map (numbers->state [1 2 3 0])) (numbers->state [1 2 3 0 0]))
           (is= (iterate-map (numbers->state [1 2 3 0 0])) (numbers->state [1 2 3 0 0 1]))
           )}
  [{num-pos :pos-map last-number :last-num last-pos :last-pos}]
   {:last-num (if-let [prev-pos (num-pos last-number)] (- last-pos prev-pos) 0)
    :last-pos (inc last-pos)
    :pos-map  (assoc num-pos last-number last-pos)}
   )


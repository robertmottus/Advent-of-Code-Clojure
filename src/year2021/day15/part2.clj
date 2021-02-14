(ns year2021.day15.part2
  (:require [ysera.test :refer [is is= is-not]]))

(declare init-state)
(declare iterate-map)

(defn get-number
  {:test (fn []
           (is= (get-number 4 [1, 2, 3]) 0)
           (is= (get-number 30000000 [1, 17, 0, 10, 18, 11, 6]) 1708310)
           )}
  [pos init-numbers]
  (let
    [state (init-state init-numbers)
     ;old (into {} (map-indexed (fn [i n] [n (inc i)]) (drop-last init-numbers)))
     ]
    (loop [s state]
      (if (== (:last-pos s) pos)
        (:last-num s)
        (recur (iterate-map s))
        )
      )
    )
  )

(defn init-state
  {:doc  "Initialzes position-map"
   :test (fn []
           (is= (init-state [5 6 7]) {:last-num 7 :last-pos 3 :pos-map {5 1 6 2}})
           (is= (init-state [1 2 2]) {:last-num 2 :last-pos 3 :pos-map {1 1 2 2}})
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
  {:doc  "Initialzes position-map"
   :test (fn []
           (is= (iterate-map {:last-num 0 :last-pos 4 :pos-map {1 1 2 2 3 3}})
                {:last-num 0 :last-pos 5 :pos-map {1 1 2 2 3 3 0 4}})
           )}
  [state]
  (let
    [num-pos (:pos-map state)
     last-number (:last-num state)
     last-pos (:last-pos state)
     new-number (if (contains? num-pos last-number) (- last-pos (num-pos last-number)) 0)
     new-last-pos (inc last-pos)
     new-num-pos (assoc num-pos last-number last-pos)
     new-state {:last-num new-number :last-pos new-last-pos :pos-map new-num-pos}
     ]
    ;(println "old:" state)
    ;(println "new:" new-state)
    new-state
    ))

;(defn init-pos
;  {"Initialzes position-map"
;   :test (fn []
;           (is= (init-pos [5 6 7]) {last-num 7 last-pos 3 pos-map {5 1 6 2}})
;           (is= (init-pos [5 6 6]) {5 1 6 2 7 3})
;           )}
;  [init-numbers]
;  (let
;    [numPos (into {} (map-indexed (fn [i n] [n (inc i)]) (drop-last init-numbers)))
;     last-number (last init-numbers)
;     last-pos (count init-numbers)]
;    (if (contains? numPos last-number)
;      (- last-pos (numPos last-number))
;      0
;      )
;    ))


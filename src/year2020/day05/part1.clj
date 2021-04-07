(ns year2020.day05.part1
  (:require [ysera.test :refer [is is= is-not]]
            [clojure.math.numeric-tower :as math :refer [expt]]
            [clojure.string :as str :only [index-of]]))

; https://adventofcode.com/2020/day/5

(defn string->position
  {:doc "
  Calculates a position coded as successive choices of first or second half.
  Second arg specifies the chars for first and second half respectively."
   :test (fn []
           (is= (string->position "00" "01") 0)
           (is= (string->position "01" "01") 1)
           (is= (string->position "10" "01") 2)
           (is= (string->position "11" "01") 3)
           (is= (string->position "BFFFBBF" "FB") 70)
           )}
  [pos-string, codes]
  (->> pos-string
       (reverse)
       (map-indexed
         (fn [idx ch]
           (let [pos-factor (math/expt 2 idx)
                 low-high-factor (str/index-of codes ch)]
             (* pos-factor low-high-factor)
             )))
       (reduce +)
       ))

(defn string->position2
  {:doc "
  Calculates a position coded as successive choices of first or second half.
  Second arg specifies the chars for first and second half respectively."
   :test (fn []
           (is= (string->position2 "00" "01") 0)
           (is= (string->position2 "01" "01") 1)
           (is= (string->position2 "10" "01") 2)
           (is= (string->position2 "11" "01") 3)
           (is= (string->position2 "BFFFBBF" "FB") 70)
           )}
  [pos-string, codes]
  (->> pos-string
       (replace {(get codes 0) 0 (get codes 1) 1})
       (apply str)
       (str "2r")
       (read-string)
       ))


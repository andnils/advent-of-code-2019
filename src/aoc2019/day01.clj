(ns aoc2019.day01
  (:require [aoc2019.utils :refer [read-file sum]]))


(def masses
  (read-file "input01.txt"
             :parse-lines-with Long/parseLong))


(defn fuel-requirements [mass]
  (- (quot mass 3) 2))


(defn total-fuel-requirements [mass]
  (loop [total 0
         fuel (fuel-requirements mass)]
    (if (pos? fuel)
      (recur (+ total fuel) (fuel-requirements fuel))
      total)))


(def part-1-solution
  (sum (map fuel-requirements masses)))
;; => 3336439

(def part-2-solution
  (sum (map total-fuel-requirements masses)))
;; => 5001791  





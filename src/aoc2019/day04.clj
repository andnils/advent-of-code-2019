(ns aoc2019.day04
  (:require [aoc2019.utils :refer [->int]]))


;; Rules

(defn correct-length [pwd]
  (= (count pwd) 6))


(defn correct-range [pwd]
  (let [numpwd (->int pwd)]
    (< 123257 numpwd 647015)))


(defn has-double [pwd]
  (or (= (get pwd 0) (get pwd 1))
      (= (get pwd 1) (get pwd 2))
      (= (get pwd 2) (get pwd 3))
      (= (get pwd 3) (get pwd 4))
      (= (get pwd 4) (get pwd 5))))


(defn dont-repeat-too-much [pwd]
  (some (fn [[_ f]] (= f 2))
        (frequencies pwd)))


(defn never-decrease [pwd]
  (let [digits (map ->int pwd)]
    (apply <= digits)))


;; Main program

(defn count-valid [rules]
  (->> (range 123257 647015)
       (filter (comp rules str))
       (count)))


(def part-1-rules
  [correct-length
   correct-range
   has-double
   never-decrease])


(def part-2-rules
  (conj part-1-rules dont-repeat-too-much))



(comment

  (count-valid (apply every-pred part-1-rules))
  

  (count-valid (apply every-pred part-2-rules))

  )







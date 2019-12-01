(ns aoc2019.day01
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))



(def input (->> (io/resource "input01.txt")
                (slurp)
                (s/split-lines)
                (map #(Long/parseLong %))))



(defn fuel-requirements [mass]
  (- (quot mass 3) 2))

(defn solve-part-1 []
  (reduce + (map fuel-requirements input)))


;; ----------------

(defn fuel-requirements-2 [mass]
  (loop [acc 0
         add (fuel-requirements mass)]
    (if (pos? add)
      (recur (+ acc add) (fuel-requirements add))
      acc)))


(defn solve-part-2 []
  (reduce + (map fuel-requirements-2 input)))



(comment

  (solve-part-1)
  ;; => 3336439
  
  (solve-part-2)
;; => 5001791  

  )



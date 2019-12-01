(ns aoc2019.utils)


(def sum
  "Sum of numbers"
  (partial reduce +))


(defmacro read-file [filename _ option2]
  `(->> (clojure.java.io/resource ~filename)
        (slurp)
        (clojure.string/split-lines)
        (map #(~option2 %))))

(ns aoc2019.utils
  (:require [clojure.java.io]
            [clojure.edn]))


(def sum
  "Sum of numbers"
  (partial reduce +))

(defn read-edn-string [filename]
  (-> filename
      (clojure.java.io/resource)
      (slurp)
      (clojure.edn/read-string)))

(defmacro read-file [filename _ option2]
  `(->> (clojure.java.io/resource ~filename)
        (slurp)
        (clojure.string/split-lines)
        (map #(~option2 %))))

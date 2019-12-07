(ns aoc2019.day06
  (:require [aoc2019.utils :refer [read-file]]
            [com.stuartsierra.dependency :as dep]
            [clojure.string :as str]
            [clojure.set :as s]))


(def RGX (re-pattern #"\)"))

(defn line-parser [s]
  (str/split s RGX))

(def input
  (delay
    (read-file "input06.txt"
               :parse-lines-with line-parser)))

(def example-input
  (delay
    (read-file "input06-example.txt"
               :parse-lines-with line-parser)))

(def example-input2
  (delay
    (read-file "input06-example2.txt"
               :parse-lines-with line-parser)))


;; Part 1

(defn num-orbits [orbit-graph node]
  (count (dep/transitive-dependents orbit-graph node)))

(defn nodes [orbit-graph]
  (dep/nodes orbit-graph))

(defn solve-part-1 [orbit-graph]
  (reduce + (map num-orbits (repeat orbit-graph) (nodes orbit-graph))))


;; Part 2

(defn make-orbit-graph [input]
  (reduce (fn [graph [parent child]] (dep/depend graph parent child))
          (dep/graph)
          input))



(defn path-to-center [graph start-node]
  (loop [visited []
         node start-node]
    (let [parent (first (dep/immediate-dependents graph node))]
      (if (= parent "COM")
        (conj visited parent)
        (recur (conj visited parent) parent)))))


(defn minimum-orbital-transfers [path1 path2]
  (let [s1 (into #{} path1)
        s2 (into #{} path2)]
    (+ (count (s/difference s1 s2))
       (count (s/difference s2 s1)))))

(defn solve-part-2 [graph node1 node2]
  (minimum-orbital-transfers
   (path-to-center graph node1)
   (path-to-center graph node2)))


(comment

  (solve-part-1 (make-orbit-graph @example-input))

  
  (solve-part-1 (make-orbit-graph @input))


  (solve-part-2  (make-orbit-graph @example-input2) "YOU" "SAN")
  
  (solve-part-2  (make-orbit-graph @input) "SAN" "YOU")

  
  )

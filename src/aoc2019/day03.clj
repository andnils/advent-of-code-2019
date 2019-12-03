(ns aoc2019.day03
  (:require [clojure.set :as set]
            [clojure.string :as str]
            [aoc2019.utils :refer [abs read-file]]))

(set! *warn-on-reflection* true)


(defn manhattan-distance
  ([coord]
   (manhattan-distance [0 0] coord))
  ([[x1 y1] [x2 y2]]
   (+ (abs (- x2 x1))
      (abs (- y2 y1)))))


(def INSTRUCTION (re-pattern  #"([RLUD])(\d+)"))

(defn string->instruction [s]
  (let [[_ a b] (re-matches INSTRUCTION s)]
    [(keyword a) (Long/parseLong b)]))


(defn line->instructions [line]
  (->> (str/split line #",")
       (map string->instruction)))


(def wires
  (delay (read-file "input03.txt"
                    :parse-lines-with line->instructions)))



(defn instruction->coords [[x y] [direction steps]]
  (case direction
    :R (for [dx (range steps)] [(+ x dx 1) y])
    :L (for [dx (range steps)] [(- x dx 1) y])
    :U (for [dy (range steps)] [x (+ y dy 1)])
    :D (for [dy (range steps)] [x (- y dy 1)])
    [x y]))

(defn process-instruction [{:keys [pos acc]} instruction]
  (let [coords (instruction->coords pos instruction)]
    {:pos (last coords)
     :acc (into acc coords)}))

(defn wire-path [wire]
  (let [init-val  {:pos [0 0] :acc []}]
    (:acc (reduce process-instruction init-val wire))))


(defn find-intersections [wire-1 wire-2]
  (set/intersection
   (into #{} (wire-path wire-1))
   (into #{} (wire-path wire-2))))


(defn steps-to-position [^clojure.lang.PersistentVector path position]
  (inc (.indexOf path position)))


(defn solve-1 []
  (let [wire-1 (first wires)
        wire-2 (second wires)
        intersections (find-intersections wire-1 wire-2)]
    (apply min (map manhattan-distance intersections))))



(defn solve-2 []
  (let [wire-1 (first wires)
        wire-2 (second wires)
        path-1 (wire-path wire-1)
        path-2 (wire-path wire-2)
        intersections (find-intersections wire-1 wire-2)]
    (apply min
           (map #(+ (steps-to-position path-1 %)
                    (steps-to-position path-2 %))
                intersections))))



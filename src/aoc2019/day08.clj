(ns aoc2019.day08
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def IMAGE-WIDTH 25)
(def IMAGE-HEIGHT 6)
(def LAYER-SIZE (* IMAGE-WIDTH IMAGE-HEIGHT))


(defn read-input []
  (slurp (io/resource "input08.txt")))

(defn get-layers [input]
  (partition LAYER-SIZE input))

(defn count-digit [digit layer]
  (count (filter #{digit} layer)))


(defn find-fewest-zeroes [layers]
  (reduce
   (fn [acc layer]
     (if (< (count-digit \0 acc) (count-digit \0 layer))
       acc
       layer))
   layers))

(defn solve-part-1 []
  (let [layers (get-layers (read-input))
        layer (find-fewest-zeroes layers)]
    (* (count-digit \1 layer) (count-digit \2 layer))))


(defn remove-transparents [& pixels]
  (first (drop-while #(= \2 %) pixels)))

(defn process-pixels [layers]
  (apply map remove-transparents layers))

(defn solve-part-2 []
  (let [layers (get-layers (read-input))
        image-data (process-pixels layers)]
    (doseq [row (partition IMAGE-WIDTH image-data)]
      (println (-> (apply str row)
                   (str/replace #"0" " ")
                   (str/replace #"1" "*"))))))


(comment

  (solve-part-1)
;; => 1965  

  (solve-part-2)
  ;; GZKJY
  
  
  )

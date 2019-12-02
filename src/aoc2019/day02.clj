(ns aoc2019.day02
  (:import [java.util Arrays])
  (:require [aoc2019.utils :refer [read-edn-string]]))

(set! *warn-on-reflection* true)



;; Memory setup

(def initial-memory (delay (read-edn-string "input02.txt")))

(defn setup-memory [initial-memory a b]
  (-> initial-memory
      (assoc 1 a)
      (assoc 2 b)
      (int-array)))


;; Accessor functions for instructions

(defn get-opcode [memory pointer]
  (get memory pointer))

(defn get-params [memory pointer]
  ;;;; a bit ugly, but it seems to be the fastest way to get a slice of an array
  (vec (Arrays/copyOfRange ^ints memory ^long (+ 1 pointer) ^long (+ 4 pointer))))

(defn get-result [memory]
  (get memory 0))


;; Implementation of instructions

(defn add! [memory [param1 param2 out]]
  (aset-int memory out
            (+ (get memory param1)
               (get memory param2))))

(defn mult! [memory [param1 param2 out]]
  (aset-int memory out
            (* (get memory param1)
               (get memory param2))))



;; Main program

(defn run-instruction! [memory op-code params]
  (case (int op-code)
    1 (add! memory params)
    2 (mult! memory params)
    :no-op))

(defn run-step
  ([memory]
   (run-step memory 0))
  ([memory pointer]
   (let [op-code (get-opcode memory pointer)
         params (get-params memory pointer)
         next-pointer (+ pointer 4)]
     (when (not= op-code 99)
       (run-instruction! memory op-code params)
       next-pointer))))

(defn run-loop [memory]
  (loop [next-pointer (run-step memory)]
    (if next-pointer
      (recur (run-step memory next-pointer))
      (get memory 0))))




;; Part 2

(def MAGIC-NUMBER  19690720)


(defn answer [noun verb]
  (+ (* 100 noun) verb))

(defn calculate [initial-memory promise noun verb]
  (let [result (run-loop (setup-memory initial-memory noun verb))]
    (when (= result MAGIC-NUMBER)
      (deliver promise (answer noun verb)))))

;; ========

(defn part-1-solution []
  (run-loop (setup-memory @initial-memory 12 2)))


(defn part-2-solution []
  (let [answer (promise)
        memory @initial-memory]
    (doseq [noun (range 100)
            verb (range 100)]
      (future (calculate memory answer noun verb)))
    (deref answer)))

;; =======

(comment

  (part-1-solution)
  ;; => 5482655

  (part-2-solution)
;; => 4967  

  )





(ns aoc2019.day05
  (:import [java.util Arrays])
  (:require [aoc2019.utils :refer [read-edn-string]]))

(set! *warn-on-reflection* true)



;; Memory setup

(def initial-memory (delay (read-edn-string "input05.txt")))

(take 10 @initial-memory)

(defn setup-memory
  ([initial-memory]
   (int-array initial-memory))
  ([initial-memory a b]
   (-> initial-memory
       (assoc 1 a)
       (assoc 2 b)
       (int-array))))


(defn get-params
  "Extract n values from memory.
  E.g. if n is 3 and pointer is 0, you'll get the values
  from meory index 1, 2 and 3.
  If n is 2 and pointer is 15, you'll get indexes 16 and 17."
  [n memory pointer]
  ;;;; a bit ugly, but it seems to be the fastest way to get a slice of an array
  (vec (Arrays/copyOfRange ^ints memory ^long (+ 1 pointer) ^long (+ 1 n pointer))))



(defn get-result [memory]
  (get memory 0))

(defn modal-get [mode memory param]
  (case mode
    :position-mode (get memory param)
    :immediate-mode param))



(defprotocol Instruction
  (go! [this memory])
  (halt? [this])
  (advance-pointer [this]))

(defrecord AddInstruction [params modes]
  Instruction
  (go! [this memory]
    (let [p1 (get params 0)
          m1 (nth modes 2)
          p2 (get params 1)
          m2 (nth modes 1)
          out (get params 2)]
      (aset-int memory out
                (+ (modal-get m1 memory p1)
                   (modal-get m2 memory p2)))))
  (halt? [this] false)
  (advance-pointer [this] (fn [ptr] (+ 4 ptr))))

(defrecord MultInstruction [params modes]
  Instruction
  (go! [this memory]
    (let [p1 (get params 0)
          m1 (nth modes 2)
          p2 (get params 1)
          m2 (nth modes 1)
          out (get params 2)]
      (aset-int memory out
                (* (modal-get m1 memory p1)
                   (modal-get m2 memory p2)))))
  (halt? [this] false)
  (advance-pointer [this] (fn [ptr] (+ 4 ptr))))



(defrecord InputInstruction [params modes]
  Instruction
  (go! [this memory]
    (let [param (get params 0)
          input 1]  ;; TODO: hard-coded input?!? is it always 1?!?!
      (aset-int memory param input)))
  (halt? [this] false)
  (advance-pointer [this] (fn [ptr] (+ 2 ptr))))

(defrecord OutputInstruction [params modes]
  Instruction
  (go! [this memory]
    (let [param (get params 0)]
      (println "Output: " (get memory param))))
  (halt? [this] false)
  (advance-pointer [this] (fn [ptr] (+ 2 ptr))))

(defrecord HaltInstruction []
  Instruction
  (go! [this memory])
  (halt? [this] true)
  (advance-pointer [this] (fn [ptr] (+ 1 ptr))))



(defrecord InstructionType [opcode modes])

(defn parse-mode [mode]
  (case mode
    \0 :position-mode
    \1 :immediate-mode))

(defn parse-instruction [i]
  (let [s (format "%05d" i)
        opcode (Integer/parseInt (.substring s 3 5))
        modes (mapv parse-mode (.substring s 0 3))]
    (->InstructionType opcode modes)))



(defmulti make-instruction
  (fn [memory pointer]
    (-> (get memory pointer)
        (parse-instruction)
        :opcode)))

(defmethod make-instruction 1
  [memory pointer]
  (->AddInstruction (get-params 3 memory pointer)
                    (:modes (parse-instruction (get memory pointer)))))

(defmethod make-instruction 2
  [memory pointer]
  (->MultInstruction (get-params 3 memory pointer)
                     (:modes (parse-instruction (get memory pointer)))))

(defmethod make-instruction 3
  [memory pointer]
  (->InputInstruction (get-params 1 memory pointer)
                      (:modes (parse-instruction (get memory pointer)))))

(defmethod make-instruction 4
  [memory pointer]
  (->OutputInstruction (get-params 1 memory pointer)
                       (:modes (parse-instruction (get memory pointer)))))

(defmethod make-instruction 99
  [_ _]
  (->HaltInstruction))


(defn run-step
  ([memory]
   (run-step memory 0))
  ([memory pointer]
   (let [instruction (make-instruction memory pointer)]
     (when (not (halt? instruction))
       (go! instruction memory)
       ((advance-pointer instruction) pointer)))))

(defn run-loop [memory]
  (loop [next-pointer (run-step memory)]
    (if next-pointer
      (recur (run-step memory next-pointer))
      (get memory 0))))





(defn part-1-solution []
  (run-loop (setup-memory @initial-memory)))



;; =======

(comment

  (run-loop (setup-memory @initial-memory))


  )





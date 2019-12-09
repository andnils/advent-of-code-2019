(ns aoc2019.day05
  (:import [java.util Arrays])
  (:require [aoc2019.utils :refer [read-edn-string]]))

(set! *warn-on-reflection* true)

(def ^:dynamic *memory*)
(def ^:dynamic *input-instruction* 1)
(def OUTPUT (atom nil))


;; Memory setup

(def initial-memory (delay (read-edn-string "input05.txt")))


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
  [n pointer]
  ;;;; a bit ugly, but it seems to be the fastest way to get a slice of an array
  (vec (Arrays/copyOfRange ^ints *memory* ^long (+ 1 pointer) ^long (+ 1 n pointer))))



(defn get-result []
  (get *memory* 0))

(defn modal-get [mode param]
  (case mode
    :position-mode (get *memory* param)
    :immediate-mode param))



(defprotocol Instruction
  (go! [this])
  (advance-pointer [this]))


(defrecord AddInstruction [params modes]
  Instruction
  (go! [this]
    (let [[p1 p2 p3] params
          [_ m2 m1] modes
          val1 (modal-get m1 p1)
          val2 (modal-get m2 p2)]
      (aset-int *memory* p3 (+ val1 val2))))
  (advance-pointer [this]
    (partial + 4)))


(defrecord MultInstruction [params modes]
  Instruction
  (go! [this]
    (let [[p1 p2 p3] params
          [_ m2 m1] modes
          val1 (modal-get m1 p1)
          val2 (modal-get m2 p2)]
      (aset-int *memory* p3 (* val1 val2))))
  (advance-pointer [this]
    (partial + 4)))


(defrecord InputInstruction [params modes]
  Instruction
  (go! [this]
    (let [[param] params]
      (aset-int *memory* param *input-instruction*)))
  (advance-pointer [this]
    (partial + 2)))


(defrecord OutputInstruction [params modes]
  Instruction
  (go! [this]
    (let [param (get params 0)]
      (reset! OUTPUT  (get *memory* param))))
  (advance-pointer [this]
    (partial + 2)))


(defrecord JumpIfTrueInstruction [params modes]
  Instruction
  (go! [this])
  (advance-pointer [this]
    (let [[p1 p2] params
          [_ m2 m1] modes]
      (if (zero? (modal-get m1 p1))
        (partial + 3)
        (constantly (modal-get m2 p2))))))


(defrecord JumpIfFalseInstruction [params modes]
  Instruction
  (go! [this])
  (advance-pointer [this]
    (let [[p1 p2] params
          [_ m2 m1] modes]
      (if (zero? (modal-get m1 p1))
        (constantly (modal-get m2 p2))
        (partial + 3)))))

(defrecord LessThanInstruction [params modes]
  Instruction
  (go! [this]
    (let [[p1 p2 p3] params
          [_ m2 m1] modes
          val1 (modal-get m1 p1)
          val2 (modal-get m2 p2)
          output (if (< val1 val2) 1 0)]
      (aset-int *memory* p3 output)))
  (advance-pointer [this]
    (partial + 4)))


(defrecord EqualInstruction [params modes]
  Instruction
  (go! [this]
    (let [[p1 p2 p3] params
          [_ m2 m1] modes
          val1 (modal-get m1 p1)
          val2 (modal-get m2 p2)
          output (if (= val1 val2) 1 0)]
      (aset-int *memory* p3 output)))
  (advance-pointer [this]
    (partial + 4)))


(defrecord HaltInstruction []
  Instruction
  (go! [this])
  (advance-pointer [this]
    inc))



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
  (fn [pointer]
    (-> (get *memory* pointer)
        (parse-instruction)
        :opcode)))

(defmethod make-instruction 1
  [pointer]
  (->AddInstruction (get-params 3 pointer)
                    (:modes (parse-instruction (get *memory* pointer)))))

(defmethod make-instruction 2
  [pointer]
  (->MultInstruction (get-params 3 pointer)
                     (:modes (parse-instruction (get *memory* pointer)))))

(defmethod make-instruction 3
  [pointer]
  (->InputInstruction (get-params 1 pointer)
                      (:modes (parse-instruction (get *memory* pointer)))))

(defmethod make-instruction 4
  [pointer]
  (->OutputInstruction (get-params 1 pointer)
                       (:modes (parse-instruction (get *memory* pointer)))))

(defmethod make-instruction 5
  [pointer]
  (->JumpIfTrueInstruction (get-params 2 pointer)
                           (:modes (parse-instruction (get *memory* pointer)))))

(defmethod make-instruction 6
  [pointer]
  (->JumpIfFalseInstruction (get-params 2 pointer)
                            (:modes (parse-instruction (get *memory* pointer)))))

(defmethod make-instruction 7
  [pointer]
  (->LessThanInstruction (get-params 3 pointer)
                         (:modes (parse-instruction (get *memory* pointer)))))

(defmethod make-instruction 8
  [pointer]
  (->EqualInstruction (get-params 3 pointer)
                      (:modes (parse-instruction (get *memory* pointer)))))

(defmethod make-instruction 99
  [_]
  (->HaltInstruction))


(defn halt? [instruction]
  (instance? HaltInstruction instruction))


(defn print-memory [pointer]
  (println (mapv #(format "% 4d" %) (into [] *memory*)))
  (println (format (str "% " (+ 6 (* pointer 5)) "d") pointer)))




(defn run-step
  ([]
   (run-step 0))
  ([pointer]
   (let [instruction (make-instruction pointer)]
     (when (not (halt? instruction))
       (go! instruction)
       (let [next-pointer-position ((advance-pointer instruction) pointer)]
         ;;(print-memory next-pointer-position)
         next-pointer-position)))))

(defn run-loop []
  (loop [next-pointer (run-step)]
    (if next-pointer
      (recur (run-step next-pointer))
      (let [output @OUTPUT]
        (println "Output " output)
        output))))





(defn part-1-solution []
  (binding [*input-instruction* 1
            *memory* (setup-memory @initial-memory)]
    (run-loop)))

(defn part-2-solution []
  (binding [*input-instruction* 5
            *memory* (setup-memory @initial-memory) ]
    (run-loop)))

;; =======

(comment

  (part-1-solution)

  (part-2-solution)


  ;; test 1
  (binding [*input-instruction* 8
            *memory* (setup-memory [3,9,8,9,10,9,4,9,99,-1,8]) ]
    (run-loop))

  ;; test 2
  (binding [*input-instruction* 7
            *memory* (setup-memory [3,9,7,9,10,9,4,9,99,-1,8]) ]
    (run-loop))

  ;; test 3
  (binding [*input-instruction* 8
            *memory* (setup-memory [3,3,1108,-1,8,3,4,3,99]) ]
    (run-loop))

  ;; test 4
  (binding [*input-instruction* 7
            *memory* (setup-memory [3,3,1107,-1,8,3,4,3,99]) ]
    (run-loop))

  ;; test 5
  (binding [*input-instruction* 7
            *memory* (setup-memory [3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9]) ]
    (run-loop))


  )





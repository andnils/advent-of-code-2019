(ns aoc2019.day02
  (:import [java.util Arrays])
  (:require [aoc2019.utils :refer [read-edn-string]]))

(set! *warn-on-reflection* true)



;; Memory setup

(def initial-memory (delay (read-edn-string "input02.txt")))

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

(defprotocol Instruction
  (go! [this memory])
  (halt? [this])
  (op-code [this])
  (parameters [this]))

(defrecord AddInstruction [params]
  Instruction
  (go! [this memory]
    (let [p1 (get params 0)
          p2 (get params 1)
          out (get params 2)]
      (aset-int memory out
                (+ (get memory p1)
                   (get memory p2)))))
  (halt? [this] false)
  (op-code [this] 1)
  (parameters [this] params))

(defrecord MultInstruction [params]
  Instruction
  (go! [this memory]
    (let [p1 (get params 0)
          p2 (get params 1)
          out (get params 2)]
      (aset-int memory out
                (* (get memory p1)
                   (get memory p2)))))
  (halt? [this] false)
  (op-code [this] 2)
  (parameters [this] params))

(defrecord HaltInstruction []
  Instruction
  (go! [this memory])
  (halt? [this] true)
  (op-code [this] 99)
  (parameters [this] []))




(defmulti make-instruction
  (fn [memory pointer]
    (get memory pointer)))

(defmethod make-instruction 1
  [memory pointer]
  (->AddInstruction (get-params 3 memory pointer)))

(defmethod make-instruction 2
  [memory pointer]
  (->MultInstruction (get-params 3 memory pointer)))

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
       (+ pointer (inc (count (parameters instruction))))))))

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





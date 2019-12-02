(ns aoc2019.day02-test
  (:require [expectations.clojure.test :refer [defexpect expect]]
            [aoc2019.day02 :as sut]))




(defn- run-step
  "Takes care of converting the
  memory data structure to/from
  int-array and vector"
  [memory]
  (let [mem  (int-array memory)]
    (sut/run-step mem)
    (into [] mem)))

(defn- run-loop
  "Takes care of converting the
  memory data structure to/from
  int-array and vector"
  [memory]
  (let [mem  (int-array memory)]
    (sut/run-loop mem)
    (into [] mem)))


(defexpect single-step
  (expect
   (run-step [1,9,10,3,2,3,11,0,99,30,40,50])
   [1,9,10,70,2,3,11,0,99,30,40,50]))

(defexpect small-programs
  
  (expect
   (run-loop [1,0,0,0,99])
   [2,0,0,0,99])

  (expect
   (run-loop [2,3,0,3,99])
   [2,3,0,6,99])

  (expect
   (run-loop [2,4,4,5,99,0])
   [2,4,4,5,99,9801])

  (expect
   (run-loop [1,1,1,4,99,5,6,0,99])
   [30,1,1,4,2,5,6,0,99])
  
  )

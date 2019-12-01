(ns aoc2019.day01-test
  (:require [expectations.clojure.test :refer [defexpect expect]]
            [aoc2019.day01 :as sut]))


(defexpect fuel-requirements-part-1
  (expect (sut/fuel-requirements 12) 2)
  (expect (sut/fuel-requirements 14) 2)
  (expect (sut/fuel-requirements 1969) 654)
  (expect (sut/fuel-requirements 100756) 33583))


(defexpect fuel-requirements-part-2
  (expect (sut/total-fuel-requirements 1969) 966)
  (expect (sut/total-fuel-requirements 100756) 50346))


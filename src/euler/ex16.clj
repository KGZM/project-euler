(ns euler.ex16
  {:author "KGZM"}
  (:require [clojure.test :refer :all]
            [clojure.math.numeric-tower :as math]))


;; 2^15 = 32768 and the sum of its digits is 3 + 2 + 7 + 6 + 8 = 26.
;; What is the sum of the digits of the number 2^1000?

(defn parse-int [s]
  (Integer/parseInt s))

(defn digits [n]
  (map (comp parse-int str) (str n)))

(defn sum-digits [n]
  (reduce + 0 (digits n)))


(deftest given-solution
  (is (= 26 (sum-digits (math/expt 2 15)))))


(comment 
  (sum-digits (math/expt 2 1000)) ;;=> 1366
  )

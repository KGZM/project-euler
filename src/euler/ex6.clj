(ns euler.ex6 
  (:require [clojure.test :refer :all]))

;; The sum of the squares of the first ten natural numbers is,
;; 12^2 + 22^2 + ... + 102^2 = 385

;; The square of the sum of the first ten natural numbers is,
;; (1 + 2 + ... + 10)^2 = 552 = 3025
;; Hence the difference between the sum of the squares of the first ten natural numbers and the square of the sum is 3025 − 385 = 2640.

;; Find the difference between the sum of the squares of the first one hundred natural numbers and the square of the sum.

(defn square [n] (* n n))

(defn sum-of-squares [n]
  (->> (range 1 (inc n)) 
       (map square)
       (reduce +)))

(defn square-of-sum [n]
  (->> (range 1 (inc n))
       (reduce +)
       (square)))

(defn difference-between-sum-of-squares-and-square-of-sum
  [n]
  (- (square-of-sum n)
     (sum-of-squares n)))


(deftest given-solution 
  (is (= 2640)
      (difference-between-sum-of-squares-and-square-of-sum 10)))

(comment 
  (difference-between-sum-of-squares-and-square-of-sum 100)
  ;;=> 25164150
  )

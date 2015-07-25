(ns euler.ex1
  (:require [clojure.test :refer :all]))

;; Problem 1
;; If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.  
;; Find the sum of all the multiples of 3 or 5 below 1000.

(defn is-multiple [s o] (zero? (mod s o)))

(defn sum-multiples-below [n]
  (->> (range n)
       (filter (some-fn #(is-multiple % 3)
                        #(is-multiple % 5))) 
       (reduce +)))


(deftest given-solution
  (is (= 23 
         (sum-multiples-below 10))))
(comment 
  (sum-multiples-below 1000) ;233168
  )

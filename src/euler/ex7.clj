(ns euler.ex7
  (:require [euler.ex3 :refer [lazy-prime-factors]]
            [clojure.test :refer :all]))

;; By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that the 6th prime is 13.
;; What is the 10 001st prime number?

(defn nth-prime [n]
  (->> (range)
       (filter odd?)
       (filter (comp (partial = 1)
                     count 
                     lazy-prime-factors))
       (cons 2)
       (drop (dec n))
       (first))) 


(deftest given-solution
  (is (= 13 (nth-prime 6))))

(comment 
  (nth-prime 10001)
  ;;=> 104759
  ;; So slow! I'll have to learn more about generating prime numbers. I don't even want to discuss how long
  )

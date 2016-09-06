(ns euler.ex17
  {:author "KGZM"}
  (:require [clojure.test :refer :all]
            [numbers.core :as numbers]))


;; If the numbers 1 to 5 are written out in words: one, two, three, four, five, then there are 3 + 3 + 5 + 4 + 4 = 19 letters used in total.
;; If all the numbers from 1 to 1000 (one thousand) inclusive were written out in words, how many letters would be used?
;; NOTE: Do not count spaces or hyphens. For example, 342 (three hundred and forty-two) contains 23 letters and 115 (one hundred and fifteen) contains 20 letters. The use of "and" when writing out numbers is in compliance with British usage.

;; So.. I already wrote a little thing to write numbers as english so I just imported it into this project.
;; I made some modifications, "to comply with british usage" but that's all, as my solution was already complete.

(defn count-letters [n]
  (count (filter (complement (set [\space \-]))
          (numbers/english-number n))))

(defn count-letters-up-to [n]
  (reduce + (map count-letters (range 1 (inc n)))))

(deftest given-solution
  (is (= 23 (count-letters 342)))
  (is (= 20 (count-letters 115)))
  (is (= 19 (count-letters-up-to 5))))


(comment 
  (count-letters-up-to 1000);;=> 21124
  )


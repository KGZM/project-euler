(ns euler.ex3
  (:require [euler.ex1 :refer [is-multiple]]
            [clojure.test :refer :all]))

;; The prime factors of 13195 are 5, 7, 13 and 29.
;; What is the largest prime factor of the number 600851475143 ?


(defn prime? [n]
  (->> (range 2 (inc (bigint (Math/sqrt n))))
       (every? #(not (is-multiple n %)))))

(defn slow-prime-factors 
  "Very slow, optimized greatly. See below."
  [n]
  (loop [quotient n 
         candidate (quot n 2)
         factors []]
    (if (or (<= quotient 1)
            (<= candidate 1))
      factors
      (let [is-factor (is-multiple quotient candidate)] 
        (recur 
          (if is-factor 
            (quot quotient candidate)
            quotient)
          (if is-factor 
            (quot quotient candidate) 
            (dec candidate))
          (if is-factor
            (if (prime? candidate)
              (conj factors candidate)
              (concat (slow-prime-factors candidate) factors))
            factors))))))

(defn strict-prime-factors 
  "A faster version that increments instead of decrements.
  I no longer have to call prime? to verify that the number is indeed prime.
  Browsing around after the fact I see that this could still be much faster
  but I'm willing to let it rest for now as my personal best solution."
  [n]
  (loop [quotient n candidate 2 factors []]
    (if (<= quotient 1) factors
      (if (is-multiple quotient candidate)
        (recur (quot quotient candidate) candidate (conj factors candidate))
        (recur quotient (inc candidate) factors)))))

(defn lazy-prime-factors 
  "A lazy version. Its use case is unclear,
  but it was interesting to write."
  ([quotient] (lazy-prime-factors quotient 2))
  ([quotient candidate]
   (cond (<= quotient 1) []
         (is-multiple quotient candidate) 
         (cons candidate 
               (lazy-seq 
                 (lazy-prime-factors (quot quotient candidate) 
                                     candidate)))
         :else (recur quotient (inc candidate))))) 

(deftest given-solution
  (doseq [f [slow-prime-factors 
             strict-prime-factors
             lazy-prime-factors]]
    (is (= [5 7 13 29]
           (sort (f 13195))))
    (is (= 29
           (apply max (f 13195))))))

(comment 
 (apply max (strict-prime-factors 600851475143N)) ;6857
  )

(ns euler.ex14
  {:author "KGZM"}
  (:require [clojure.test :refer :all]
            [clojure.core.reducers :as r]))

;; The following iterative sequence is defined for the set of positive integers:
;; 
;; n → n/2 (n is even)
;; n → 3n + 1 (n is odd)
;; 
;; Using the rule above and starting with 13, we generate the following sequence:
;; 
;; 13 → 40 → 20 → 10 → 5 → 16 → 8 → 4 → 2 → 1
;; It can be seen that this sequence (starting at 13 and finishing at 1) contains 10 terms. Although it has not been proved yet (Collatz Problem), it is thought that all starting numbers finish at 1.
;; 
;; Which starting number, under one million, produces the longest chain?
;; 
;; NOTE: Once the chain starts the terms are allowed to go above one million.

(defn collatz [n]
  (if (= 1 n) 
    [n] 
    (cons n (lazy-seq 
              (collatz  (if (even? n) 
                          (/ n 2) 
                          (+ (* 3 n) 1)))))))
(defn longest-reducer 
  ([] [0 []])
  ([a b] (max-key (comp count second) a b)))

(defn longest-collatz-parallel [n]
  (->> (range 1 n)
       (r/map (fn [n] [n (collatz n)]))
       (r/reduce longest-reducer)
      (first)))

(defn longest-collatz-single [n]
  (->> (range 1 n)
       (map (fn [n] [n (collatz n)]))
       (reduce (partial max-key (comp count second)))
       (first)))


(comment 
  (time (longest-collatz-parallel 1000000)) ;;=> 837799
  )

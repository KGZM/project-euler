(ns euler.ex10
  {:author "KGZM"}
  (:require [clojure.test :refer :all]
            [euler.ex3 :refer [lazy-prime-factors]]))

The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.
Find the sum of all the primes below two million.

(defn primes-slow
  "This is superceded by the work below.
  This function was too slow to solve the problem in the time
  I was willing to wait."
  []
  (->> (range)
       (filter odd?)
       (filter (comp (partial = 1)
                     count 
                     lazy-prime-factors))
       (cons 2)))

(defn find-index 
  "Returns the first index that matches the predicate
  or nil if none do."
  ([pred coll] (find-index pred coll 0))
  ([pred coll start]
   (let [length (count coll)] 
     (loop [i start]
       (cond (pred (nth coll i nil)) i
             (< i length) (recur (inc i))
             :else nil))))) 
  
(defn primes-up-to 
  "My implementation of The Sieve of Eratosthenes.
  I started with immutable data and was able to get
  it to run more than twice as fast just by adding
  transient, two !s, and persistent! ."
  [n]
  (loop [p 2 r 4 xs (transient (vec (range 0 (inc n))))]
    (if (<= p r n)
      (if (< n (+ r p))
        (let [nextp  (find-index some? xs (inc p))]
          (recur nextp
                 (* 2 nextp)
                 (assoc! xs r nil)))
        (recur p
               (+ r p)
               (assoc! xs r nil)))
      (drop 2 (filter some? (persistent! xs))))))

(defn sum-primes-below [n]
  (reduce + (primes-up-to n)))

(deftest given-solution
  (is (= 17 (sum-primes-below 10))))

(comment 
  (sum-primes-below 2000000) ;;=> 142913828922
  )

(ns euler.ex5 
  (:require [euler.ex3 :refer [lazy-prime-factors]]
            [clojure.test :refer :all]
            [clojure.math.numeric-tower :refer [expt]]) )
;; 2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.

;; What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?

;; For this one I spent some time in the REPL thinking about the problem and ended up solving it manually by analysis, using the REPL as a glorified calculator before distilling the solution into a function.
;;I'm going to reproduce some of my REPL session and my thinking below because that's how I actually produced the correct answer.

(comment 
  ;; I immediately thought of my function to get a prime factorization from Problem 3, because it occured to me that there must be a way to build this number from factors.
  (require '[euler.ex3 :as ex3] :reload)

  ;;I then wanted to have a look at the prime factorization of the easiest number that would be divisible by all its predecessors (10 factorial)
  (ex3/lazy-prime-factors (apply * (range 1 (inc 10))))
  ;;=>(2 2 2 2 2 2 2 2 3 3 3 3 5 5 7)

  ;;Then the prime factorization of the given answer.
  (ex3/lazy-prime-factors 2520)
  ;;=> (2 2 2 3 3 5 7)

  ;;Followed by a long look at 20 factorial. 
  (ex3/lazy-prime-factors (apply * (range 1 (inc 20))))
  ;;=> (2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 3 3 3 3 3 3 3 3 5 5 5 5 7 7 11 13 17 19)

  (frequencies (ex3/lazy-prime-factors (apply * (range 1 (inc 20)))))
  ;;=> {2 18, 3 8, 5 4, 7 2, 11 1, 13 1, 17 1, 19 1}

  ;;Then a look at the frequencies 10 factorial's prime factors.
  (frequencies (ex3/lazy-prime-factors (apply * (range 1 (inc 10)))))
  ;=>{2 8, 3 4, 5 2, 7 1}

  ;;Another look at the given answer.
  (frequencies (ex3/lazy-prime-factors 2520))
  ;;=> {2 3, 3 2, 5 1, 7 1}

  ;;After a bit more probing, I arrived at the insight that the smallest number that's evenly divisible by each of a range of numbers from 1 .. n must have exactly enough of each of its prime factors to produce the highest power that's within the range. So I manually went through and considered each number from 1 to 20 to produce a prime factorization.
  (require '[clojure.math.numeric-tower :refer :all])
  (reduce * (map (partial apply expt)
                 {2 4, 3 2, 5 1, 7 1, 11 1, 13 1, 17 1, 19 1}))
  ;;=>232792560
  )


;; And this grisly function does the job!
(defn smallest-evenly-disibly-by-1-to [n]
  (->> (range 1 (inc n))
       (map lazy-prime-factors)
       (filter (comp (partial = 1) 
                     count 
                     distinct))
       (group-by first)
       (map (comp (partial apply max-key count) 
                  second))
       (flatten)
       (apply *)))

(deftest does-it-fly? 
  (is (= 2520 (smallest-evenly-disibly-by-1-to 10)))
  (is (= 232792560 (smallest-evenly-disibly-by-1-to 20))))

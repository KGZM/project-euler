(ns euler.ex2
  (:require [clojure.test :refer :all]))


(defn fibonacci []
 (let [generator  
       (fn generator [a b] 
         (cons a (lazy-seq (generator b (+ a b)))))]
   (generator 1 2)))

(defn solution []
  (->> (take-while (partial > 4000000) (fibonacci))
       (filter even?)
       (reduce +)))
(comment 
  (solution) ;4613732
  )
  

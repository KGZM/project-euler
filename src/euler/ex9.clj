(ns euler.ex9
  {:author "KGZM"}
  (:require [clojure.math.numeric-tower :as math]
            [clojure.test :refer :all]
            [clojure.string :as string]))

;; A Pythagorean triplet is a set of three natural numbers, a < b < c, for which,
;; a^2 + b^2 = c^2
;; For example, 3^2 + 4^2 = 9 + 16 = 25 = 5^2.
;; There exists exactly one Pythagorean triplet for which a + b + c = 1000.
;; Find the product a * b * c.

(defn is-factor [n d]
  (if (zero? (rem n d))
    [d (quot n d)]
    nil))

(defn factor [n]
  (->> (range 1 (inc (math/sqrt n)))
       (map (partial is-factor n))
       (filter some?)))

(defn triplets 
  "I used Dixon's Method to generate 
  a lazy infinite sequence of all the pythagorean triplets.
  See: https://en.wikipedia.org/wiki/Formulas_for_generating_Pythagorean_triples#Dickson.27s_method "
  ([] (triplets 2))
  ([r]
   (let [r (if (even? r) r (inc r))
         r' (/ (math/expt r 2) 2)
         factors (factor r')
         ts (for [[s t] factors]
              [(+ r s)
               (+ r t)
               (+ r s t)])]
     (lazy-cat ts (triplets (+ r 2))))))

(deftest triplets-work
  (doseq [[a b c] (take 500 (triplets))]
    (is (= (math/expt c 2) 
           (+ (math/expt a 2)
              (math/expt b 2))))))

(defn triplet-with-sum [sum]
  (first (drop-while (comp (partial not= sum)
                           (partial reduce +))
                     (triplets))))

(defn product-of-triplet-with-sum [sum]
  (reduce * (triplet-with-sum sum)))

(comment 
  (product-of-triplet-with-sum 1000) ;;=> 31875000

  )

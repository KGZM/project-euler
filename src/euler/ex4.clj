(ns euler.ex4
  (:require [euler.ex3 :refer [lazy-prime-factors]]
            [clojure.math.combinatorics :as combo]
            [clojure.test :refer :all]
            [clojure.string :as string]
            [clojure.math.numeric-tower :refer [expt]]))

;; A palindromic number reads the same both ways. 
;; The largest palindrome made from 
;; the product of two 2-digit numbers is 9009 = 91 × 99.
;; Find the largest palindrome made from the product of two 3-digit numbers.

(defn palindromic? [s]
  (let [l (count s)
        s1 (subs s 0 (quot l 2))
        s2 (subs (string/reverse s) 0 (quot l 2))]
    (= s1 s2)))

(deftest identify-palindrome 
  (is (palindromic? (str 9009)))
  (is (palindromic? "racecar"))
  (is (not (palindromic? "definitelynot"))))


(defn highest-palindrome-brute-force 
  "A brute force solution."
  [digits]
  (let [upper (expt 10 digits)
        lower (expt 10 (dec digits))
        span (range lower upper)
        products (for [x span y span] (* x y))]
    (apply max (filter (comp palindromic? str) products))))


(comment 
  (highest-palindrome 3) ;;906609
  (highest-palindrome 5) ;; I haven't the patience.
  )

;;An attempt to make it slightly faster, but having to check if each palindrome
;; is a product of two numbers with the same number of digits is still giving terrible performance. 
;; For 4 digits it takes on the order of 27 seconds where as the brute force solution takes 39. 
;; I couldn't find any particular properties of palindromic numbers to make the acceptance check any easier.

;;EDIT I'm going to leave my above lament in place for posterity but I believe I can make this much faster by inspecting the factors of the numbers in question.

;; I'm gathering my tools here:
(def prime-factors lazy-prime-factors)

(defn multiple-pairs [n]
  (let [factors (combo/partitions (lazy-prime-factors n) :min 2 :max 2)
        to-pairs (fn [pair] (map (partial apply *) pair))]
    (map to-pairs factors)))


(defn generate-single-palindrome [digits]
  (let [even-span (range (expt 10 (dec (quot digits 2))) 
                    (expt 10 (quot digits 2)))
        odd-span (range (expt 10 (quot digits 2)) 
                    (expt 10 (inc (quot digits 2))))
        even-f (fn [n] (str n (string/reverse (str n))))
        odd-f  (fn [n] (str n (subs (string/reverse (str n)) 1)))
        span    (if (even? digits) even-span odd-span)
        f    (if (even? digits) even-f odd-f)]
    (map (comp read-string f) span)))

(defn generate-palindromes-for-digits [digits]
  (let [lower (expt 10 (dec digits))
        upper (dec (expt 10 digits))
        lower-product (* lower lower)
        upper-product (* upper upper)
        lower-digits (->> lower-product str count)
        upper-digits (->> upper-product str count)
        span (range lower-digits (inc upper-digits))
        is-product (fn [n] (->> (range lower (inc upper)) 
                                (filter #(and (<= lower (quot n %) upper)
                                              (zero? (rem n %))))
                                (first)
                                (some?)))
        accept (fn [n] (and (<= lower-product n upper-product)
                            (is-product n) 
                            ))
        ]
    (filter accept (mapcat generate-single-palindrome span))))

(defn generate-palindromes-for-digits-pessimized [digits]
  (let [lower (expt 10 (dec digits))
        upper (dec (expt 10 digits))
        lower-product (* lower lower)
        upper-product (* upper upper)
        lower-digits (->> lower-product str count)
        upper-digits (->> upper-product str count)
        span (range lower-digits (inc upper-digits))
        has-digits (fn [pair] (every? #(<= lower % upper) pair))
        accept (fn [n] (and (<= lower-product n upper-product)
                            (some has-digits (multiple-pairs n))))
        ]
    (filter accept (mapcat generate-single-palindrome span))))

(defn highest-palindrome-generated-for-digits [digits]
  (apply max (generate-palindromes-for-digits digits)))

(defn highest-palindrome-generated-for-digits-pessimized 
  [digits] (apply max (generate-palindromes-for-digits-pessimized digits)))

(deftest comparing-algorithms
  (doseq [f [highest-palindrome-brute-force
             highest-palindrome-generated-for-digits
             highest-palindrome-generated-for-digits-pessimized]]
    
  (is 9009 (f 2))
  (is 906609 (f 3))))

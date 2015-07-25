(ns euler.ex4
  (:require [clojure.test :refer :all]
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


(defn highest-palindrome 
  "A brute force solution."
  [digits]
  (let [upper (expt 10 digits)
        lower (expt 10 (dec digits))
        span (range lower upper)
        products (for [x span y span] (* x y))]
    (apply max (filter (comp palindromic? str) products))))

(deftest given-solution
  (is 9009 (highest-palindrome 2)))

(comment 
  (highest-palindrome 3) ;;906609
  (highest-palindrome 5) ;; I haven't the patience.
  )

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

(defn generate-palindromes-as-product [digits]
  (let [lower (expt 10 (dec digits))
        upper (dec (expt 10 digits))
        lower-product (* lower lower)
        upper-product (* upper upper)
        lower-digits (->> lower-product str count)
        upper-digits (->> upper-product str count)
        span (range lower-digits (inc upper-digits))
        is-product (fn [n] (->> (range lower (inc upper)) 
                                (filter #(and (->> (quot n %) 
                                                   str 
                                                   count 
                                                   (= digits)) 
                                              (zero? (rem n %))))
                                (first)
                                (some?)))
        accept (fn [n] (and (< (* lower lower)
                               n
                               (* upper upper))
                            (is-product n))) ]
    (filter accept (mapcat generate-single-palindrome span))))

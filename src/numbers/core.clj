(ns numbers.core
  {:author "KGZM"
   :description "Just a contraption to convert numbers into english words.
                I made some changes from the original version to support british usage
                for Project Euler, I'll probably want to make a switch between the two later."}
  (:require [clojure.string :as string]))

(def magnitude-words
  {1             nil 
   1000          "thousand"
   1000000       "million"
   1000000000    "billion"
   1000000000000 "trillion"})

(def teens-words 
  {11 "eleven"
   12 "twelve"
   13 "thirteen"
   14 "fourteen"
   15 "fifteen"
   16 "sixteen"
   17 "seventeen"
   18 "eighteen"
   19 "nineteen"})

(def ones-words 
  {1 "one"
   2 "two"
   3 "three"
   4 "four"
   5 "five"
   6 "six"
   7 "seven"
   8 "eight"
   9 "nine"})

(def tens-words 
  {10 "ten"
   20 "twenty"
   30 "thirty"
   40 "forty"
   50 "fifty"
   60 "sixty"
   70 "seventy"
   80 "eighty"
   90 "ninety"})

(defn log10 [n] (Math/floor (Math/log10 n)))

(defn magnitude [n] (Math/pow 10 (log10 n)))

(defn pow10 [n] (Math/pow 10 n))

(defn reciprocal [n] (/ 1 n))

(defn parseInt [s] (Integer/parseInt s))

(defn get-under [number place]
  (let [place-exp (log10 place)
        number-exp (log10 number)] 
    (long (- number
             (* (pow10 (inc place-exp)) 
                (Math/floor 
                  (* number 
                     (pow10 (- number-exp (inc place-exp)))
                     (reciprocal (magnitude number)))))))))

(defn get-place [number place]
  (long (Math/floor 
          (/ (get-under number place)
             place))))

(defn get-full-place [number place]
  (* place (get-place number place)))

(defn english-tens [number]
  (cond
    (= number 10) (tens-words number)
    (< number 10) (ones-words number)
    (< number 20) (teens-words number)
    :else 
    (let [tens-place (get-full-place number 10)] 
      (str (tens-words tens-place)  
           (when-let [ones-text (ones-words (- number tens-place))]
             (str "-" ones-text))))))

(defn number-parts [num mag & [tail]]
  (let [tens (get-under num 10)
        hundreds (get-place num 100)]
    (filterv some?  
             [(when (pos? hundreds) 
                (str (get ones-words hundreds) 
                     " hundred"))
              (when (and (pos? hundreds) ; To comply with "british usage". 
                         (pos? tens)) 
                "and")
              (when (pos? tens) 
                (english-tens tens))
              (get magnitude-words mag)
              (when (and (not-empty tail) ; More "british usage."
                         (or (pos? tens)
                             (pos? hundreds)))
                "and")
              (not-empty tail)])))

(defn chunk-number [number]
  (map (comp parseInt #(apply str %) reverse) 
       (partition 3 3 [] (reverse (str number)))))

(defn english-number [number]
  (if (zero? number) 
    "zero"
    (loop [[d & ds] (chunk-number number) mag 1 output ""]
      (if (nil? d) output
        (recur ds (* 1000 mag)
               (if (zero? d) output
                 (string/join " " (number-parts d mag output))))))))

(comment 
  (english-number 123456)
  (english-number 1000000)
  (english-number 999999999))



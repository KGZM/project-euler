(ns euler.ex15
  {:author "KGZM"}
  (:require [clojure.test :refer :all]
            [clojure.math.combinatorics :as combo]))

;; Starting in the top left corner of a 2×2 grid, and only being able to move to the right and down, there are exactly 6 routes to the bottom right corner.
;; How many such routes are there through a 20×20 grid?


(defn paths-recursive 
  ([x y] (paths-recursive x y []))
  ([x y ps] 
   (if (every? zero? [x y])
     [ps]
     (lazy-cat (if (pos? x) (paths-recursive (dec x) y (conj ps :right))) 
               (if (pos? y) (paths-recursive x (dec y) (conj ps :down)))))))

(defn paths [x y]
  (combo/count-permutations (concat (repeat x :right) (repeat y :up))))

(deftest given-solution 
  (is (= 6 (paths 2 2))))

(comment
  (paths 20 20) ;;=> 137846528820
  )

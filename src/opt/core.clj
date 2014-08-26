(ns opt.core
  (:require [clojure.math.numeric-tower :as num]
            [opt.base :as base]
            [opt.problems :as problems]
            [opt.cbc :as cbc])
  (:use plumbing.core))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#_(

   (def prob (problems/knapsack [:a :b :c :d]
                                {:a 10 :b 7 :c 3 :d 6}
                                {:a 3  :b 5 :c 7 :d 4}
                                13
                                :max))

   (def solved (update-problem prob cbc/solve))

   @(tag->var solved :a)
   ;; -> 1
   @(tag->var solved :b)
   ;; -> 1
   @(tag->var solved :c)
   ;; -> 0
   @(tag->var solved :d)
   ;; -> 1

   (def prob2
     (let [els  [1 2 3 4 5]
           sets [#{1 2} #{1 3} #{2 3 4} #{1 5}]]
       (set-union-knapsack sets els #(Math/sqrt (count %)) (constantly 1) 4:max)))

   
     
   )

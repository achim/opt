(ns opt.core
  (:require [clojure.math.numeric-tower :as num]
            [opt.base :as base]
            [opt.problems :as problems]
            [opt.cbc :as cbc])
  (:use plumbing.core))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(prefer-method print-method java.util.Map clojure.lang.IDeref)

#_(

   (def prob (problems/knapsack [:a :b :c :d]
                                {:a 10 :b 7 :c 3 :d 6}
                                {:a 3  :b 5 :c 7 :d 4}
                                13))

   (def solved (update-problem prob cbc/solve))

   @(tag->var solved :a)
   ;; -> 1
   @(tag->var solved :b)
   ;; -> 1
   @(tag->var solved :c)
   ;; -> 0
   @(tag->var solved :d)
   ;; -> 1

   )

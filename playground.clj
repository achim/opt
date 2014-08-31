(ns playground
  (:require [opt.core :as opt]
            [opt.formats.lp :as lp]
            [opt.solvers.cbc :as cbc]
            [opt.solvers.gurobi :as grb]
            [opt.problems :as problems]))

(def ks (problems/knapsack [:a :b :c :d]
                           {:a 10 :b 7 :c 3 :d 6} ; values
                           {:a 3  :b 5 :c 7 :d 4} ; sizes
                           13                     ; max-size
                           :max))

(println (lp/repr ks))

;;; Maximize
;;; 6 v4 + 3 v3 + 7 v2 + 10 v1
;;; Subject To
;;; 4 v4 + 7 v3 + 5 v2 + 3 v1 <= 13
;;; Bounds
;;; 0 <= v4 <= 1
;;; 0 <= v3 <= 1
;;; 0 <= v2 <= 1
;;; 0 <= v1 <= 1
;;; Binary
;;; v1 v2 v3 v4
;;; End

(def ks-solved (opt/update-problem ks grb/solve))

(for [v (opt/get-ovars ks-solved) :when (= @v 1)] (:tag v))

;;; (:d :b :a)

(:val ks-solved)

;;; 23.0


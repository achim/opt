(ns opt.problems
  (:require [opt.core :as o]))

(defn knapsack [items val-fn size-fn max-size]
  (let [vars  (map #(o/variable :type :bin :tag %) items)
        vals  (map val-fn items)
        sizes (map size-fn items)]
    (o/problem :max (o/linear-term vars vals)
               [(o/affine-constr :<= (o/linear-term vars sizes) max-size)]
               vars)))

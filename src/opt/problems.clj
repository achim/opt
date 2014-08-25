(ns opt.problems
  (:require [opt.base :as b]))

(defn knapsack [items val-fn size-fn max-size]
  (let [vars  (map #(b/variable :type :bin :tag %) items)
        vals  (map val-fn items)
        sizes (map size-fn items)]
    (b/problem :max (b/linear-term vars vals)
               [(b/affine-constr :<= (b/linear-term vars sizes) max-size)]
               vars)))

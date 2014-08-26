(ns opt.problems
  (:require [opt.base :as b]
            [clojure.set :as set]
            [clojure.math.combinatorics :as comb]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- binvar [tag]
  (b/variable :type :bin :tag tag))

(defn tag-fn [f]
  (fn [& args]
    (apply f (map :tag args))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn knapsack [items val-fn size-fn total-size flavor]
  (let [vars  (map binvar items)
        vals  (map val-fn items)
        sizes (map size-fn items)
        cmp   (get {:max :<= :min :>=} flavor flavor)]
    (b/problem flavor (b/linear-term vars vals)
               (b/affine-constr cmp (b/linear-term vars sizes) total-size)
               vars)))

(defn ifthen-constrs [pred a-vars b-vars]
  (for [[a b] (comb/cartesian-product a-vars b-vars) :when ((tag-fn pred) a b)]
    (b/affine-constr :<= (b/linear-term [a b] [1 -1]) 0)))

(defn set-system-constrs [set-vars element-vars]
  (ifthen-constrs contains? set-vars element-vars))

(defn set-union-knapsack [sets elements set-val-fn element-size-fn max-size flavor]
  (let [set-vars (map binvar sets)
        el-vars  (map binvar elements)
        set-vals (map set-val-fn sets)
        el-sizes (map element-size-fn elements)
        cmp      ({:max :<= :min :>=} flavor)]
    (b/problem flavor (b/linear-term set-vars set-vals)
               [(set-system-constrs set-vars el-vars)
                (b/affine-constr cmp (b/linear-term el-vars el-sizes) max-size)]
               [set-vars el-vars])))

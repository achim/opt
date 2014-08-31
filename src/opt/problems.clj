(ns opt.problems
  (:require [opt.core :as opt]
            [clojure.set :as set]))



(defn tag-fn [f]
  (fn [& args]
    (apply f (map :tag args))))



(defn knapsack [items val-fn size-fn total-size sense]
  (let [vars  (opt/ovars items :type :bin)
        vals  (map val-fn items)
        sizes (map size-fn items)
        cmp   (get {:max :<= :min :>=} sense sense)]
    (opt/problem sense (opt/linear-expr vars vals)
                 (opt/affine-constr cmp (opt/linear-expr vars sizes) total-size)
                 vars)))

(defn ifthen-constrs [pred a-vars b-vars]
  (for [a a-vars
        b b-vars
        :when ((tag-fn pred) a b)]
    (opt/affine-constr :<= (opt/linear-expr [a b] [1 -1]) 0)))

(defn set-system-constrs [set-vars element-vars]
  (ifthen-constrs contains? set-vars element-vars))



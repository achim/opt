(ns opt.core
  (:require [clojure.math.numeric-tower :as num]
            [opt.util :as util]
            [opt.cbc :as cbc]
            [opt.problems :as problems]
            [opt.types :as types])
  (:use plumbing.core))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def ^:private make-id
  (let [max (num/expt 10 15)
        ctr (atom 0)
        nxt (fn [i] (mod (inc i) max))]
    (fn [] (swap! ctr nxt))))

(defn variable [& {:keys [type tag] :or {type :gen} :as args}]
  (types/->Variable (make-id) type (:>= args) (:<= args) tag nil))

(defn linear-term
  ([vars->coeffs] (types/->LinearTerm (map-keys :id vars->coeffs)))
  ([vars coeffs] (linear-term (zipmap vars coeffs))))

(defn negate [linear-term]
  (update-in linear-term [:varids->coeffs] #(map-vals - %)))

(defn affine-constr
  ([ineq lhs rhs]
     (if (= ineq :<=)
       (types/->AffineConstr lhs rhs)
       (types/->AffineConstr (negate lhs) (- rhs)))))

(defn problem [flavor objective constraints vars]
  (types/->Problem flavor
                   objective
                   (flatten constraints)
                   (util/index-by-unique :id vars)
                   (->> vars
                        (filter :tag)
                        (util/index-by-unique :tag)
                        (map-vals :id))
                   :unattempted
                   nil))

(defn update-problem [problem solver]
  (let [result       (solver problem)
        status       (:status result)
        val          (:val result)
        varids->vals (:varids->vals result)
        problem      (-> problem
                         (assoc :status status)
                         (assoc :val val))
        step         (fn [acc [vid val]]
                  (update-in acc [:varids->vars vid] assoc :val val))]
    (reduce step problem varids->vals)))

(defn tag->var [problem tag]
  (let [varids->vars (:varids->vars problem)
        tags->varids (:tags->varids problem)]
    (-> tag tags->varids varids->vars)))

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

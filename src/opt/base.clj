(ns opt.base
  (:require [clojure.math.numeric-tower :as num]
            [opt.util :as util]
            [clojure.pprint :as pp])
  (:use plumbing.core))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrecord Variable [id type >= <= tag val]
  clojure.lang.IDeref
  (deref [me] val))

(prefer-method print-method java.util.Map clojure.lang.IDeref)
(prefer-method print-method clojure.lang.IRecord clojure.lang.IDeref)
(prefer-method pp/simple-dispatch java.util.Map clojure.lang.IDeref)
(prefer-method pp/simple-dispatch clojure.lang.IRecord clojure.lang.IDeref)
(prefer-method pp/simple-dispatch clojure.lang.IPersistentMap clojure.lang.IDeref)

(defrecord LinearTerm [varids->coeffs])
(defrecord AffineConstr [linear-term upper-bound])
(defrecord Problem [flavor objective constraints varids->vars tags->varids status val])
(defrecord SolverResult [status val varids->vals])

(def ^:private make-id
  (let [max (num/expt 10 15)
        ctr (atom 0)
        nxt (fn [i] (mod (inc i) max))]
    (fn [] (swap! ctr nxt))))

(defn variable [& {:keys [type tag] :or {type :gen} :as args}]
  (->Variable (make-id) type (:>= args) (:<= args) tag nil))

(defn linear-term
  ([vars->coeffs] (->LinearTerm (map-keys :id vars->coeffs)))
  ([vars coeffs] (linear-term (zipmap vars coeffs))))

(defn negate [linear-term]
  (update-in linear-term [:varids->coeffs] #(map-vals - %)))

(defn affine-constr
  ([ineq lhs rhs]
     (if (= ineq :<=)
       (->AffineConstr lhs rhs)
       (->AffineConstr (negate lhs) (- rhs)))))

(defn problem [flavor objective constraints vars]
  (let [vars (flatten vars)]
    (->Problem flavor
               objective
               (flatten [constraints])
               (util/index-by-unique :id vars)
               (->> vars
                    (filter :tag)
                    (util/index-by-unique :tag)
                    (map-vals :id))
               :unattempted
               nil)))

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

(defn vars [problem]
  (vals (:varids->vars problem)))



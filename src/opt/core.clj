(ns opt.core
  (:import (clojure.lang IDeref IRecord IPersistentMap)
           (java.util Map))
  (:require [clojure.math.numeric-tower :as num]
            [opt.util :as util]
            [clojure.pprint :as pp])
  (:use plumbing.core))


(defrecord Variable [id type >= <= tag val]
  IDeref
  (deref [_] val))

(defrecord LinearExpr   [varids->coeffs])
(defrecord AffineConstr [sense linear-expr bound])
(defrecord Problem      [sense objective constraints varids->vars tags->varids status val])
(defrecord SolverResult [status val varids->vals])

(prefer-method print-method       Map            IDeref)
(prefer-method print-method       IRecord        IDeref)
(prefer-method pp/simple-dispatch Map            IDeref)
(prefer-method pp/simple-dispatch IRecord        IDeref)
(prefer-method pp/simple-dispatch IPersistentMap IDeref)

(def ^:private make-id
  (let [max (num/expt 10 15)
        ctr (atom 0)
        nxt (fn [i] (mod (inc i) max))]
    (fn [] (swap! ctr nxt))))

(defn ovar [& {:keys [type tag] :or {type :gen} :as args}]
  {:pre [(or (not= type :semi) (and (:>= args) (:<= args)))]}
  (let [[lb ub] (if (= type :bin) [0 1] ((juxt :>= :<=) args))]
    (->Variable (make-id) type lb ub tag nil)))

(defn ovars [tags & args]
  (for [t tags] (apply ovar :tag t args)))

(defn linear-expr
  ([vars->coeffs] (->LinearExpr (map-keys :id vars->coeffs)))
  ([vars coeffs] (linear-expr (zipmap vars coeffs))))

(defn negate [linear-expr]
  (update-in linear-expr [:varids->coeffs] #(map-vals - %)))

(defn affine-constr [sense linear-expr bound]
  (->AffineConstr sense linear-expr bound))

(defn problem [sense objective constraints vars]
  (let [vars (flatten vars)]
    (->Problem sense
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

(defn tag->ovar [problem tag]
  (let [varids->vars (:varids->vars problem)
        tags->varids (:tags->varids problem)]
    (-> tag tags->varids varids->vars)))

(defn get-ovars [problem]
  (vals (:varids->vars problem)))



(ns opt.solvers.gurobi
  (:require [opt.core :as opt])
  (:import [gurobi GRBEnv GRBLinExpr GRBModel GRB GRB$Status GRB$IntAttr GRB$DoubleAttr GRBVar])
  (:use plumbing.core))


(def ^:dynamic ^GRBEnv *env* (GRBEnv.))

(def ^:private vartypes {:gen  GRB/CONTINUOUS
                         :bin  GRB/BINARY
                         :int  GRB/INTEGER
                         :semi GRB/SEMICONT})

(def ^:private valtypes {:gen  double
                          :bin  int
                          :int  int
                          :semi double})

(def ^:private constr-senses {:>= GRB/GREATER_EQUAL
                              :=  GRB/EQUAL
                              :<= GRB/LESS_EQUAL})

(def ^:private senses {:min GRB/MINIMIZE
                       :max GRB/MAXIMIZE})

(defn- model [problem]
  (let [^GRBModel result (GRBModel. *env*)
        grbvar (fn [v]
                 (.addVar result
                          (:>= v) (:<= v)
                          (get (:objective problem) (:id v) 0)
                          (vartypes (:type v))
                          (str "v" (:id v))))
        varids->grbvars (map-vals grbvar (:varids->vars problem))
        linexpr (fn [e]
                  (let [result (GRBLinExpr.)]
                    (dorun
                      (for [[varid ^double c] (:varids->coeffs e)]
                        (let [^GRBVar gv (varids->grbvars varid)]
                          (.addTerm result c gv))))
                    result))
        obj (linexpr (:objective problem))]
    (.update result)
    (.setObjective result obj (senses (:sense problem)))
    (.update result)
    (doall
      (for [c (:constraints problem)]
        (let [le (linexpr (:linear-expr c))
              s  (constr-senses (:sense c))
              b  (double (:bound c))]
          (.addConstr result le s b ""))))
    (.update result)
    [result varids->grbvars]))

(defn solve [problem]
  (let [[^GRBModel m varids->grbvars] (model problem)
        varids->types (map-vals :type (:varids->vars problem))]
    (.optimize m)
    (when (= (.get m GRB$IntAttr/Status) GRB$Status/OPTIMAL)
      (let [optval       (.get m GRB$DoubleAttr/ObjVal)
            varids->vals (into {}
                               (for [[varid grbv] varids->grbvars]
                                 [varid ((valtypes (varids->types varid))
                                         (.get grbv GRB$DoubleAttr/X))]))]
        (opt/->SolverResult :optimal optval varids->vals)))))
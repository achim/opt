(ns opt.types
  (:require [clojure.math.numeric-tower :as num]
            [opt.util :as util]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- sgnchar [x]
  (if (>= x 0) \+ \-))

(defn- render-var [varid]
  (str "v" varid))

(def ^:private flavorstr {:min "Minimize" :max "Maximize"})
(def ^:private vartypestr {:gen "General" :bin "Binary" :int "Integer"})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrecord Variable [id type >= <= tag val]
  Object ;----------
  (toString [me] (render-var id))
  clojure.lang.IDeref ;----------
  (deref [me] val))

(prefer-method print-method java.util.Map clojure.lang.IDeref)

(defrecord LinearTerm [varids->coeffs]
  Object ;----------
  (toString [me]
    (let [[[vid coeff]
           & vidcoeffs] (seq varids->coeffs)
           sbuilder     (StringBuilder.)]
      (.append sbuilder (str coeff " " (render-var vid)))
      (dorun
       (for [[vid coeff] vidcoeffs]
         (.append sbuilder
                  (str " " (sgnchar coeff)
                       " " (num/abs coeff)
                       " " (render-var vid)))))
      (str sbuilder))))

(defrecord AffineConstr [linear-term upper-bound]
  Object ;----------
  (toString [me] (str (str linear-term " <= " upper-bound))))

(defrecord Problem [flavor objective constraints varids->vars tags->varids status val]
  Object ;----------
  (toString [me]
    (let [sbuilder (StringBuilder.)
          add-line (fn [& vals]
                     (.append sbuilder (apply str (interpose " " vals)))
                     (.append sbuilder "\n"))]
      (add-line (flavorstr flavor))
      (add-line objective)
      (add-line "Subject To")
      (dorun (map add-line constraints))
      (add-line "Bounds")
      (dorun (for [v (vals varids->vars)]
               (add-line (or (:>= v) (if (= :bin (:type v))
                                       0
                                       "-infinity"))
                         "<=" v "<="
                         (or (:<= v) (if (= :bin (:type v))
                                       1
                                       "+infinity")))))
      (dorun (for [[sec vs] (util/index-by :type (vals varids->vars))]
               (do (add-line (vartypestr sec))
                   (apply add-line vs))))
      (add-line "End")
      (str sbuilder))))

(defrecord SolverResult [status val varids->vals])

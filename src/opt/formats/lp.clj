(ns opt.formats.lp
  (:import (opt.core LinearExpr AffineConstr Problem Variable))
  (:require [clojure.math.numeric-tower :as num]
            [opt.util :as util]))


(defn- sgnchar [x]
  (if (>= x 0) \+ \-))

(defn- render-var [varid]
  (str "v" varid))

(def ^:private sensestr {:min "Minimize"
                          :max "Maximize"})
(def ^:private vartypestr {:gen  "General"
                           :bin  "Binary"
                           :int  "Integer"
                           :semi "Semi-Continuous"})


(defprotocol LpFormat
  (repr [me]))

(extend-protocol LpFormat
  Variable ;----------
  (repr [me]
    (render-var (:id me)))
  LinearExpr ;----------
  (repr [me]
    (let [[[vid coeff]
           & vidcoeffs] (seq (:varids->coeffs me))
           sbuilder     (StringBuilder.)]
      (.append sbuilder (str coeff " " (render-var vid)))
      (dorun
       (for [[vid coeff] vidcoeffs]
         (.append sbuilder
                  (str " " (sgnchar coeff)
                       " " (num/abs coeff)
                       " " (render-var vid)))))
      (str sbuilder)))
  AffineConstr ;----------
  (repr [me]
    (str (repr (:linear-expr me))
         " " (name (:sense me))
         " " (:bound me)))
  Problem ;----------
  (repr [me]
    (let [sbuilder (StringBuilder.)
          add-line (fn [& vals]
                     (.append sbuilder (apply str (interpose " " vals)))
                     (.append sbuilder "\n"))]
      (add-line (sensestr (:sense me)))
      (add-line (repr (:objective me)))
      (add-line "Subject To")
      (dorun (map #(add-line (repr %)) (:constraints me)))
      (add-line "Bounds")
      (dorun (for [v (vals (:varids->vars me))]
               (add-line (or (:>= v) (if (= :bin (:type v))
                                       0
                                       "-infinity"))
                         "<=" (repr v) "<="
                         (or (:<= v) (if (= :bin (:type v))
                                       1
                                       "+infinity")))))
      (dorun (for [[sec vs] (util/index-by :type (vals (:varids->vars me)))]
               (do (add-line (vartypestr sec))
                   (apply add-line (map repr vs)))))
      (add-line "End")
      (str sbuilder))))

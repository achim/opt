(ns opt.lp
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

(defprotocol LpFormat
  (repr [me]))

(extend-protocol LpFormat
  opt.base.Variable ;----------
  (repr [me] (render-var (:id me)))
  opt.base.LinearTerm ;----------
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
  opt.base.AffineConstr ;----------
  (repr [me] (str (repr (:linear-term me)) " <= " (:upper-bound me)))
  opt.base.Problem ;----------
  (repr [me]
    (let [sbuilder (StringBuilder.)
          add-line (fn [& vals]
                     (.append sbuilder (apply str (interpose " " vals)))
                     (.append sbuilder "\n"))]
      (add-line (flavorstr (:flavor me)))
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

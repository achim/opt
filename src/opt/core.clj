(ns opt.core
  (use clojure.math.numeric-tower
       plumbing.core))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- sgnchar [x]
  (if (>= x 0) \+ \-))

(defn- index-by [f coll]
  (letfn [(step [acc x]
            (update-in acc [(f x)] conj x))]
    (reduce step {} coll)))

(def ^:private flavorstr {:min "Minimize" :max "Maximize"})
(def ^:private vartypestr {:gen "General" :bin "Binary" :int "Integer"})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprotocol HasVars
  (vars [me]))

(defrecord Var [id type >= <= tag]
  Object ;----------
  (toString [me] (str "x" id))
  HasVars ;----------
  (vars [me] me))

(defrecord LinearTerm [vars-to-coeffs]
  Object ;----------
  (toString [me]
    (let [[[v c] & vcs] (seq vars-to-coeffs)
          sbuilder      (StringBuilder.)]
      (.append sbuilder (str c " " v))
      (dorun
       (for [[v c] vcs]
         (.append sbuilder (str " " (sgnchar c) " " (abs c) " " v))))
      (str sbuilder)))
  HasVars ;----------
  (vars [me] (keys vars-to-coeffs)))

(defrecord AffineConstr [linear-term lower-bound]
  Object ;----------
  (toString [me] (str (str linear-term " >= " lower-bound)))
  HasVars ;----------
  (vars [me] (vars linear-term)))

(defrecord Problem [flavor objective constraints vars]
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
      (dorun (for [v vars]
               (add-line (or (:>= v) (if (= :bin (:type v))
                                       0
                                       "-infinity"))
                         "<=" v "<="
                         (or (:<= v) (if (= :bin (:type v))
                                       1
                                       "+infinity")))))
      (dorun (for [[sec vs] (index-by :type vars)]
               (do (add-line (vartypestr sec))
                   (apply add-line vs))))
      (add-line "End")
      (str sbuilder)))
  HasVars ;----------
  (vars [me] vars))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def ^:private make-id
  (let [max (expt 10 15)
        ctr (atom 0)
        nxt (fn [i] (mod (inc i) max))]
    (fn [] (swap! ctr nxt))))

(defn make-var [& {:keys [type tag] :or {type :gen} :as args}]
  (Var. (make-id) type (:>= args) (:<= args) tag))

(defn linear-term
  ([vars-to-coeffs] (LinearTerm. vars-to-coeffs))
  ([vars coeffs] (linear-term (zipmap vars coeffs))))

(defn negate [linear-term]
  (LinearTerm. (map-vals - (:vars-to-coeffs linear-term))))

(defn affine-constr
  ([ineq lhs rhs]
     (if (= ineq :>=)
       (AffineConstr. lhs rhs)
       (AffineConstr. (negate lhs) (- rhs)))))

(defn make-problem [flavor objective constraints]
  (let [all-vars (reduce (partial reduce conj)
                         (into #{} (vars objective))
                         (map vars constraints))]
    (Problem. flavor objective constraints all-vars)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#_(

   (str (linear-term (repeatedly 3 make-var) [-1 -2 -3]))

   
   (make-var)

   (make-var :>= 5 :<= 10 :type :int)

   (make-var :tag [:foo 1])

   )

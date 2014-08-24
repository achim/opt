(ns opt.util
  (:use plumbing.core))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn index-by [f coll]
  (letfn [(step [acc x]
            (update-in acc [(f x)] conj x))]
    (reduce step {} coll)))

(defn index-by-unique [f coll]
  (map-vals first (index-by f coll)))

  

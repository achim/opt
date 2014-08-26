(ns opt.core
  (:require [clojure.math.numeric-tower :as num]
            [opt.base :as base]
            [opt.problems :as problems]
            [opt.cbc :as cbc]
            [opt.lp :as lp]
            [opt.util :as util])
  (:use plumbing.core))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#_(

(def prob (problems/knapsack [:a :b :c :d]
                             {:a 10 :b 7 :c 3 :d 6}
                             {:a 3  :b 5 :c 7 :d 4}
                             13
                             :max))

;; -> {:flavor :max,
;;     :objective {:varids->coeffs {4 6, 3 3, 2 7, 1 10}},
;;     :constraints
;;     ({:linear-term {:varids->coeffs {4 4, 3 7, 2 5, 1 3}},
;;       :upper-bound 13}),
;;     :varids->vars
;;     {4 {:id 4, :type :bin, :>= nil, :<= nil, :tag :d, :val nil},
;;      3 {:id 3, :type :bin, :>= nil, :<= nil, :tag :c, :val nil},
;;      2 {:id 2, :type :bin, :>= nil, :<= nil, :tag :b, :val nil},
;;      1 {:id 1, :type :bin, :>= nil, :<= nil, :tag :a, :val nil}},
;;     :tags->varids {:d 4, :c 3, :b 2, :a 1},
;;     :status :unattempted,
;;     :val nil}

(def solved (base/update-problem prob cbc/solve))

;; -> {:flavor :max,
;;     :objective {:varids->coeffs {4 6, 3 3, 2 7, 1 10}},
;;     :constraints
;;     ({:linear-term {:varids->coeffs {4 4, 3 7, 2 5, 1 3}},
;;       :upper-bound 13}),
;;     :varids->vars
;;     {4 {:id 4, :type :bin, :>= nil, :<= nil, :tag :d, :val 1},
;;      3 {:id 3, :type :bin, :>= nil, :<= nil, :tag :c, :val 0},
;;      2 {:id 2, :type :bin, :>= nil, :<= nil, :tag :b, :val 1},
;;      1 {:id 1, :type :bin, :>= nil, :<= nil, :tag :a, :val 1}},
;;     :tags->varids {:d 4, :c 3, :b 2, :a 1},
;;     :status :optimal,
;;     :val 23.0}

@(base/tag->var solved :a)

;; -> 1

@(base/tag->var solved :b)

;; -> 1

@(base/tag->var solved :c)

;; -> 0

@(base/tag->var solved :d)

;; -> 1

(def prob2
  (let [els  [1 2 3 4 5]
        sets [#{1 2} #{1 3} #{2 3 4} #{1 5}]]
    (problems/set-union-knapsack
     sets els #(Math/sqrt (count %)) (constantly 1) 4 :max)))

;; -> {:flavor :max,
;;     :objective
;;     {:varids->coeffs
;;      {4 1.4142135623730951,
;;       3 1.7320508075688772,
;;       2 1.4142135623730951,
;;       1 1.4142135623730951}},
;;     :constraints
;;     ({:linear-term {:varids->coeffs {5 -1, 1 1}}, :upper-bound 0}
;;      {:linear-term {:varids->coeffs {6 -1, 1 1}}, :upper-bound 0}
;;      {:linear-term {:varids->coeffs {5 -1, 2 1}}, :upper-bound 0}
;;      {:linear-term {:varids->coeffs {7 -1, 2 1}}, :upper-bound 0}
;;      {:linear-term {:varids->coeffs {6 -1, 3 1}}, :upper-bound 0}
;;      {:linear-term {:varids->coeffs {7 -1, 3 1}}, :upper-bound 0}
;;      {:linear-term {:varids->coeffs {8 -1, 3 1}}, :upper-bound 0}
;;      {:linear-term {:varids->coeffs {5 -1, 4 1}}, :upper-bound 0}
;;      {:linear-term {:varids->coeffs {9 -1, 4 1}}, :upper-bound 0}
;;      {:linear-term {:varids->coeffs {9 1, 8 1, 7 1, 6 1, 5 1}},
;;       :upper-bound 4}),
;;     :varids->vars
;;     {7 {:id 7, :type :bin, :>= nil, :<= nil, :tag 3, :val nil},
;;      1 {:id 1, :type :bin, :>= nil, :<= nil, :tag #{1 2}, :val nil},
;;      4 {:id 4, :type :bin, :>= nil, :<= nil, :tag #{1 5}, :val nil},
;;      6 {:id 6, :type :bin, :>= nil, :<= nil, :tag 2, :val nil},
;;      3 {:id 3, :type :bin, :>= nil, :<= nil, :tag #{4 3 2}, :val nil},
;;      2 {:id 2, :type :bin, :>= nil, :<= nil, :tag #{1 3}, :val nil},
;;      9 {:id 9, :type :bin, :>= nil, :<= nil, :tag 5, :val nil},
;;      5 {:id 5, :type :bin, :>= nil, :<= nil, :tag 1, :val nil},
;;      8 {:id 8, :type :bin, :>= nil, :<= nil, :tag 4, :val nil}},
;;     :tags->varids
;;     {1 5, 4 8, #{4 3 2} 3, #{1 5} 4, 3 7, 2 6, #{1 3} 2, 5 9, #{1 2} 1},
;;     :status :unattempted,
;;     :val nil}

(def solved2 (base/update-problem prob cbc/solve))

(->> solved2
     base/vars
     (util/index-by-unique :tag)
     (map-vals deref))

;; -> {:a 1, :b 1, :c 0, :d 1}

   )

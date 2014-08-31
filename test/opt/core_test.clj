(ns opt.core-test
  (:require [clojure.test :refer :all]
            [opt.core :refer :all]
            [opt.formats.lp :as lp]
            [clojure.string :as string]))



(defn nospace [s]
  (string/replace s #"\s" ""))

(deftest test-linear-terms
  (let [v1 (assoc (ovar) :id 1)
        v2 (assoc (ovar) :id 2)
        lt (linear-expr [v1 v2] [3 -7])]
    (testing "can construct linear terms"
      (let [s (nospace (lp/repr lt))]
        (is (= (count (re-seq #"-" s)) 1))
        (is (<= (count (re-seq #"\+" s)) 1))
        (is (.contains s "3v1"))
        (is (.contains s "-7v2"))
        (is (and (<= 7 (count s))
                 (<= (count s) 8)))))
    (testing "can negate linear terms"
      (let [s (nospace (lp/repr (negate lt)))]
        (is (= (count (re-seq #"-" s)) 1))
        (is (<= (count (re-seq #"\+" s)) 1))
        (is (.contains s "-3v1"))
        (is (.contains s "7v2"))
        (is (and (<= 7 (count s))
                 (<= (count s) 8)))))))

(deftest test-affine-constrs
  (let [v1 (assoc (ovar) :id 1)
        v2 (assoc (ovar) :id 2)
        lt (linear-expr [v1 v2] [3 -7])
        a1 (nospace (lp/repr (affine-constr :<= lt 123)))
        a2 (nospace (lp/repr (affine-constr :>= lt 456)))]
    (testing "<= constraints work"
      (is (.contains a1 "<=123"))
      (is (.contains a1 "-7v2")))
    (testing ">= constraints work"
      (is (.contains a2 ">=456"))
      (is (.contains a2 "3v1")))))

(deftest test-ovar
  (testing "semi needs bounds"
    (is (thrown? AssertionError (ovar :type :semi)))))
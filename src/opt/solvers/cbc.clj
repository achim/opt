(ns opt.solvers.cbc
  (:import (java.nio.file Path))
  (:require [opt.util :as util]
            [opt.core :as opt]
            [opt.formats.lp :as lp]
            [clojure.string :as string]))



(defn run-solver [lp]
  (util/with-tempdir [^Path dir]
    (let [in-path  (str (.resolve dir "in.lp"))
          out-path (str (.resolve dir "out.txt"))]
      (spit (str in-path) lp)
      (util/run-external "cbc" in-path "solve" "solution" out-path)
      (slurp out-path))))

(defn- split-row [s]
  (-> s
      string/trim
      (string/split #"\s+")))

(defn solve [problem]
  (let [[^String synopsis & rows] (string/split-lines
                            (run-solver (lp/repr problem)))]
    (when (.startsWith synopsis "Optimal")
      (let [optval       ((if (= (:sense problem) :max) - +)
                          (read-string (last (split-row synopsis))))
            varids->vals (into {} (for [r rows]
                                    (let [cols (split-row r)]
                                      [(read-string (subs (cols 1) 1))
                                       (read-string (cols 2))])))]
        (opt/->SolverResult :optimal optval varids->vals)))))

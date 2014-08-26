(ns opt.cbc
  (:require [opt.util :as util]
            [opt.base :as base]
            [opt.lp :as lp]
            [clojure.string :as string]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn run-solver [lp]
  (util/with-tempdir [dir]
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
  (let [[synopsis & rows] (string/split-lines
                           (run-solver (lp/repr problem)))]
    (when (.startsWith synopsis "Optimal")
      (let [optval ((if (= (:flavor problem) :max) - +)
                    (read-string (last (split-row synopsis))))
            varids->vars   (into {} (for [r rows]
                              (let [cols (split-row r)]
                                [(read-string (subs (cols 1) 1))
                                 (read-string (cols 2))])))]
        (base/->SolverResult :optimal optval varids->vars)))))

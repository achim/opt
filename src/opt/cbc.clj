(ns opt.cbc
  (:require [opt.util :as util]
            [opt.types :as types]
            [clojure.string :as string]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- run-solver [lp]
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
                           (run-solver (str problem)))]
    (when (.startsWith synopsis "Optimal")
      (let [optval ((if (= (:flavor problem) :max) - +)
                    (read-string (last (split-row synopsis))))
            varids->vars   (into {} (for [r rows]
                              (let [cols (split-row r)]
                                [(read-string (subs (cols 1) 1))
                                 (read-string (cols 2))])))]
        (types/->SolverResult :optimal optval varids->vars)))))
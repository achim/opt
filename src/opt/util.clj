(ns opt.util
  (:use plumbing.core)
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]
           [org.apache.commons.io FileUtils]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn index-by [f coll]
  (letfn [(step [acc x]
            (update-in acc [(f x)] conj x))]
    (reduce step {} coll)))

(defn index-by-unique [f coll]
  (map-vals first (index-by f coll)))

(defn run-external [& args]
  (let [procbuilder (doto (ProcessBuilder. args)
                      (.inheritIO))]
    (.waitFor (.start procbuilder))))
    
(defmacro with-tempdir [[dir] & body]
  (let [result (gensym)]
    `(let [~dir (Files/createTempDirectory nil (make-array FileAttribute 0))]
       (let [~result (do ~@body)]
         (FileUtils/deleteDirectory (.toFile ~dir))
         ~result))))


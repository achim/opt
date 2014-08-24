(defproject opt "0.1.0-SNAPSHOT"
  :description "A Clojure library for interfacing with (I)LP solvers"
  :url "https://github.com/achim/opt"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :jvm-opts ["-Xmx1g" "-server"] 
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/math.numeric-tower "0.0.4"]
                 [prismatic/plumbing "0.3.3"]])

(defproject org.clojars.achim/opt "0.1.0-SNAPSHOT"
  :description "A Clojure library for interfacing with (I)LP solvers"
  :url "https://github.com/achim/opt"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :repositories {"clojars-https" {:url "https://clojars.org/repo"
                                  :username "achim"
                                  :password :env}}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/core.async "0.1.338.0-5c5012-alpha"]
                 [org.clojure/math.numeric-tower "0.0.4"]
                 [org.clojure/math.combinatorics "0.0.8"]
                 [prismatic/plumbing "0.3.3"]
                 [commons-io "2.4"]]
  :native-path "/Library/gurobi563/mac64/lib/"
  :classpath-add "/Library/gurobi563/mac64/lib/gurobi.jar")

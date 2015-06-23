(defproject mvxcvi/withings-clj "0.1.0-SNAPSHOT"
  :description "A Clojure client for the Withings API."
  :url "https://github.com/greglook/withings-clj"
  :license {:name "Public Domain"
            :url "http://unlicense.org/"}

  :deploy-branches ["master"]

  :dependencies
  [[clj-http "1.1.2"]
   [clj-oauth "1.5.2"]
   [clj-time "0.9.0"]
   [me.arrdem/meajure "2.0.0"]
   [org.clojure/clojure "1.7.0-RC1"]
   [org.clojure/tools.logging "0.3.1"]])

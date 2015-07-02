(ns user
  (:require
    (clj-time
      [coerce :as coerce-time]
      [core :as time]
      [format :as format-time])
    [clojure.repl :refer :all]
    [clojure.stacktrace :refer [print-cause-trace]]
    [clojure.string :as str]
    [clojure.tools.namespace.repl :refer [refresh]]
    [meajure.core :as meajure]
    [mvxcvi.withings :as withings]
    [puget.data]))


(puget.data/extend-tagged-value
  org.joda.time.DateTime
  'inst
  (partial format-time/unparse (format-time/formatters :date-time)))


(puget.data/extend-tagged-value
  meajure.core.UnitValue
  'meajure/unit
  #(->> (:units %)
        (mapcat (fn [[k v]] (if (== 1 v) [k] [k v])))
        (cons (:val %))
        (vec)))


(def oauth-creds
  (read-string (slurp "oauth-creds.edn")))


(def client
  (withings/http-client oauth-creds))

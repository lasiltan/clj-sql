(ns clj-sql.resolvers.postgres
  (:require [clj-sql.core :as q]
            [clojure.string :as string])
  (:import (clojure.lang Keyword)))

(defprotocol PsqlArrayValue
  (->psql-array-value [this]))

(extend-type Keyword
  PsqlArrayValue
  (->psql-array-value [this] (name this)))

(extend-type Number
  PsqlArrayValue
  (->psql-array-value [this] this))

(extend-type String
  PsqlArrayValue
  (->psql-array-value [this] (str "\"" this "\"")))

(extend-type Boolean
  PsqlArrayValue
  (->psql-array-value [this] (str this)))

(deftype PsqlArray [coll]
  q/Sql
  (->sql [this]
    (str "'{" (string/join "," (map ->psql-array-value coll)) "}'")))

(def resolvers
  {:contains? (fn [[_ k v]] (str (name k) " ?? " (q/->sql v)))
   :every?    (fn [[_ k v]] (str (name k) " ??& " (q/->sql (->PsqlArray v))))
   :some?     (fn [[_ k v]] (str (name k) " ??| " (q/->sql (->PsqlArray v))))})

(defn add-resolvers []
  (run! (fn [[k f]] (q/add-resolver k f)) resolvers))

(ns clj-sql.core
  (:require [clojure.string :as string])
  (:import (clojure.lang Keyword)))

(defprotocol Sql
  (->sql [this]))

(def resolvers (atom {:=           (fn [[_ k v]] (str (->sql k) "=" (->sql v)))
                      :in          (fn [[_ k vs]] (str (->sql k) " IN (" (string/join "," (map ->sql vs)) ")"))
                      :gt          (fn [[_ k v]] (str (->sql k) " > " (->sql v)))
                      :gte         (fn [[_ k v]] (str (->sql k) " >= " (->sql v)))
                      :lt          (fn [[_ k v]] (str (->sql k) " < " (->sql v)))
                      :lte         (fn [[_ k v]] (str (->sql k) " <= " (->sql v)))
                      :is-null     (fn [[_ k]] (str (->sql k) " IS NULL"))
                      :is-not-null (fn [[_ k]] (str (->sql k) " IS NOT NULL"))
                      :between     (fn [[_ k v1 v2]] (str (->sql k) " BETWEEN " (->sql v1) " AND " (->sql v2)))}))

(defn add-resolver [k f]
  (swap! resolvers assoc k f))

(extend-type Keyword
  Sql
  (->sql [this] (name this)))

(extend-type Number
  Sql
  (->sql [this] this))

(extend-type String
  Sql
  (->sql [this] (str "'" this "'")))

(extend-type Boolean
  Sql
  (->sql [this] (str this)))

(deftype Select [xs]
  Sql
  (->sql [this] (str "SELECT " (->> (first xs)
                                    (map ->sql)
                                    (string/join ", "))
                     " "
                     (string/join " " (map ->sql (rest xs))))))

(deftype From [x]
  Sql
  (->sql [this] (str "FROM " (->> x
                                  (map (fn [[table-name table-alias]]
                                         (cond-> (str (name table-name))
                                           (some? table-alias) (str " " (name table-alias)))))
                                  (string/join ", ")))))

(deftype Join [x]
  Sql
  (->sql [this] (str "JOIN " (letfn [(->join-clause [[table-name table-alias-or-on-clause on-clause]]
                                       (cond-> (str (name table-name))
                                         (nil? on-clause) (str " ON " (->sql table-alias-or-on-clause))
                                         (some? on-clause) (str " " (name table-alias-or-on-clause) " ON " (->sql on-clause))))]
                               (string/join ", " (map ->join-clause x))))))

(deftype Where [x]
  Sql
  (->sql [this] (str "WHERE " (->sql x))))

(deftype And [x]
  Sql
  (->sql [this] (str "(" (string/join " AND " (map ->sql x)) ")")))

(deftype Or [x]
  Sql
  (->sql [this] (str "(" (string/join " OR " (map ->sql x)) ")")))

(deftype Not [x]
  Sql
  (->sql [this] (str "NOT (" (->sql x) ")")))

(deftype Paging [x y]
  Sql
  (->sql [this] (str "OFFSET " (or x 0) " LIMIT " (or y 25))))

(deftype GroupBy [fields]
  Sql
  (->sql [this] (str "GROUP BY " (->> fields
                                      (map ->sql)
                                      (string/join ",")))))

(deftype Order [field-defs]
  Sql
  (->sql [this] (->> field-defs
                     (map #(->> (map ->sql %)
                                (string/join " ")))
                     (string/join ",")
                     (str "ORDER BY "))))

(deftype SubQuery [sql]
  Sql
  (->sql [this] (str "(" (->sql sql) ")")))

(deftype Matcher [x]
  Sql
  (->sql [this] ((get @resolvers (first x)) x)))

(defn select
  "First argument is a list of fields to select in the form: `[:table-alias.field1 :table-alias.field2]`.
   The following optional arguments must extend the type clj-sql.core/Sql"
  [x & others]
  (->Select (concat [x] others)))

(defn from
  "The argument is in the form `[[:table1 :alias1] [:table2 :alias2]]`"
  [x]
  (->From x))

(defn join
  "The argument is in the form `[[:table-to-join :joined-table-alias on-clause]]`.
   The `on-clause` must extend the type clj-sql.core/Sql."
  [x]
  (->Join x))

(defn where
  "The argument must extend the type clj-sql.core/Sql.
    Examples: `(select [:t.id] (from [[:table :t]]) (where (is [:= :t.field \"aaa\"]))`
              `(select [:t.id] (from [[:table :t]]) (where (and (is [:= :t.field1 \"aaa\"])
                                                                (is [:= :t.field2 true])))`"
  [x]
  (->Where x))

(defn and
  "The argument is a vararg list of types that extend the type clj-sql.core/Sql"
  [& x]
  (->And x))

(defn or
  "The argument is a vararg list of types that extend the type clj-sql.core/Sql"
  [& x]
  (->Or x))

(defn not
  "The argument must extend the type clj-sql.core/Sqls"
  [x]
  (->Not x))

(defn paging
  "The arguments are the offset and limit, respectively"
  [^Number x ^Number y]
  (->Paging x y))

(defn group-by
  "The argument is a vararg list of field names"
  [& fields]
  (->GroupBy fields))

(defn order
  "The argument is list of field definitions in the form of `[field direction]`
   Examples: [[:id :desc]] -> ORDER BY id desc
             [[:id] [:title :desc]] -> ORDER BY id, title desc"
  [fields]
  (->Order fields))

(defn sub
  "Sub query, encloses in parentheses.
   The argument is a Select.
   Examples: (sub (select :a (from [[:table]]))")

(defn is
  "The argument is in the form `[matcher-key :field value]`.
   The `matcher-key` must be present in the `resolvers` atom. Call `(add-resolver key resolver-fn)` to add your own.
   The `value` must extend the type clj-sql.core/Sqls"
  [x]
  (->Matcher x))

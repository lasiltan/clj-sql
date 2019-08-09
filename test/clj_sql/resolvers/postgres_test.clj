(ns clj-sql.resolvers.postgres-test
  (:require [clojure.test :refer [deftest testing is]]
            [clj-sql.core :refer [->sql] :as q]
            [clj-sql.resolvers.postgres :as psql]))

(psql/add-resolvers)

(deftest psql-resolvers-contains?
  (testing "resolves string"
    (is (= "field ?? 'aaa'"
           (->sql (q/is [:contains? :field "aaa"])))))

  (testing "resolves number"
    (is (= "field ?? 1"
           (->sql (q/is [:contains? :field 1])))))

  (testing "resolves keyword"
    (is (= "field ?? another_field"
           (->sql (q/is [:contains? :field :another_field])))))

  (testing "resolves boolean"
    (is (= "field ?? true"
           (->sql (q/is [:contains? :field true]))))))

(deftest psql-resolvers-some?
  (testing "resolves strings"
    (is (= "field ??| '{\"aaa\",\"bbb\"}'"
           (->sql (q/is [:some? :field ["aaa" "bbb"]])))))

  (testing "resolves numbers"
    (is (= "field ??| '{1,2}'"
           (->sql (q/is [:some? :field [1 2]])))))

  (testing "resolves keywords"
    (is (= "field ??| '{field1,field2}'"
           (->sql (q/is [:some? :field [:field1 :field2]])))))

  (testing "resolves booleans"
    (is (= "field ??| '{true,false}'"
           (->sql (q/is [:some? :field [true false]]))))))

(deftest psql-resolvers-every?
  (testing "resolves strings"
    (is (= "field ??& '{\"aaa\",\"bbb\"}'"
           (->sql (q/is [:every? :field ["aaa" "bbb"]])))))

  (testing "resolves numbers"
    (is (= "field ??& '{1,2}'"
           (->sql (q/is [:every? :field [1 2]])))))

  (testing "resolves keywords"
    (is (= "field ??& '{field1,field2}'"
           (->sql (q/is [:every? :field [:field1 :field2]])))))

  (testing "resolves booleans"
    (is (= "field ??& '{true,false}'"
           (->sql (q/is [:every? :field [true false]]))))))



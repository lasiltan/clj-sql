(ns clj-sql.core-test
  (:require [clojure.test :refer [deftest testing is]]
            [clj-sql.core :refer [->sql] :as q]))

(deftest select-test
  (testing "handles keywords as fields"
    (is (= "SELECT a, b "
           (->sql (q/select [:a :b]))))))

(deftest from-test
  (testing "handles single table with alias"
    (is (= "FROM table t"
           (->sql (q/from [[:table :t]])))))

  (testing "handles single table without alias"
    (is (= "FROM table"
           (->sql (q/from [[:table]])))))

  (testing "handles multiple tables with aliases"
    (is (= "FROM table t, other o"
           (->sql (q/from [[:table :t] [:other :o]])))))

  (testing "handles multiple tables without aliases"
    (is (= "FROM table, other"
           (->sql (q/from [[:table] [:other]]))))))

(deftest join-test
  (testing "handles joining a table with alias with clause"
    (is (= "JOIN table t ON t.id=o.t_id"
           (->sql (q/join [[:table :t (q/is [:= :t.id :o.t_id])]])))))

  (testing "handles joining a table without alias with clause"
    (is (= "JOIN table ON table.id=other.t_id"
           (->sql (q/join [[:table (q/is [:= :table.id :other.t_id])]]))))))

(deftest equals-matcher-test
  (testing "equals with field reference as value"
    (is (= "t.id=o.t_id"
           (->sql (q/is [:= :t.id :o.t_id])))))

  (testing "equals with number value"
    (is (= "t.id=1"
           (->sql (q/is [:= :t.id 1])))))

  (testing "equals with string value"
    (is (= "t.id='1_2_3'"
           (->sql (q/is [:= :t.id "1_2_3"])))))

  (testing "equals with boolean value"
    (is (= "t.id=true"
           (->sql (q/is [:= :t.id true]))))))

(deftest in-matcher-test
  (testing "in with field references as value"
    (is (= "t.id IN (o.t_id,a.t_id)"
           (->sql (q/is [:in :t.id [:o.t_id :a.t_id]])))))

  (testing "in with number values"
    (is (= "t.id IN (1,2,3)"
           (->sql (q/is [:in :t.id [1 2 3]])))))

  (testing "in with string values"
    (is (= "t.id IN ('aaa','bbb','ccc')"
           (->sql (q/is [:in :t.id ["aaa" "bbb" "ccc"]])))))

  (testing "in with boolean values"
    (is (= "t.id IN (true,false)"
           (->sql (q/is [:in :t.id [true false]]))))))

(deftest is-null-matcher-test
  (is (= "t.id IS NULL"
         (->sql (q/is [:is-null :t.id])))))

(deftest is-not-null-matcher-test
  (is (= "t.id IS NOT NULL"
         (->sql (q/is [:is-not-null :t.id])))))

(deftest between-matcher-test
  (is (= "t.id BETWEEN 1 AND 6"
         (->sql (q/is [:between :t.id 1 6])))))

(deftest full-query-test
  (is (= "SELECT * FROM table WHERE id='1-2-3-4' OFFSET 4 LIMIT 600"
         (->sql (q/select [:*]
                          (q/from [[:table]])
                          (q/where (q/is [:= :id "1-2-3-4"]))
                          (q/paging 4 600)))))

  (is (= "SELECT p.id, p.title, c.category FROM product p JOIN category c ON p.category_id=c.id WHERE (p.title IS NOT NULL AND (p.category_id IN (1,2,3) OR p.category_id=6)) ORDER BY p.id desc"
         (->sql (q/select [:p.id :p.title :c.category]
                          (q/from [[:product :p]])
                          (q/join [[:category :c (q/is [:= :p.category_id :c.id])]])
                          (q/where (q/and (q/is [:is-not-null :p.title])
                                          (q/or (q/is [:in :p.category_id [1 2 3]])
                                                (q/is [:= :p.category_id 6]))))
                          (q/order [[:p.id :desc]]))))))

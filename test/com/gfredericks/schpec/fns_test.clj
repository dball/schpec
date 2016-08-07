(ns com.gfredericks.schpec.fns-test
  (:require [clojure.test :refer :all]
            [com.gfredericks.schpec.fns :refer :all]))

(deftest test-commutative
  (let [f (commutative +)]
    (is (f {:args [1 2]
            :ret 3})))
  (let [f (commutative -)]
    (is (not (f {:args [1 2]
                 :ret -1})))))

(deftest test-associative
  (let [f (associative +)]
    (is (f {:args [1 2 3]
            :ret 6})))
  (let [f (associative -)]
    (is (not (f {:args [1 2 3]
                 :ret -4})))))

(deftest test-has-identity
  (let [f (has-identity + 0)]
    (is (f {:args [1 2]
            :ret 3})))
  (let [f (has-identity + 1)]
    (is (not (f {:args [1 2]
                 :ret 3})))))

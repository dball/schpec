(ns com.gfredericks.schpec.numbers-test
  (:require [clojure.test :refer :all]
            [com.gfredericks.schpec.numbers :refer :all]))

(deftest test-finite
  (is (finite? 2))
  (is (finite? 2.0))
  (is (finite? 2M))
  (is (finite? 2N))
  (is (not (finite? Double/POSITIVE_INFINITY)))
  (is (not (finite? Double/NEGATIVE_INFINITY)))
  (is (not (finite? Double/NaN))))

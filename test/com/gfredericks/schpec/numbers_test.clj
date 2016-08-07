(ns com.gfredericks.schpec.numbers-test
  (:require [clojure.spec :as s]
            [clojure.test :refer :all]
            [com.gfredericks.schpec.numbers :refer :all]))

(deftest test-finite
  (is (finite? 2))
  (is (finite? 2.0))
  (is (finite? 2M))
  (is (finite? 2N))
  (is (not (finite? Double/POSITIVE_INFINITY)))
  (is (not (finite? Double/NEGATIVE_INFINITY)))
  (is (not (finite? Double/NaN))))

(deftest test-bigdec-in
  (let [spec (bigdec-in :min 0M :max 10M :scale 2 :precision 3)]
    (is (s/valid? spec 0M))
    (is (s/valid? spec 0.11M))
    (is (s/valid? spec 1.11M))
    (is (not (s/valid? spec 1.111M)))))

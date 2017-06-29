(ns coding.core-test
  (:require [clojure.test :refer :all]
            [coding.core :refer :all]))

(deftest fib-index-test
  (testing "the first term to contain three digits should be 12."
    (is (= (fib-index 3)
           12))))

(deftest sum-factors-test
  (testing "The sum of all prime numbers below 10 should be 17."
    (is (= (sum-factors 10)
           17))))
  

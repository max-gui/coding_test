(ns coding.core
  (:require [clojure.math.numeric-tower :as math])
  (:gen-class))

(defn fib-pair-value [[a b c]]
  [b (+' a b) (inc c)])

(defn fib-index [len]
  (last (nth
         (filter (fn [[v i]]
                   (> (count (str v))
                      (dec len)))
                 (map (fn [[a b c]] [a c])
                      (iterate fib-pair-value [1 1 1])))
         0)))

(def primes
  (letfn [(enqueue [sieve n step]
            (let [m (+ n step)]
              (if (sieve m)
                (recur sieve m step)
                (assoc sieve m step))))
          (next-sieve [sieve candidate]
            (if-let [step (sieve candidate)]
              (-> sieve
                (dissoc candidate)
                (enqueue candidate step))
              (enqueue sieve candidate (+ candidate candidate))))
          (next-primes [sieve candidate]
            (if (sieve candidate)
              (recur (next-sieve sieve candidate) (+ candidate 2))
              (cons candidate
                (lazy-seq (next-primes (next-sieve sieve candidate)
                            (+ candidate 2))))))]
    (cons 2 (lazy-seq (next-primes {} 3)))))

(defn sum-factors [n]
  (reduce + (take-while #(< % n) primes )))

(defn -main
  "This should be pretty simple."
  []
  (println "the index of the first term in the Fibonacci sequence to contain 1000 digits is" (fib-index 1000))
  (println "the sum of all prime numbers below 2,000,000 is " (sum-factors 2000000)))

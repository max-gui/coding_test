(ns coding.core
  (:require [clojure.math.numeric-tower :as math])
  (:gen-class))

(defn fib-iter [a b p q count]
  (cond
    (= count 0) b
    (even? count)    (recur a
                            b
                            (+ (* p p) (* q q))
                            (+ (* 2 p q) (* q q))
                            (/ count 2N))
    :else (recur (+ (* b q) (* a q) (* a p))
                  (+ (* b p) (* a q))
                  p
                  q
                  (- count 1N))))
                
(defn fib [n]
  (fib-iter 1N 0N 0N 1N n))

(defn fib-index-math [len]
  (cond (= len 1) 1
        (< len 1) 0
        :else ((fn [pre_index, index, len]
                 (let [current-len (count (str (fib index)))
                       hit (= (- index pre_index)
                              1)]
                   (cond (and (= current-len len)
                              hit) index
                         (< current-len len) (recur index (+ index index) len)
                         :else (recur pre_index
                                      (quot (+ pre_index index)
                                            2)
                                      len))))
               2, 4, len)))
      
(defn fib-pair-value [[a b c]]
  [b (+' a b) (inc c)])

(defn fib-by-pair [n]
  (first
   (nth
    (iterate fib-pair-value [1 1 1])
    (dec n))))

(defn fib-index [len]
  (inc
   (last (last
          (take-while (fn [[v _ _]]
                        (< (count (str v))
                           len))
                      (iterate fib-pair-value [1 1 1]))))))

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
  (println "the index of the first term in the Fibonacci sequence to contain 1000 digits is" (fib-index-math 1000))
  (println "the sum of all prime numbers below 2,000,000 is " (sum-factors 2000000)))

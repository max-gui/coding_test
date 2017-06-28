(ns hello-world.handler
  (:require [clojure.math.numeric-tower :as math]))

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

(defn sieve
  [[p & rst]]
  ;; make sure the stack size is sufficiently large!
  (lazy-seq (cons p (sieve (filter #(= 0 (mod % p)) rst)))))


(def primes (sieve (iterate inc 2)))

(defn sum-factors [n]
  (reduce + (filter #(< % n) primes )))

(defn sum-factors-new [n]
  (reduce + (take-while #(< % n) primes-new )))


                   
(defn lazy-primes3 []
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

 (defn non-trivial-sqrt? [n m]
   (cond (= n 1) false 
         (= n (- m 1)) false
         ; book reads: whose square is equal to 1 modulo n 
         ; however, what was meant is square is congruent 1 modulo n 
         :else (= (rem (math/sqrt n) m) 1))) 

 (defn expmod [base exp m]
   (cond ((= exp 0) 1) 
         ((even? exp) 
          (let [x (expmod base (/ exp 2) m)]
            (if (non-trivial-sqrt? x m) 0 (rem (math/sqrt x) m))) 
          :else 
             (rem (* base (expmod base (- exp 1) m)) 
                        m)))) 
  
(defn miller-rabin-test [a n] 
   (cond (= a 0) true
         ; expmod is congruent to 1 modulo n 
         (= (expmod a (- n 1) n) 1) (miller-rabin-test (- a 1) n)
         :else false)) 
  

(defn miller-rabin [n]
   (miller-rabin-test (- n 1) n)) 
  
(defn mill-rabin-expmod [base exp m]
  "base^exp mod m adapted for use in the Miller-Rabin test"
  (cond (= exp 0) 1
        (even? exp) (let [itr (mill-rabin-expmod base (/ exp 2) m)
                          sqr (math/expt itr 2)]
                      (if (and (not= itr 1)
                               (not= itr (dec m))
                               (= sqr (mod 1 m)))
                        0
                        (mod sqr m)))
        :else (mod (* base (mill-rabin-expmod base (dec exp) m))
                   m)))

(defn mr-fermat-test [n]
  (let [rand (int (inc (rand (dec n))))]
    (= (mill-rabin-expmod rand (dec n) n) (mod 1 n))))

(defn mr-fast-prime? [n times]
  (every? mr-fermat-test
          (take times (repeat n))))

(def primes-new (filter mr-fermat-test (iterate inc 2)))

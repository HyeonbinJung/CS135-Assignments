;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname prime-fun) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;
;; ***********************************************
;; Hyeonbin Jung (21128068)            
;; CS 135 Fall 2025
;; Assignment 09, Problem 4
;; ***********************************************

;;---------------------------------------------------------------------------------------------------

;; Problem 4 (a)

;; prime?: Nat -> Bool
;; Purpose: consumes natural number and produce true if it is a prime otherwise, false.

(define (prime? n)
  (cond [(< n 2) false]
        [else
         (foldr
          (lambda (d acc)
            (and acc (not (= (remainder n d) 0))))
          true
          (build-list (sub1 (sub1 n)) (lambda (k) (+ k 2))))]))

;; Test cases
(check-expect (prime? 13) true)
(check-expect (prime? 14) false)
(check-expect (prime? 2) true)
(check-expect (prime? 1) false)
(check-expect (prime? 0) false)

;; Problem 4 (b)

;; list-primes: Nat -> (listof Nat)
;; Purpose: Consume a natural number n and produce a list consist of prime numbers
;; less than n or equal to n
(define (list-primes n)
  (filter prime?
          (build-list (sub1 n)
                      (lambda (k) (+ k 2)))))

;; Test cases
(check-expect (list-primes 10) (list 2 3 5 7))
(check-expect (list-primes 53)
(list 2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53))
(check-expect (list-primes 2) (list 2))


;; Problem 4 (c)

;; gb: Nat -> (listof (listof Nat))
;; Purpose: consumes a natural number n (with n >= 4) and produces a list of pairs
;; natural number in pair should be prime, and the sum of them should be n
;; first number in pair is small or equal to second number in pair

(define (gb n)
  (filter (lambda (p)
            (cond [(list? p) true]
                  [else false]))
          (map
           (lambda (x)
             (cond [(and (prime? x)
                         (prime? (- n x))
                         (<= x (- n x)))
                    (list x (- n x))]
                   [else false]))
           (build-list (quotient n 2)
                       (lambda (k) (+ k 2))))))

;; Test cases
(check-expect (gb 4) (list (list 2 2)))
(check-expect (gb 10) (list (list 3 7) (list 5 5)))
(check-expect (gb 9) (list (list 2 7)))
(check-expect (gb 11) empty)




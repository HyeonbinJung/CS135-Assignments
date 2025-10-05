;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname recursion) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;; ***********************************************
;; Hyeonbin Jung (21128068)            
;; CS 135 Fall 2025
;; Assignment 03, Problem 2 
;; ***********************************************
;;
;; 
;; Question 2(a): Write a function harmonic that consumes a natural number n and produces Hn.
;; Determine Hn = 1 + 1/2 + ... + 1/n with H0 = 0
;;
;; Purpose: Consume n as n-th, Produce the n-th harmonic number Hn as an exact rational.
;; Contract:
;;   harmonic: Nat -> Rat
;; Requires:
;;   n is Nat   

(define (harmonic n)
  (cond
    [(zero? n) 0]
    [else (+ (/ 1 n) (harmonic (- n 1)))]))

;; Test cases
(check-expect (harmonic 0) 0) ; H0 = 0
(check-expect (harmonic 1) 1) ; H1 = 1
(check-expect (harmonic 2) (/ 3 2))    ; value of H2 = 1 + 1/2 = 3/2
(check-expect (harmonic 4) (/ 25 12))  ; value of H3 = 1 + 1/2 + 1/3 + 1/4 = 25/12

;; 
;; Question 2(b): Write a function ss-exact that consumes a natural number n and uses the formula on
;; the right to compute the sum of squares.
;; Write a function ss-recursive that consumes a natural number and uses recursion on
;; the natural numbers to compute the sum of squares.
;;
;; Purpose:
;;   Consume n as n-th, Produce the sum of 0^2 to n^2 
;; Contracts:
;;   ss-exact:     Nat -> Nat
;;   ss-recursive: Nat -> Nat
;; Requires:
;;   n is Nat 

;; n(n+1)(2n+1)/6
(define (ss-exact n)
  (/ (* n (+ n 1) (+ (* 2 n) 1)) 6))

;; F(n) = n^2 + F(n-1),  F(0) = 0 which has equal result as n(n+1)(2n+1)/6
(define (ss-recursive n)
  (cond
    [(zero? n) 0]
    [else (+ (* n n) (ss-recursive (- n 1)))]))

;; Test cases
(check-expect (ss-exact 0) 0)
(check-expect (ss-recursive 0) 0)

(check-expect (ss-exact 1) 1)
(check-expect (ss-recursive 1) 1)

(check-expect (ss-exact 5) 55)
(check-expect (ss-recursive 5) 55)

(check-expect (ss-exact 10) 385)
(check-expect (ss-recursive 10) 385)

(check-expect (ss-recursive 0) (ss-exact 0))
(check-expect (ss-recursive 1) (ss-exact 1))
(check-expect (ss-recursive 5) (ss-exact 5))
(check-expect (ss-recursive 10) (ss-exact 10))
(check-expect (ss-recursive 7) (ss-exact 7))
(check-expect (ss-recursive 999) (ss-exact 999))
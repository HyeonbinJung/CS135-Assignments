;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname wythoff) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;
;; ***********************************************
;; Hyeonbin Jung (21128068)            
;; CS 135 Fall 2025
;; Assignment 07, Problem 4
;; ***********************************************

;;---------------------------------------------------------------------------------------------------

;; Question 4: Write (wythoff m n) which consumes the size of the piles and
;; produces a symbol indicating the winner.

;; Purpose: consume two natural numbers m and n representing the sizes of the piles
;; and produces a symbol ('Alice or 'Bob) indicating which player will win
;; Contract: Nat Nat -> Sym

(define (wythoff m n)
  (alice m n))

(define (alice m n)
  (cond
    [(or (zero? m) (zero? n) (= m n)) 'Alice]
    [(> m n) (bob (sub1 m) n)]
    [else (bob m (sub1 n))]))

(define (bob m n)
  (cond
    [(or (zero? m) (zero? n) (= m n)) 'Bob]
    [else (alice (sub1 m) (sub1 n))]))

;; Test cases
(check-expect (wythoff 7 3) 'Alice)
(check-expect (wythoff 19 10) 'Bob)
(check-expect (wythoff 17 30) 'Bob)
(check-expect (wythoff 0 5) 'Alice)  
(check-expect (wythoff 5 0) 'Alice)
(check-expect (wythoff 1 1) 'Alice)
(check-expect (wythoff 6 4) 'Bob)

  
    
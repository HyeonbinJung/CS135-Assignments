;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname horner) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;; ***********************************************
;; Hyeonbin Jung (21128068)            
;; CS 135 Fall 2025
;; Assignment 04, Problem 5
;; ***********************************************



;;---------------------------------------------------------------------------------------------------
;; Question 5: Write a function eval-poly

;; function eval-poly
;; Purpose: consumes a list of numbers (representing the coefficients of a polynomial)
;; and a value for x,
;; produces the result of evaluating the given polynomial at the given value of x.

;; Contract: (listof Num) Num -> Num

(define (eval-poly coeffs x)
  (cond
    [(empty? coeffs) 0]
    [(empty? (rest coeffs)) (first coeffs)]
    [else (+ (first coeffs) (* x (eval-poly (rest coeffs) x)))]
    )
  )

;; Test Cases
(check-expect (eval-poly (cons 1.4 (cons 4 (cons 0 (cons 2 empty)))) 3) 67.4)
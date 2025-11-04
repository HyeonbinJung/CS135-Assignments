;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname minor) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;
;; ***********************************************
;; Hyeonbin Jung (21128068)            
;; CS 135 Fall 2025
;; Assignment 06, Problem 5
;; ***********************************************

;;---------------------------------------------------------------------------------------------------

;; Question 5: Write a function minor that inputs A, i, and j, and outputs the (i, j)th minor of A.

;; Purpose: consume an m×n matrix, A and i and j
;; produce new matrix which the ith list of A
;; and the jth number from each of the m-1 remaining lists of A are deleted
;; Contract: (listof (listof Num)) Nat Nat -> (listof (listof Num))

(define (remove-i i lst)
  (cond
    [(empty? lst) empty]
    [(= i 1) (rest lst)]
    [else (cons (first lst)
                (remove-i (sub1 i) (rest lst)))]))

(define (remove-j-from-all j matrix)
  (cond
    [(empty? matrix) empty]
    [else
     (cons (remove-i j (first matrix))
           (remove-j-from-all j (rest matrix)))]))


(define (minor A i j)
  (remove-j-from-all j (remove-i i A)))

(define A (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12)))
(define B (list (list 15 16) (list 17 18)))
(check-expect (minor A 1 1) (list (list 5 6) (list 8 9) (list 11 12)))
(check-expect (minor A 2 3) (list (list 1 2) (list 7 8) (list 10 11)))
(check-expect (minor B 2 1) (list (list 16)))
(check-expect (minor B 1 2) (list (list 17)))
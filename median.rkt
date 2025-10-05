;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname median) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;; ***************************************************
;; Hyeonbin Jung (21128068)
;; CS 135 Fall 2025
;; Assignment 02, Problem 3
;; ***************************************************
;;
;;
;; Question 3)
;; Purpose: Find the median of three numbers a, b, and c
;; Contract: median-of-3-simple : Num Num Num -> Num
;; Consumes 3 numbers, Produces median of the three numbers
(define (median-of-3-simple a b c)
  (cond
    [(< a b)
     (cond
       [(< b c) b]     
       [(< a c) c]      
       [else a])]       
    [(or (< b a) (= a b))               
     (cond
       [(< a c) a]      
       [(< b c) c]     
       [else b])]))     

;; Test cases
(check-expect (median-of-3-simple 1 2 3) 2)
(check-expect (median-of-3-simple 2 1 3) 2)
(check-expect (median-of-3-simple 3 1 2) 2)
(check-expect (median-of-3-simple 1 3 2) 2)
(check-expect (median-of-3-simple 2 3 1) 2)
(check-expect (median-of-3-simple 3 2 1) 2)
(check-expect (median-of-3-simple 2 2 3) 2)
(check-expect (median-of-3-simple 2 3 2) 2)
(check-expect (median-of-3-simple 3 2 2) 2)
(check-expect (median-of-3-simple 2 2 2) 2)
(check-expect (median-of-3-simple 3 5 7) 5)
(check-expect (median-of-3-simple 5 3 7) 5)
(check-expect (median-of-3-simple 5 5 5) 5)
;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname matrices) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;
;; ***********************************************
;; Hyeonbin Jung (21128068)            
;; CS 135 Fall 2025
;; Assignment 09, Problem 3
;; ***********************************************

;;---------------------------------------------------------------------------------------------------

;; Problem 3 (a)

;; Helper Function
(define (index n lst)
(first (foldl (lambda (x y) (rest y)) lst
(build-list (sub1 n) +))))

;; extract-column : (listof (listof Any)) Nat -> (listof Any)
;; Purpose: consumes matrix A and index j and producess the jth column.

(define (extract-column A j)
  (map (lambda (row)
         (index j row))
       A))

;; Test cases
(define A (list (list 1 2 3) (list 4 5 6)
(list 7 8 9) (list 10 11 12)))
(check-expect (extract-column A 1) (list 1 4 7 10))
(check-expect (extract-column A 2) (list 2 5 8 11))


;; Problem 3 (b)

;; Helper Function
(define (dot lst1 lst2)
  (foldr + 
         0
         (map (lambda (x y) (* x y))
              lst1
              lst2)))

;; matrix-multiply : (listof (listof Num)) (listof (listof Num)) -> (listof (listof Num))
;; Purpose: consumes matrix A and B and produces AB

(define (matrix-multiply A B)
  (map (lambda (rowA)
         (map (lambda (j)
                (dot rowA (extract-column B j)))
              (build-list (length (first B))
                          (lambda (k) (add1 k)))))
       A))

;; Test cases
(define B (list (list 1 0) (list 0 1) (list 1 1)))
(check-expect (matrix-multiply A B)
(list (list 4 5)
(list 10 11)
(list 16 17)
(list 22 23)))
(define C (list (list 1 1) (list 1 1)))
(define D (list (list 1 2) (list 3 4)))
(check-expect (matrix-multiply C D) (list (list 4 6) (list 4 6)))
(check-expect (matrix-multiply D C) (list (list 3 3) (list 7 7)))
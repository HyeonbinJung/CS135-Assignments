;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname faster) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;
;; ***********************************************
;; Hyeonbin Jung (21128068)            
;; CS 135 Fall 2025
;; Assignment 05, Problem 2
;; ***********************************************

;;---------------------------------------------------------------------------------------------------

;; Question 2: Rewrite the function to make it faster. 
;; Function: count-types-fast
;; Purpose: consume a list and produce a list with two elements.
;; The first element is the number of symbols in the list. The second element is the number of
;; non-symbols in the list. (The purpose is same as count-types but fast)
;; Contract: (listof Any) -> (list Nat Nat)

(define (count-types-fast lst)
  (helper lst 0 0))

(define (helper lst count-sym count-non-sym)
  (cond
    [(empty? lst)
     (list count-sym count-non-sym)]
    [(symbol? (first lst))
     (helper (rest lst) (add1 count-sym) count-non-sym)]
    [else
     (helper (rest lst) count-sym (add1 count-non-sym))]
    )
  )
;; Test cases for helper
(check-expect (helper empty 0 0) (list 0 0))
(check-expect (helper (list 'a 'b 'c) 0 0) (list 3 0))
(check-expect (helper (list 'a 1 'b 2 'c 3) 0 0) (list 3 3))

;; For test function
(define (alt-test n)
  (cond [(zero? n) empty]
        [else (cons 'a (cons 1 (alt-test (sub1 n))))]))

;; Test cases
(check-expect (count-types-fast (list 'a 1 'b 2 'c 3)) (list 3 3))
(check-expect (count-types-fast (list 'x 'y 'z)) (list 3 0))
(check-expect (count-types-fast (list 1 2 3)) (list 0 3))
(check-expect (count-types-fast empty) (list 0 0))
(check-expect (count-types-fast (alt-test 12)) (list 12 12))

;; Speed test
(check-expect (count-types-fast (alt-test 100000)) (cons 100000 (cons 100000 empty)))
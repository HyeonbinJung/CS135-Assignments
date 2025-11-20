;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname arranged) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;
;; ***********************************************
;; Hyeonbin Jung (21128068)            
;; CS 135 Fall 2025
;; Assignment 08, Problem 3
;; ***********************************************

;;---------------------------------------------------------------------------------------------------

;; arranged?: (list (X -> Bool) (X X -> Bool)) (listof X) -> Bool
;; Purpose: consumes a list containing a predicate and a binary relational operator, a list of values
;; produces true exactly when all values satisfy the predicate and
;; each consecutive pair satisfies the relational operator, otherwise false.

(define (arranged? pf-and-op ops)
  (local
    [(define ok? (first pf-and-op))           
     (define ordered? (second pf-and-op))    
     (define (all-ok? lst)
       (cond [(empty? lst) true]
             [(ok? (first lst)) (all-ok? (rest lst))]
             [else false]))
     (define (ordered-list? lst)
       (cond [(or (empty? lst)
                  (empty? (rest lst)))
              true]
             [(ordered? (first lst) (first (rest lst)))
              (ordered-list? (rest lst))]
             [else false]))]

    (cond
      [(empty? ops) true]
      [(empty? (rest ops))
       (ok? (first ops))]
      [(not (all-ok? ops)) false]
      [else (ordered-list? ops)])))


;; Test cases
(check-expect (arranged? (list integer? <) (list)) true)
(check-expect (arranged? (list integer? >) (list 1)) true)
(check-expect (arranged? (list integer? >) (list 'red)) false)
(check-expect (arranged? (list string? >) (list "wow" 'red)) false)
(check-expect (arranged? (list string? string>?)
(list "wow" "cs135" "amazing"))
true)

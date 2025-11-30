;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname super) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;
;; ***********************************************
;; Hyeonbin Jung (21128068)            
;; CS 135 Fall 2025
;; Assignment 09, Problem 2
;; ***********************************************

;;---------------------------------------------------------------------------------------------------

;; Problem 2 (a)

;; Purpose: consumes a predicate and a (listof Any), and produces a
;; (listof Any) that generalizes filter to work on an arbitrarily nested list applying the
;; predicate to filter the atoms in each nested list.
;; super-filter : (Any -> Bool) (listof Any) -> (listof Any)

(define (super-filter pred? lst)
  (foldr (lambda (x acc)
           (cond [(list? x)
                  (cons (super-filter pred? x) acc)]
                 [(pred? x)
                  (cons x acc)]
                 [else
                  acc]))
         empty
         lst))

;; Test cases
(define L (list 1 (list 2 (list 2 3 4) 5 6 (list 7 8 9)) 10 11 12))

(check-expect (super-filter odd? L)
(list 1 (list (list 3) 5 (list 7 9)) 11))

(check-expect (super-filter (lambda (x) (>= x 9)) L)
(list (list empty (list 9)) 10 11 12))

(check-expect (super-filter (lambda (x) (>= x 11)) L)
(list (list empty empty) 11 12))


;; Problem 2 (b)

;; ruthless : (listof Any) -> (listof Any)
;; Purpose: consumes a list and produces a list with all symbol 'ruth removed

(define (ruthless lst)
  (super-filter (lambda (x) (not (symbol=? x 'ruth))) lst))

;; Test cases
(define M (list 'ruth 'chris 'steakhouse
                (list 'ruth 'handler 'created 'what
                      (list 'babe 'ruth 'blue)
                      (list 'ruth 'ruth)
                      'ruth 'bader 'ginsberg)
                'hello))

(check-expect (ruthless M)
              (list 'chris 'steakhouse
                    (list 'handler 'created 'what
                          (list 'babe 'blue)
                          empty
                          'bader 'ginsberg)
                    'hello))


;; Problem 2 (c)

;; supersize : Nat (listof Any) -> (listof Any)
;; Purpose: consumes a nested list and
;; prudce a new nested list with all numbers less than natural number n removed
(define (supersize n lst)
  (super-filter (lambda (x) (>= x n)) lst))



;; Test cases
(check-expect (supersize 4 (list 8 1 (list 2 6 3) 10 1))
(list 8 (list 6) 10))
(check-expect (supersize 200 (list 8 5 (list 2 6 3) 10 1))
(list empty))
(check-expect (supersize 200 (list 5 6 7)) empty)


;; Problem 2 (d)

;; super-keeper : (Any -> Bool) (listof Any) -> (listof Any)
;; Purpose: consumes a predicate pred? and a (nested) list, and
;; produces a new list of the same nested structure that contains only
;; those atoms for which pred? returns false.
(define (super-keeper pred? lst)
  (super-filter (lambda (x) (not (pred? x))) lst))

;; Test cases
(check-expect
(super-keeper
odd?
(list 1 (list 2 (list 2 3 4) 5 6 (list 7 8 9)) 10 11 12))
(list (list 2 (list 2 4) 6 (list 8)) 10 12))



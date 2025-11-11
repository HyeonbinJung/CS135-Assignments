;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname trees) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;
;; ***********************************************
;; Hyeonbin Jung (21128068)            
;; CS 135 Fall 2025
;; Assignment 07, Problem 1
;; ***********************************************

;;---------------------------------------------------------------------------------------------------

;; Helper Functions
(define (mk-node label left right)
  (list label left right))

(define (get-label bt)
  (first bt))

(define (get-left bt)
  (second bt))

(define (get-right bt)
  (third bt))

;; Definition
;; A binary tree (BT) is one of
;; * empty
;; * (list Any BT BT)

;; Example Binary tree for test cases
(define example
(list 6
(list 3
(list 2 empty empty)
(list 45 empty empty))
(list 10 empty
(list 3
(list 9 empty empty)
(list 5 empty empty)))))

      
;; Question 1 (a): Write a function bt-mirror that consumes a binary tree and produces a new
;; binary tree that is the mirror image of the original

;; Purpose: Consume binary tree
;; produces a new binary tree that is the mirror image of the original
;; Contract: BT -> BT

(define (bt-mirror bt)
  (cond
    [(empty? bt) empty] 
    [else
     (list (first bt)
           (bt-mirror (get-right bt)) 
           (bt-mirror (get-left bt)))]))

;; Test cases
(check-expect (bt-mirror empty) empty)
(check-expect (bt-mirror example)
(list 6
(list 10
(list 3
(list 5 empty empty)
(list 9 empty empty))
empty)
(list 3
(list 45 empty empty)
(list 2 empty empty))))


;; Question 1 (b): Write a function bt-sum that consumes a binary tree of numbers and produces
;; the sum of all labels in the tree.

;; Purpose: Consume binary tree of numbers and produces the sum of all labes in the tree.
;; Contract: BT -> Num

(define (bt-sum bt)
  (cond
    [(empty? bt) 0]
    [else
     (+ (get-label bt) (bt-sum (get-left bt)) (bt-sum (get-right bt)))]
    )
  )

;; Test cases
(check-expect (bt-sum empty) 0)
(check-expect (bt-sum example) 83)


;; Question 1 (c): Write a function bt-height that consumes a binary tree and produces its height.

;; Purpose: Consumes a binary tree and produces its height.
;; Contract: BT -> Nat

(define (bt-height bt)
  (cond
    [(empty? bt) 0]
    [else
     (+ 1 (max (bt-height (get-left bt))
               (bt-height (get-right bt))))]))

;; Test cases
(check-expect (bt-height empty) 0)
(check-expect (bt-height example) 4)
(check-expect (bt-height (list 5 empty empty)) 1)



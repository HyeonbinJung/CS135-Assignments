;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname bsts) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;
;; ***********************************************
;; Hyeonbin Jung (21128068)            
;; CS 135 Fall 2025
;; Assignment 07, Problem 2
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

;; Data definition
;; A binary search tree (BST) is one of
;; * empty
;; * (list Num BST BST)
;; Requires: label is > than labels in left subtree
;; and label < labels in right subtree

;; Example bst
(define bst-example
(list 6
(list 3
(list 2 empty empty)
empty)
(list 10
(list 9 empty empty)
(list 45 empty empty))))

;; Question 2 (a): Write a function bst->list that consumes a binary search tree and produces a
;; list of all labels using in-order traversal (left subtree, root, right subtree)

;; Purpose: Consumes a binary search tree and produces a list of all labels using in-order traversal
;; Contract: BST -> (listof Num)

(define (bst->list bst)
  (cond
    [(empty? bst) empty] 
    [else
     (append (bst->list (get-left bst)) 
             (list (get-label bst))      
             (bst->list (get-right bst)))]))

;; Test cases
(check-expect (bst->list empty) empty)
(check-expect (bst->list bst-example) (list 2 3 6 9 10 45))

;; Question 2 (b): Write a predicate valid-bst? that consumes a binary tree of numbers (see the
;; data definition in Q1) and produces true if it satisfies the BST ordering property, false
;; otherwise.

;; Purpose: Consumes a binary tree of numbers
;; produces true if it satisfies the BST ordering property, false otherwise.
;; Contract: BST -> Bool

(define (valid-bst? bt)
  (increasing? (bst->list bt)))

(define (increasing? lst)
  (cond
    [(or (empty? lst) (empty? (rest lst))) true]
    [else
     (and (< (first lst) (second lst))
          (increasing? (rest lst)))]))

;; Test cases
(check-expect (valid-bst? empty) true)
(check-expect (valid-bst? bst-example) true)
(check-expect (valid-bst? (list 5 (list 7 empty empty)
(list 3 empty empty))) false)
(check-expect (valid-bst? (list 10
(list 5 (list 15 empty empty) empty)
empty)) false)
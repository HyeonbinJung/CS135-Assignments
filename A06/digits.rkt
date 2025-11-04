;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname digits) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;
;; ***********************************************
;; Hyeonbin Jung (21128068)            
;; CS 135 Fall 2025
;; Assignment 06, Problem 3
;; ***********************************************

;;---------------------------------------------------------------------------------------------------

;; Question 3: Write a function digit-groups that consumes a string and
;; produces a list with two elements.

;; Purpose: consumes a string and produces a list with two elements
;; • The first element of the produced list indicates the number of round digits in the string.
;; • The second element indicates the number of non-round digits in the string.
;; Contract: Str -> (list Nat Nat)

(define (digit-groups str)
  (digit-groups/list (string->list str)))

(define (digit-groups/list loc)
  (cond
    [(empty? loc)
     (list 0 0)]

    [(or (char<? (first loc) #\0)
         (char>? (first loc) #\9))
     (digit-groups/list (rest loc))]

    [(or (char=? (first loc) #\0)
         (char=? (first loc) #\6)
         (char=? (first loc) #\8)
         (char=? (first loc) #\9))
     (list (add1 (first (digit-groups/list (rest loc))))
           (second (digit-groups/list (rest loc))))]

    [else
     (list (first (digit-groups/list (rest loc)))
           (add1 (second (digit-groups/list (rest loc)))))]))


;; Test cases
(check-expect (digit-groups "B2-809!") (list 3 1))
(check-expect (digit-groups "Room 101") (list 1 2))
(check-expect (digit-groups "No numbers here") (list 0 0))
(check-expect (digit-groups "") (list 0 0))
(check-expect (digit-groups "12345") (list 0 5))


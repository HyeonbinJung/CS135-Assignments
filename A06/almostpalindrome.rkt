;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname almostpalindrome) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;
;; ***********************************************
;; Hyeonbin Jung (21128068)            
;; CS 135 Fall 2025
;; Assignment 06, Problem 4
;; ***********************************************

;;---------------------------------------------------------------------------------------------------

;; Question 4: Write a predicate almost-palindrome? that consumes a string and produces true exactly
;; when it is an almost-palindrome. (You may want to first write a predicate palindrome? that
;; produces true exactly when the input is a palindrome.)

;; Purpose: consume str and produce true if str is almost palindrome, otherwise, false
;; Contract: Str -> Bool

;; produces true if deleting one char can make str a palindrome
(define (almost-palindrome? str)
  (cond
    [(string=? str "") false]
    [(palindrome? str) true]
    [else
     (almost-helper (string->list str) 0)]))

;; produces true if the string reads the same forwards and backwards
(define (palindrome? str)
  (cond
    [(string=? (list->string (reverse (string->list str))) str) true]
    [else false]))

;; removes the nth element from a list
(define (remove-nth lst n)
  (cond
    [(empty? lst) empty]
    [(zero? n) (rest lst)]
    [else (cons (first lst)
                (remove-nth (rest lst) (sub1 n)))]))

;; (When it is not perfectly palindrome, but almost)
;; if it is palindrome when one element of the list (converted str)
;; it is considered almost palindrome.
(define (almost-helper loc n)
  (cond
    [(>= n (length loc)) false]
    [(palindrome? (list->string (remove-nth loc n))) true]
    [else (almost-helper loc (add1 n))]))

;; Test cases
(check-expect (almost-palindrome? "racecar") true)
(check-expect (almost-palindrome? "xy") true)
(check-expect (almost-palindrome? "z") true)
(check-expect (almost-palindrome? "") false)
(check-expect (almost-palindrome? "abbccd") false)
(check-expect (almost-palindrome? "aabbcdbbaa") true)
;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname weekday) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;; ***************************************************
;; Hyeonbin Jung (21128068)
;; CS 135 Fall 2025
;; Assignment 02, Problem 5
;; ***************************************************
;;
;;
;; Question 5)
;; Purpose: Find the day-of-week thorugh year, month, date of month
;; Consume a natural number YYYYMMDD and produce day-of-week.
;; Contract: date->day-of-week : Nat -> Week-Day
;; Requires: Gregorian date in form YYYYMMDD.
;; Week-Day is (anyof 'Monday 'Tuesday 'Wednesday 'Thursday 'Friday 'Saturday 'Sunday)
;; Algorithms cited: Zeller`s Congruence(http://datagenetics.com/blog/november12019/index.html)
(define (convert-janfeb-month n)
  (cond [(< (remainder (floor (/ n 100)) 100) 3) (+ (remainder (floor (/ n 100)) 100) 12)]
        [else (remainder (floor (/ n 100)) 100)]))

(define (convert-janfeb-year n)
  (cond [(< (remainder (floor (/ n 100)) 100) 3) (- (floor (/ n 10000)) 1)]
        [else (floor (/ n 10000))]))

(define (year-of-century n) (remainder (convert-janfeb-year n) 100))
(define (century n) (floor (/ (convert-janfeb-year n) 100))) 

(define (zeller-formula n)
  (modulo (+ (remainder n 100)
             (floor (/ (* 13 (+ (convert-janfeb-month n) 1)) 5))
             (year-of-century n)
             (floor (/ (year-of-century n) 4))
             (floor (/ (century n) 4))
             (* 5 (century n)))
          7))

(define (date->day-of-week n)
    (cond [(= (zeller-formula n) 0) 'Saturday]
        [(= (zeller-formula n) 1) 'Sunday]
        [(= (zeller-formula n) 2) 'Monday]
        [(= (zeller-formula n) 3) 'Tuesday]
        [(= (zeller-formula n) 4) 'Wednesday]
        [(= (zeller-formula n) 5) 'Thursday]
        [(= (zeller-formula n) 6) 'Friday]))
  
;; Test cases
(check-expect (date->day-of-week 20250923) 'Tuesday)  ;
(check-expect (date->day-of-week 20240924) 'Tuesday)
(check-expect (date->day-of-week 38781202) 'Monday)  

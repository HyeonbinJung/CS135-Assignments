;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname speed) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;; ***************************************************
;; Hyeonbin Jung (21128068)
;; CS 135 Fall 2025
;; Assignment 02, Problem 2
;; ***************************************************
;;
;;
;; Question 2a)
;; Purpose: Convert a speed to miles per hour (mph) from metres per second (m/s).
;; Contract: m/s->mph: Num -> Num
;; Consumes a speed in m/s and produces the same speed, which is converted to mph.

(define metres-per-mile 1609.344)
(define seconds-per-hour 3600)

(define (m/s->mph speed)
  (* speed (/ seconds-per-hour metres-per-mile)))

;; Test cases
(check-expect (m/s->mph 0) 0)         
(check-expect (m/s->mph 1) (/ 3600 1609.344))
(check-expect (m/s->mph 1609.344) 3600)

;; Question 2b)
;; Purpose: Convert a speed to Smoots per millifortnight (S/mfn) from miles per hour (mph) .
;; Contract: mph->s/mfn: Num -> Num
;; Consumes a speed in mph and produces the converted speed, which is converted to S/mfn.

(define metres-per-smoot 1.7018)
(define seconds-per-mfn 1209.6)

(define (mph->s/mfn speed)
  (* speed
     (/ (* metres-per-mile (/ 1 metres-per-smoot))  
        (* seconds-per-hour (/ 1 seconds-per-mfn))))) 

;; Test cases
(check-expect (mph->s/mfn 0) 0) 
(check-expect (mph->s/mfn 1) (/ 532224 1675)) 
(check-expect (mph->s/mfn 60) (* 60 (/ 532224 1675))) 
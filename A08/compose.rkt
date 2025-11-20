;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname compose) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;
;; ***********************************************
;; Hyeonbin Jung (21128068)            
;; CS 135 Fall 2025
;; Assignment 08, Problem 4
;; ***********************************************

;;---------------------------------------------------------------------------------------------------

;; compose-f-g: (X -> X) (X -> X) -> (X -> X)
;; Purpose: consumes functions f and g and produces a new function.

(define (compose-f-g f g)
  (local
    [(define (h n) (f (g n)))] h)
  )

(check-expect ((compose-f-g - add1) 10) -11)

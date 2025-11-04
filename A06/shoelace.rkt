;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname shoelace) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;
;; ***********************************************
;; Hyeonbin Jung (21128068)            
;; CS 135 Fall 2025
;; Assignment 06, Problem 1
;; ***********************************************

;;---------------------------------------------------------------------------------------------------

;; Question 1: Write a function shoelace that consumes a Polygon and produces its area.
;; Place your solution in shoelace.rkt.

;; A Point is a (x,y) point in the Cartesian coordinate system
;; A Point is a (list Num Num)
;;
;; A Polygon is a (listof Point)
;; Requires: points form a simple polygon
;; (length Polygon) >= 3

;; Purpose: consumes a Polygon and produces its area
;; Contract: (listof Point) -> Num

(define (shoelace polygon)
  (/ (abs (sum-shoelace polygon (first polygon))) 2))

(define (sum-shoelace pts first-pt)
  (cond
    [(empty? (rest pts))
     (- (* (get-x (first pts)) (get-y first-pt))
        (* (get-x first-pt) (get-y (first pts))))]
    [else
     (+ (- (* (get-x (first pts)) (get-y (second pts)))
           (* (get-x (second pts)) (get-y (first pts))))
        (sum-shoelace (rest pts) first-pt))]))

(define (get-x p) (first p)) ;; get x from the Point
(define (get-y p) (second p)) ;; get y from the Point

;; Test cases
(check-expect (shoelace (list (list 0 0) (list 0 3) (list 4 0))) 6)
(check-expect (shoelace (list (list 0 0) (list 0 5) (list 5 5) (list 5 0)))
              25)                                                            
(check-expect (shoelace (list (list 0 0) (list 0 7) (list 5 7) (list 5 0)))
              35)                                                          
(check-expect (shoelace (list (list -2 -2) (list -2 2) (list 2 -2))) 8)
                              
                              
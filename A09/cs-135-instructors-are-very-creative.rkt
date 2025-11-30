;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname cs-135-instructors-are-very-creative) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;
;; ***********************************************
;; Hyeonbin Jung (21128068)            
;; CS 135 Fall 2025
;; Assignment 09, Problem 1
;; ***********************************************

;;---------------------------------------------------------------------------------------------------

;; Problem 1 (a)

;; count-down : Nat -> (listof Nat)
;; Purpose: consumes a natural number
;; n and produces a list of natural numbers counting down from n to 0 
(define (count-down n)
  (build-list (add1 n)
              (lambda (i)
                (- n i))))

;; Test cases
(check-expect (count-down 4) (list 4 3 2 1 0))
(check-expect (count-down 5) (list 5 4 3 2 1 0))

;; Problem 1 (b)

;; sandwich : (listof Sym) -> (listof Sym)
;; Purpose: consume a non-empty list of symbols and produces a
;; list of symbols, where each symbol from the input list has the symbol 'bread on either
;; side of it.

(define (sandwich lst)
  (foldr (lambda (x acc)
           (cond [(empty? acc)
                  (list 'bread x 'bread)]
                 [else
                  (cons 'bread (cons x acc))]))
         empty
         lst))

;; Test cases
(check-expect (sandwich (list 'baguette))
              (list 'bread 'baguette 'bread))

(check-expect (sandwich (list 'dumpling 'bananabread))
              (list 'bread 'dumpling 'bread 'bananabread 'bread))


;; Problem 1 (c)

;; dot: (listof Num) (listof Num) -> Num
;; Purpose: consumes two lists of numbers of equal length and produces
;; the dot product of the two lists, defined as the sum of the
;; products of corresponding elements.
(define (dot lst1 lst2)
  (foldr + 
         0
         (map (lambda (x y) (* x y))
              lst1
              lst2)))

;; Test cases
(check-expect (dot (list 1 2 3) (list 1 1 1)) 6)
(check-expect (dot (list 1 0 -1 0) (list 2 -7 2 25)) 0)

;; Problem 1 (d)

;; eval-poly : (listof Num) Num -> Num
;; Purpose: consumes a list of coefficients for a polynomial and a number x,
;; and produces the value of the polynomial at x.


(define (eval-poly coeffs x)
  (foldr (lambda (a acc)
           (+ a (* x acc)))
         0
         coeffs))

;; Test cases
(check-expect (eval-poly (list 2 4 1) 3) 23)
(check-within (eval-poly (list 1 1) pi) #i4.141592654 .00001)

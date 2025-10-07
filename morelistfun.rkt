;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname morelistfun) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;; ***********************************************
;; Hyeonbin Jung (21128068)            
;; CS 135 Fall 2025
;; Assignment 04, Problem 3
;; ***********************************************

;;---------------------------------------------------------------------------------------------------
;; Question 3(a): Write a function negate

;; function negate
;; Purpose: consumes a list of numbers and produces a new list where
;; each number has been negated.

;; Contract: (listof Num) -> (list of Num)

(define (negate lst)
  (cond
    [(empty? lst) empty]                             
    [else (cons (* -1 (first lst)) (negate (rest lst)))]
    )
  )  

;; Test cases
(check-expect (negate (cons 100 (cons -10 (cons 0 empty))))
(cons -100 (cons 10 (cons 0 empty)))
)

(check-expect (negate (cons 0 (cons 0 (cons 0 empty))))
(cons -0 (cons 0 (cons 0 empty)))
)

(check-expect (negate (cons 10000 (cons -7777 (cons 251007 empty))))
(cons -10000 (cons 7777 (cons -251007 empty)))
)

;;---------------------------------------------------------------------------------------------------
;; Question 3(b): Write a function count-down

;; function count-down
;; Purpose: consumes a natural number and produces a list of
;; natural numbers counting down from that number to 0 (inclusive).

;; Contract: Nat -> (listof Nat)

(define (count-down n)
  (cond
    [(zero? n) (cons 0 empty)]            
    [else (cons n (count-down (- n 1)))]))

;; Test cases
(check-expect (count-down 9) (cons 9 (cons 8 (cons 7 (cons 6 (cons 5
(cons 4 (cons 3 (cons 2 (cons 1 (cons 0 empty)))))))))))

(check-expect (count-down 5) (cons 5
(cons 4 (cons 3 (cons 2 (cons 1 (cons 0 empty)))))))


;;---------------------------------------------------------------------------------------------------
;; Question 3(c): Write a function add-constant

;; function add-constant
;; Purpose: consumes a number and a list of numbers, and
;; produces a new list where the given number has been added to each element of the list.

;; Contract: Num (listof Num) -> (listof Num)

(define (add-constant n lst)
  (cond
    [(empty? lst) empty]                     
    [else (cons (+ (first lst) n)             
                (add-constant n (rest lst)))]))

;; Test cases
(check-expect (add-constant 1/2 (cons 10 (cons 4 empty)))
(cons 21/2 (cons 9/2 empty)))

(check-expect (add-constant 10 (cons 10 (cons 20 empty)))
(cons 20 (cons 30 empty)))

(check-expect (add-constant 0 (cons 5 (cons 10 (cons 4 empty))))
(cons 5 (cons 10 (cons 4 empty))))

;;---------------------------------------------------------------------------------------------------
;; Question 3(d): Write a function count-up

;; function count-up
;; Purpose: consumes a natural number and produces a list of natural
;; numbers counting up from 0 to that number (inclusive).

;; Contract: Nat -> (listof Nat)

(define (count-up n)
  (add-constant n
                (negate
                 (count-down n))))

;; Test cases
(check-expect (count-up 4)
(cons 0 (cons 1 (cons 2 (cons 3 (cons 4 empty))))))

(check-expect (count-up 9)
(cons 0 (cons 1 (cons 2 (cons 3 (cons 4 (cons 5 (cons 6 (cons 7 (cons 8 (cons 9 empty)))))))))))

(check-expect (count-up 1)
(cons 0 (cons 1 empty))
)
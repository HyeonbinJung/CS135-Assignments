;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname listfun) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;; ***********************************************
;; Hyeonbin Jung (21128068)            
;; CS 135 Fall 2025
;; Assignment 04, Problem 2
;; ***********************************************

;;---------------------------------------------------------------------------------------------------
;; Question 2(a): Write a function double-plus-one

;; Function double-plus-one
;; Purpose: consumes a list of numbers
;; produces a transformed list where each number is doubled, then 1 is added.

;; Contract: (listof Num) -> (listof Num)

(define (double-plus-one lst)
  (cond
    [(empty? lst) empty]                               
    [else (cons (+ (* 2 (first lst)) 1)                 
                (double-plus-one (rest lst)))]))

;; Test cases
(check-expect
 (double-plus-one(cons 5 (cons -3 (cons 0 (cons 12 empty)))))
(cons 11 (cons -5 (cons 1 (cons 25 empty))))
)
(check-expect
 (double-plus-one(cons 2 (cons 2 (cons 3 (cons 3 empty)))))
(cons 5 (cons 5 (cons 7 (cons 7 empty))))
)
(check-expect
 (double-plus-one(cons -50 (cons -100 (cons -150 (cons -999 empty)))))
(cons -99 (cons -199 (cons -299 (cons -1997 empty))))
)

;;---------------------------------------------------------------------------------------------------
;; Question 2(b): Write a function symbol-sandwich

;; Function symbol-sandwich
;; Purpose: consumes a list of symbols
;; produces a new list where each symbol is "sandwiched" between the symbol 'bread
;; if the consumed list is empty, symbol-sandwich produces empty

;; Contract: (listof Sym) -> (listof Sym)

(define (symbol-sandwich lst)
  (cond
    [(empty? lst) empty]  
    [else
     (cons 'bread
           (cons (first lst)
                 (cond
                   [(empty? (rest lst)) (cons 'bread empty)]  
                   [else (symbol-sandwich (rest lst))])))])) 

;; Test cases
(check-expect (symbol-sandwich empty) empty)

(check-expect (symbol-sandwich (cons 'ham (cons 'cheese (cons
'lettuce empty))))
(cons 'bread (cons 'ham (cons 'bread (cons 'cheese (cons 'bread
(cons 'lettuce (cons 'bread empty))))))))

(check-expect (symbol-sandwich (cons 'bread empty))
(cons 'bread (cons 'bread (cons 'bread empty))))

;;---------------------------------------------------------------------------------------------------
;; Question 2(c): Write a function shuffle-rock-paper-lizard-spock

;; Function shuffle-rock-paper-lizard-spock
;; Purpose: consumes a list of symbols
;;  produces a new list where:
;; • every 'rock is changed to 'paper
;; • every 'paper is changed to 'lizard
;; • every 'lizard is changed to 'rock
;; • every 'spock is removed

;; Contract: (listof Sym) -> (listof Sym)

(define (shuffle-rock-paper-lizard-spock lst)
  (cond
    [(empty? lst) empty] 
    [(symbol=? (first lst) 'rock)
     (cons 'paper (shuffle-rock-paper-lizard-spock (rest lst)))]
    [(symbol=? (first lst) 'paper)
     (cons 'lizard (shuffle-rock-paper-lizard-spock (rest lst)))]
    [(symbol=? (first lst) 'lizard)
     (cons 'rock (shuffle-rock-paper-lizard-spock (rest lst)))]
    [(symbol=? (first lst) 'spock)
     (shuffle-rock-paper-lizard-spock (rest lst))]
    [else
     (cons (first lst) (shuffle-rock-paper-lizard-spock (rest lst)))]))

(check-expect
(shuffle-rock-paper-lizard-spock (cons 'rock (cons 'paper (cons
'lizard (cons 'spock (cons 'therock empty))))))
(cons 'paper (cons 'lizard (cons 'rock (cons 'therock empty)))))

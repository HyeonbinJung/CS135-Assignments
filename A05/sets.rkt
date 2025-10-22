;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname sets) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;
;; ***********************************************
;; Hyeonbin Jung (21128068)            
;; CS 135 Fall 2025
;; Assignment 05, Problem 1
;; ***********************************************

;;---------------------------------------------------------------------------------------------------

;; Question 1(a): Write a predicate that determines if an element is contained in a set.
;; Function: in?
;; Purpose:  consume a symbol and a set
;; produce a boolean indicating whether the symbol is in the set.
;; Contract: Sym Set -> Bool

(define (in? sym set)
  (cond
    [(empty? set) #false]
    [(symbol=? sym (first set)) #true]
    [else (in? sym (rest set))]))

;; Test cases
(check-expect (in? 'a (list 'a 'b 'c)) #true)
(check-expect (in? 'd (list 'a 'b 'c)) #false)
(check-expect (in? 'a (list)) #false)

;;---------------------------------------------------------------------------------------------------

;; Question 1(b): Write a function that attempts to add an element to a set. If the element is already
;; in the set, the set remains unchanged; otherwise the new element is added.
;; Function: add
;; Purpose: consume a symbol and a set, produce a new set that contains the symbol.
;; Contract: Sym Set -> Set

(define (add sym set)
  (cond
    [(in? sym set) set]
    [else (cons sym set)]))

;; Test cases
(check-expect (add 'a (list 'a 'b 'c)) (list 'a 'b 'c))
(check-expect (add 'd (list 'a 'b 'c)) (list 'd 'a 'b 'c))
(check-expect (add 'x (list)) (list 'x))

;;---------------------------------------------------------------------------------------------------

;; Question 1(c): Write a function that produces the union of two sets.
;; Function: union
;; Purpose: consume two sets, produces the union of two sets
;; Contract: Set Set -> Set
(define (union set1 set2)
  (cond
    [(empty? set1) set2]
    [(in? (first set1) set2)
     (union (rest set1) set2)]
    [else
     (cons (first set1)
           (union (rest set1) set2))]))

;; Test cases
(check-expect (union (list 'a 'b 'c) (list 'c 'd 'e)) (list 'a 'b 'c 'd 'e))
(check-expect (union (list) (list 'x 'y)) (list 'x 'y))
(check-expect (union (list 'x 'y) (list)) (list 'x 'y))
(check-expect (union (list 'a 'b) (list 'a 'b)) (list 'a 'b))

;;---------------------------------------------------------------------------------------------------

;; Question 1(d): Write a function that produces the intersection of two sets.
;; Function: intersection
;; Purpose: consume two sets, produces the intersection of two sets
;; Contract: Set Set -> Set
(define (intersection set1 set2)
  (cond
    [(empty? set1) empty]
    [(in? (first set1) set2)
     (cons (first set1)
           (intersection (rest set1) set2))]
    [else
     (intersection (rest set1) set2)]))

;; Test cases
(check-expect (intersection (list 'a 'b 'c) (list 'b 'c 'd)) (list 'b 'c))
(check-expect (intersection (list 'a 'b) (list 'c 'd)) (list))
(check-expect (intersection (list) (list 'x 'y)) (list))
(check-expect (intersection (list 'a 'b 'c) (list 'a 'b 'c)) (list 'a 'b 'c))

;;---------------------------------------------------------------------------------------------------

;; Question 1(e): Write a function that produces the intersection of two sets.
;; Function: set=?
;; Purpose:  consume two sets
;; produce #true if both sets contain exactly the same elements. Otherwise, false.
;; Contract: Set Set -> Bool

(define (subset? s1 s2)
  (cond
    [(empty? s1) #true]
    [(in? (first s1) s2)
     (subset? (rest s1) s2)]
    [else #false]))

;; Test cases
(check-expect (subset? empty empty) #true)              
(check-expect (subset? empty (list 'a 'b 'c)) #true)       
(check-expect (subset? (list 'a 'b) (list 'a 'b 'c)) #true) 

(define (set=? set1 set2)
  (and (subset? set1 set2)
       (subset? set2 set1)))

;; Test cases
(check-expect (set=? (list 'a 'b 'c) (list 'c 'b 'a)) #true)
(check-expect (set=? (list 'a 'b) (list 'a 'b 'c)) #false)
(check-expect (set=? (list) (list)) #true)
(check-expect (set=? (list 'x 'y 'z) (list 'x 'y 'z)) #true)
(check-expect (set=? (list 'a 'b 'c) (list 'a 'b 'd)) #false)

;;---------------------------------------------------------------------------------------------------

;; Question 1(f): Write a predicate that consumes anything and determines if it is a set.
;; Function: set?
;; Purpose:  consume anything and produce #true if anything is a set 
;; Contract: Any -> Bool

(define (set? x)
  (cond
    [(empty? x) #true]
    [(not (list? x)) #false]
    [(and (symbol? (first x))
            (set? (rest x))
            (not (in? (first x) (rest x))))
      #true]
    [else #false]))

;; Test cases
(check-expect (set? (list 'a 'b 'c)) #true)
(check-expect (set? (list 'a 'a 'a)) #false)
(check-expect (set? (list)) #true)
(check-expect (set? (list 1 2 3)) #false)
(check-expect (set? 'a) #false)
(check-expect (set? (list 'x (list 'y) 'z)) #false)
;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname natlist) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;
;; ***********************************************
;; Hyeonbin Jung (21128068)            
;; CS 135 Fall 2025
;; Assignment 05, Problem 3
;; ***********************************************

;;---------------------------------------------------------------------------------------------------

;; Helper function for 3(b), 3(d)
;; Reference: CS135 Course Note 10

(define (first-n n lst)
  (cond
    [(or (empty? lst) (zero? n)) empty]
    [else (cons (first lst)
                (first-n (sub1 n) (rest lst)))]))

;; Test cases for first-n
(check-expect (first-n 3 (list 1 2 3 4 5)) (list 1 2 3))
(check-expect (first-n 0 (list 1 2 3)) empty)
(check-expect (first-n 10 (list 'a 'b 'c)) (list 'a 'b 'c))

(define (rest-n n lst)
  (cond
    [(or (empty? lst) (zero? n)) lst]
    [else (rest-n (sub1 n) (rest lst))]))

;; Test cases for rest-n
(check-expect (rest-n 3 (list 1 2 3 4 5)) (list 4 5))
(check-expect (rest-n 0 (list 1 2 3)) (list 1 2 3))
(check-expect (rest-n 10 (list 'a 'b 'c)) empty)

;;---------------------------------------------------------------------------------------------------


;; Question 3(a):  Write a function list-sublist that consumes a list of anything and two natural
;; numbers m, n and produces the list of elements with indices between (inclusive) m, n.
;; Function: list-sublist
;; Purpose:  consume a list of anything and two natural numbers m, n
;; produce the list of elements with indices between (inclusive) m, n.
;; Contract: (listof Any) Nat Nat -> (listof Any)

(define (list-sublist lst m n)
  (cond
    [(empty? lst) empty] 
    [(zero? m)
     (cond
       [(zero? n) (list (first lst))]
       [else (cons (first lst)
                   (list-sublist (rest lst) 0 (- n 1)))])]
    [else (list-sublist (rest lst) (- m 1) (- n 1))]))

;; Test cases
(check-expect (list-sublist (list 0 1 2 3 4 5) 2 4) (list 2 3 4))
(check-expect (list-sublist (list 0 1 2 3 4 5) 0 2) (list 0 1 2))
(check-expect (list-sublist (list 0 1 2 3 4 5) 3 5) (list 3 4 5))
(check-expect (list-sublist (list 'a 'b 'c 'd) 2 2) (list 'c))
(check-expect (list-sublist (list 1 2 3) 0 2) (list 1 2 3))
(check-expect (list-sublist (list) 0 0) (list))

;;---------------------------------------------------------------------------------------------------

;; Question 3(b):  Write a function list-zipper
;; Function: list-zipper
;; Purpose: consumes two lists lst1 and lst2, and two positive integers m and n
;; produces a single list by alternately taking m items from lst1
;; and n items from lst2 until both are empty
;; Contract: (listof Any) (listof Any) Nat Nat -> (listof Any)

(define (list-zipper lst1 lst2 m n)
  (cond
    [(and (empty? lst1) (empty? lst2)) empty]
    [(empty? lst1) (first-n n lst2)]
    [(empty? lst2) (first-n m lst1)]
    [else
     (append
      (first-n m lst1)
      (first-n n lst2)
      (list-zipper (rest-n m lst1)
                   (rest-n n lst2)
                   m n))]))

;; Test cases
(check-expect
 (list-zipper (list 0 2 4 6 8 10)
              (list 1 3 5 7 9)
              2 3)
 (list 0 2 1 3 5 4 6 7 9 8 10))

(check-expect
 (list-zipper (list 'a 'b 'c 'd)
              (list 'x 'y 'z 'w)
              2 2)
 (list 'a 'b 'x 'y 'c 'd 'z 'w))

(check-expect
 (list-zipper (list 10 20 30 40 50)
              (list 1 2)
              2 1)
 (list 10 20 1 30 40 2 50))

(check-expect
 (list-zipper empty (list 1 2 3 4) 2 3)
 (list 1 2 3))

;;---------------------------------------------------------------------------------------------------

;; Question 3(c):  Write a function list-insert-at
;; Function: list-insert-at
;; Purpose:  consume two lists of anything and a natural number n
;; produce a list where all the items of the second list have been
;; inserted (in order) into the first list at index n. If n is not a valid index, insert the second
;; list at the end of the first list.
;; Contract: (listof Any) (listof Any) Nat -> (listof Any)

(define (list-insert-at lst1 lst2 n)
  (cond
    [(empty? lst1) lst2]
    [(zero? n)
     (cond
       [(empty? lst2) lst1]                     
       [else (cons (first lst2)
                   (list-insert-at lst1 (rest lst2) 0))])]
    [else
     (cons (first lst1)
           (list-insert-at (rest lst1) lst2 (- n 1)))]))

;; Test cases
(check-expect (list-insert-at (list 5 6 7 8 9) (list 'x 'y 'z) 3) (list 5 6 7 'x 'y 'z 8 9))
(check-expect (list-insert-at (list 2 4 6 8) (list 'p 'q) 0) (list 'p 'q 2 4 6 8))
(check-expect (list-insert-at (list 100 200 300) (list 'r 's) 5) (list 100 200 300 'r 's))
(check-expect (list-insert-at (list) (list 1 2 3) 2) (list 1 2 3))
(check-expect (list-insert-at (list 9 8 7 6) (list) 2) (list 9 8 7 6))


;;---------------------------------------------------------------------------------------------------

;; Question 3(d):  Write a function list-swap
;; Function: list-swap
;; Purpose: consumes a list of any and two natural numbers
;; m, n and produces a list the same as the consumed list except that the items at indices
;; m and n have been swapped.
;; Contract: (listof Any) Nat Nat -> (listof Any)

(define (list-swap lst m n)
  (cond
    [(= m n) lst]
    [(< m n)
     (append
      (first-n m lst)                          
      (list (first (rest-n n lst)))            
      (first-n (- n m 1) (rest-n (+ m 1) lst)) 
      (list (first (rest-n m lst)))            
      (rest-n (+ n 1) lst))]
    [else
     (list-swap lst n m)]))                  

;; Test cases
(check-expect (list-swap (list 0 1 2 3 4 5 6) 2 4)
              (list 0 1 4 3 2 5 6))

(check-expect (list-swap (list 0 1 2 3 4 5 6) 3 3)
              (list 0 1 2 3 4 5 6))

(check-expect (list-swap (list 'a 'b 'c 'd 'e) 0 4)
              (list 'e 'b 'c 'd 'a))

(check-expect (list-swap (list 'x 'y 'z) 1 2)
              (list 'x 'z 'y))


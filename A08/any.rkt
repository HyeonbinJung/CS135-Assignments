;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname any) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;
;; ***********************************************
;; Hyeonbin Jung (21128068)            
;; CS 135 Fall 2025
;; Assignment 08, Problem 1
;; ***********************************************

;;---------------------------------------------------------------------------------------------------

;; Problem 1 (a)
;; loa-depth: (listof Any) -> Nat
;; Purpose: consume list of Any and produce its depth

(define (loa-depth lst)
  (cond
    [(not (list? lst)) 0]
    [(empty? lst) 0]
    [else
     (max (+ 1 (loa-depth (first lst)))
          (loa-depth (rest lst)))]))


;; Test cases
(check-expect (loa-depth '(a (b c (42 d e ("cs135" g) h) #\space (i
42 j) k 42))) 4)
(check-expect
(loa-depth '(apple (apple (apple apple) apple) apple)) 3)
(check-expect (loa-depth '()) 0)
(check-expect (loa-depth '(())) 1)


;; Problem 1 (b)
;; loa-transform: Sym Sym (listof Any) -> (listof Any)
;; Consume Sym1 Sym2 and listof Any, produce listof Any with all sym1 changed to sym2

(define (loa-transform old new lst)
  (cond
    
    [(symbol? lst)
     (cond [(symbol=? lst old) new]
           [else lst])]

    [(not (list? lst))
     lst]

    [(empty? lst)
     empty]

    [else
     (cons (loa-transform old new (first lst))
           (loa-transform old new (rest lst)))]))

;; Test cases
(check-expect
(loa-transform 'x 'z '(a (b c (x d e (f g) h) x (i x j) k x)))
'(a (b c (z d e (f g) h) z (i z j) k z)))

(check-expect
(loa-transform 'apple 'orange
'(apple (apple (apple apple) apple) apple))
'(orange (orange (orange orange) orange) orange))


;; Problem 1 (c)
;; loa-filter: Sym (listof Any) -> (listof Any)
;; Purpose: consume Sym and listof Any, produce new listof Any with all the Sym removed.

(define (loa-filter target lst)
  (local
    [(define (is-target? x)
       (and (symbol? x)
            (symbol=? x target)))]
    (cond
      [(not (list? lst))
       lst]
      [(empty? lst)
       empty]
      
      [(is-target? (first lst))
       (loa-filter target (rest lst))]
      
      [(list? (first lst))
       (cons (loa-filter target (first lst))
             (loa-filter target (rest lst)))]
      
      [else
       (cons (first lst)
             (loa-filter target (rest lst)))])))

;; Test cases
(check-expect
(loa-filter 'x '(a (b c (x d e (f g) h) x (i x j) k x)))
'(a (b c (d e (f g) h) (i j) k)))

(check-expect (loa-filter 'a '(a 100 (a "hello" (a))))
'(100 ("hello" ())))

(check-expect
(loa-filter 'apple '(apple (apple (apple apple) apple) apple))
'((())))


;; Problem 1 (d)
;; empty-out: (listof Any) -> (listof Any)
;; Purpose: consume listof Any and produce listof Any with all empty list removed.

(define (empty-out lst)
  (cond
    [(not (list? lst))
     lst]
    
    [(empty? lst)
     empty]

    [(list? (first lst))
     (local
       [(define cleaned-first (empty-out (first lst)))]
       (cond
         [(empty? cleaned-first)
          (empty-out (rest lst))]
         [else
          (cons cleaned-first
                (empty-out (rest lst)))]))]

    [else
     (cons (first lst)
           (empty-out (rest lst)))]))

;; Test cases
(check-expect (empty-out '((() () (()) ()))) '())
(check-expect (empty-out (list 1 empty (list empty 2) empty 3))
(list 1 (list 2) 3))

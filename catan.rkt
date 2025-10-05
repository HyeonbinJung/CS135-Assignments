;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname catan) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;; ***********************************************
;; Hyeonbin Jung (21128068)            
;; CS 135 Fall 2025
;; Assignment 03, Problem 4
;; ***********************************************

;;---------------------------------------------------------------------------------------------------

;; Inventory data definition (given)
;; An Inventory describes how much we have of lumber, brick, grain, wool, and ore, in that order.
;; An Inventory is a (cons Nat (cons Nat (cons Nat (cons Nat (cons Nat empty)))))

;; Question 4(a): Write the helper function mk-inventory that consumes 5 Nat values representing
;; lumber, brick, grain, wool, and ore, and produces an Inventory.

;; Function mk-inventory
;; Purpose: Consume 5 Nat values representing lumber, brick, grain, wool, and ore (in that order)
;; produce an Inventory in the same order.
;; Contract: mk-inventory: Nat Nat Nat Nat Nat -> Inventory
;; An Inventory is a (cons Nat (cons Nat (cons Nat (cons Nat (cons Nat empty)))))

(define (mk-inventory lumber brick grain wool ore)
  (cons lumber (cons brick (cons grain (cons wool (cons ore empty))))))

(check-expect (mk-inventory 0 0 0 0 0)
              (cons 0 (cons 0 (cons 0 (cons 0 (cons 0 empty))))))
(check-expect (mk-inventory 1 2 3 4 5)
              (cons 1 (cons 2 (cons 3 (cons 4 (cons 5 empty))))))

;;---------------------------------------------------------------------------------------------------

;; Question 4(b): Write the function count-actions. It consumes an Inventory and an Action, and
;; determines how many times the player can complete the action, given their inventory.

;; Function count-actions
;; Purpose: Consume an Inventory and an Action and produce how many times we can perform the action
;;   by spending the resources from the inventory.
;; Contract: count-actions: Inventory (anyof 'road 'settlement 'city 'card) -> Nat
;; An Inventory is a (cons Nat (cons Nat (cons Nat (cons Nat (cons Nat empty)))))

(define (count-actions inv action)
  (cond
    [(symbol=? action 'road) (construct-road (inv-lumber inv) (inv-brick inv) 0)]
    [(symbol=? action 'settlement)
     (construct-settlement (inv-lumber inv) (inv-brick inv) (inv-grain inv) (inv-wool inv) 0)]
    [(symbol=? action 'city) (upgrade-city (inv-grain inv) (inv-ore inv) 0)]
    [(symbol=? action 'card) (buy-card (inv-grain inv) (inv-wool inv) (inv-ore inv) 0)]
    )
  )

(define (inv-lumber inv) (first inv))
(define (inv-brick  inv) (first (rest inv)))
(define (inv-grain  inv) (first (rest (rest inv))))
(define (inv-wool   inv) (first (rest (rest (rest inv)))))
(define (inv-ore    inv) (first (rest (rest (rest (rest inv))))))

(define (construct-road lumber brick count0) ; How many road
  (cond
  [(zero? lumber) count0]
  [(zero? brick) count0]
  [(and (< 0 lumber) (< 0 brick)) (construct-road (- lumber 1) (- brick 1) (+ count0 1))])
  )

(define (construct-settlement lumber brick grain wool count0) ; How many settlement
  (cond
  [(zero? lumber) count0]
  [(zero? brick) count0]
  [(zero? grain) count0]
  [(zero? wool) count0]
  
  [(and (< 0 lumber) (< 0 brick) (< 0 grain) (< 0 wool))
   (construct-settlement (- lumber 1) (- brick 1) (- grain 1) (- wool 1) (+ count0 1))])
  )

(define (upgrade-city grain ore count0) ; How many city upgrade?
  (cond
  [(< grain 2) count0]
  [(< ore 3) count0]
  
  [(and (< 1 grain) (< 2 ore))
   (upgrade-city (- grain 2) (- ore 3) (+ count0 1))])
  )

(define (buy-card grain wool ore count0) ; How many card
  (cond
  [(zero? grain) count0]
  [(zero? wool) count0]
  [(zero? ore) count0]
  
  [(and (< 0 grain) (< 0 wool) (< 0 ore))
   (buy-card (- grain 1) (- wool 1) (- ore 1) (+ count0 1))])
  )

;; Test cases
(define demo-inv (mk-inventory 3 5 9 7 30))
(check-expect (count-actions demo-inv 'road)       3)
(check-expect (count-actions demo-inv 'settlement) 3)
(check-expect (count-actions demo-inv 'city)       4)
(check-expect (count-actions demo-inv 'card)       7)

(check-expect (count-actions (mk-inventory 0 0 0 0 0) 'road) 0)
(check-expect (count-actions (mk-inventory 1 0 1 1 1) 'settlement) 0)
(check-expect (count-actions (mk-inventory 8 8 3 2 4) 'city) 1)
(check-expect (count-actions (mk-inventory 7 7 7 7 7) 'card) 7)

;;---------------------------------------------------------------------------------------------------

;; Question 4(c): Write the function count-actions. It consumes an Inventory and an Action, and
;; determines how many times the player can complete the action, given their inventory.

;; Function: convert-gold-road
;; Purpose: Consume an Inventory and a Nat(bags of gold), Produce new Inventory with all bags of gold
;; converted to lumber and bricks to construct road as many as possible.
;; Contract: convert-gold-road: Inventory Nat -> Inventory
;; An Inventory is a (cons Nat (cons Nat (cons Nat (cons Nat (cons Nat empty)))))
;; (number of bags of gold is a Nat)

(define (convert-gold-road inv gold)
  (cond
    [(and (< 0 gold) (< (inv-lumber inv) (inv-brick inv)))
     (convert-gold-road (mk-inventory (+ 1 (inv-lumber inv)) (inv-brick inv) (inv-grain inv)
                                  (inv-wool inv) (inv-ore inv)) (- gold 1))]
    [(and (< 0 gold) (> (inv-lumber inv) (inv-brick inv)))
     (convert-gold-road (mk-inventory (inv-lumber inv) (+ 1 (inv-brick inv)) (inv-grain inv)
                                  (inv-wool inv) (inv-ore inv)) (- gold 1))]
    [(and (< 1 gold) (= (inv-lumber inv) (inv-brick inv)))
     (convert-gold-road (mk-inventory (+ 1 (inv-lumber inv)) (+ 1 (inv-brick inv)) (inv-grain inv)
                                  (inv-wool inv) (inv-ore inv)) (- gold 2))]
    [(and (= 1 gold) (= (inv-lumber inv) (inv-brick inv)))
     (convert-gold-road (mk-inventory (inv-lumber inv) (+ 1 (inv-brick inv)) (inv-grain inv)
                                  (inv-wool inv) (inv-ore inv)) (- gold 1))]
    [(= 0 gold) (mk-inventory (inv-lumber inv) (inv-brick inv)
                              (inv-grain inv) (inv-wool inv) (inv-ore inv))]
    )
  )

;; Test cases
(check-expect (convert-gold-road (mk-inventory 0 0 0 0 0) 10) (mk-inventory 5 5 0 0 0))

(check-expect (convert-gold-road (mk-inventory 0 11 0 0 0) 10) (mk-inventory 10 11 0 0 0))

(check-expect (convert-gold-road (mk-inventory 3 5 0 0 0) 11) (mk-inventory 9 10 0 0 0)) 

(check-expect (convert-gold-road (mk-inventory 7 7 0 0 0) 1) (mk-inventory 7 8 0 0 0))
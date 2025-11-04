;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname grimoire) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;
;; ***********************************************
;; Hyeonbin Jung (21128068)            
;; CS 135 Fall 2025
;; Assignment 06, Problem 2
;; ***********************************************

;;---------------------------------------------------------------------------------------------------

;; Helper Functions
(define (get-name sp) (first sp))
(define (get-power sp) (second sp))

;; A Spell is a (list Sym Nat), e.g. (list 'fireblast 13) is a Spell.
;;
;; A Grimoire is a (listof Spell)
;; Requires: sorted by power level in ascending order
;; all spells are unique by spell name
;; For example (list (list 'super-big-heal 4) (list 'tiny-fire-ball 11)
;; (list 'just-right-frostbolt 8000)) is a Grimoire.

;;---------------------------------------------------------------------------------------------------

;; Question 2(a): Write a function memorizable-spells that takes a Grimoire and a natural number lvl

;; Purpose:
;; Contract: Grimoire Nat -> Grimoire

(define (memorizable-spells g lvl)
  (cond
    [(empty? g) empty]
    [(<= (get-power (first g)) lvl)
     (cons (first g) (memorizable-spells (rest g) lvl))]
    [else
     (memorizable-spells (rest g) lvl)]))

;; Test cases
(check-expect (memorizable-spells (list (list 'icelance 2)
(list 'iceblock 4)
(list 'icyveins 7)) 4)
(list (list 'icelance 2) (list 'iceblock 4)))
(check-expect (memorizable-spells empty 5) empty)

;;---------------------------------------------------------------------------------------------------

;; Question 2(b): Write a function insert-spell that takes a Spell and a Grimoire and produces
;; a new Grimoire with the spell inserted in the correct position according to the following
;; ordering rule:
;; • When Spells have the same power level, the new Spell should be placed at the
;; front of all other Spells with the same power level.

;; Purpose: consume spell and Grimoire, produce new Grimoire which consumed spell is inserted
;; Contract: Spell Grimoire -> Grimoire

(define (insert-spell sp g)
  (cond
    [(empty? g)
     (list sp)]
    [(< (get-power sp) (get-power (first g)))
     (cons sp g)]
    [(= (get-power sp) (get-power (first g)))
     (cons sp g)]
    [else
     (cons (first g) (insert-spell sp (rest g)))]))

;; Test cases
(check-expect (insert-spell (list 'pyroblast 6)
(list (list 'fireball 3)
(list 'scorch 4)
(list 'fireblast 6)
(list 'combustion 10)))
(list (list 'fireball 3)
(list 'scorch 4)
(list 'pyroblast 6)
(list 'fireblast 6)
(list 'combustion 10)))

(check-expect (insert-spell (list 'chaosbolt 5) empty)
(list (list 'chaosbolt 5)))

;;---------------------------------------------------------------------------------------------------

;; Question 2(c): Write a function remove-power that takes a power level (natural number) and a
;; Grimoire, and produces a new Grimoire with all Spells of that power level removed.

;; Purpose: consume a power level (Nat) and a Grimoire,
;; produce a new Grimoire with all Spells of that power level removed.
;; Contract : Nat Grimoire -> Grimoire

(define (remove-power lvl g)
  (cond
    [(empty? g) empty]
    [(= (get-power (first g)) lvl)
     (remove-power lvl (rest g))]
    [else
     (cons (first g) (remove-power lvl (rest g)))]))

;; Test cases
(check-expect (remove-power 4 (list (list 'heal 2)
(list 'shield 4)
(list 'flashheal 4)
(list 'spiritmend 7)))
(list (list 'heal 2) (list 'spiritmend 7)))
(check-expect (remove-power 5 empty) empty)






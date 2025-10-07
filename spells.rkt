;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname spells) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;; ***********************************************
;; Hyeonbin Jung (21128068)            
;; CS 135 Fall 2025
;; Assignment 04, Problem 4
;; ***********************************************



;;---------------------------------------------------------------------------------------------------

;; Question 4(a): Create data definitions for a:
;; • Spell - A spell is represented by one of the spell symbols from the table above.
;; • Spellbook - a list of spells the wizard will memorize.

;; data definition of Spell
;; A Spell is a spell the wizard can memorize
;; A Spell is (anyof 'light 'mage-hand 'magic-missile 'shield
;;                  'fireball 'invisibility 'teleport 'meteor-swarm)

;; data definition of Spellbook
;; A Spellbook is a list of Spells the wizard will memorize
;; a (listof Spell) is one of:
;; * empty
;; * (cons Spell (listof Spell))

;;---------------------------------------------------------------------------------------------------

;; Question 4(b): Write the predicate function valid-spellbook?

;; function vaild-spellbook?
;; Purpose:
;; consumes two arguments:
;; • available-slots: the number of spell slots the wizard has.
;; • spellbook: a proposed spellbook with the spells the wizard will memorize
;; produce true if the wizard has enough spell slots to memorize all
;; the spells in their spellbook, false otherwise.

;; Contract: Num Spellbook -> Bool

(define (spell-cost s)
  (cond
    [(symbol=? s 'light)          0]
    [(symbol=? s 'mage-hand)      0]
    [(symbol=? s 'magic-missile)  1]
    [(symbol=? s 'shield)         1]
    [(symbol=? s 'fireball)       2]
    [(symbol=? s 'invisibility)   2]
    [(symbol=? s 'teleport)       3]
    [(symbol=? s 'meteor-swarm)   3]
    [else                         0]
    )
  ) 

(define (valid-spellbook? available-slots spellbook)
  (cond
    [(empty? spellbook) (>= available-slots 0)]
    [else
     (valid-spellbook?
      (- available-slots (spell-cost (first spellbook)))
      (rest spellbook))]))


;; Test cases
(check-expect (valid-spellbook? 3 (cons 'magic-missile (cons 'shield (cons 'light empty))))
true)

(check-expect (valid-spellbook? 2 (cons 'fireball (cons 'invisibility empty)))
false)

(check-expect (valid-spellbook? 1 empty)
true)

(check-expect (valid-spellbook? 5 (cons 'fireball (cons 'light (cons 'light empty))))
true)
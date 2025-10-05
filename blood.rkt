;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname blood) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;; ***************************************************
;; Hyeonbin Jung (21128068)
;; CS 135 Fall 2025
;; Assignment 02, Problem 4
;; ***************************************************
;;
;;
;; Question 4a)
;; Purpose: determine if donor blood type can donate to recipient blood type
;; Contract: can-donate-to/cond? : Sym Sym -> Bool
;; Requires: donor and recipient {any of 'O- 'O+ 'A- 'A+ 'B- 'B+ 'AB- 'AB+}
;; Consumes two blood type(one from donor, another from recipient)
;; Produce true when donor can donate, else, false
(define (can-donate-to/cond? donor recipient)
  (cond
    [(symbol=? donor 'O-) true]
    [(symbol=? donor 'O+)
     (cond
       [(symbol=? recipient 'O+) true]
       [(symbol=? recipient 'A+) true]
       [(symbol=? recipient 'B+) true]
       [(symbol=? recipient 'AB+) true]
       [else false])]
    [(symbol=? donor 'A-)
     (cond
       [(symbol=? recipient 'A-) true]
       [(symbol=? recipient 'A+) true]
       [(symbol=? recipient 'AB-) true]
       [(symbol=? recipient 'AB+) true]
       [else false])]
    [(symbol=? donor 'A+)
     (cond
       [(symbol=? recipient 'A+) true]
       [(symbol=? recipient 'AB+) true]
       [else false])]
    [(symbol=? donor 'B-)
     (cond
       [(symbol=? recipient 'B-) true]
       [(symbol=? recipient 'B+) true]
       [(symbol=? recipient 'AB-) true]
       [(symbol=? recipient 'AB+) true]
       [else false])]
    [(symbol=? donor 'B+)
     (cond
       [(symbol=? recipient 'B+) true]
       [(symbol=? recipient 'AB+) true]
       [else false])]
    [(symbol=? donor 'AB-)
     (cond
       [(symbol=? recipient 'AB-) true]
       [(symbol=? recipient 'AB+) true]
       [else false])]
    [(symbol=? donor 'AB+)
     (cond
       [(symbol=? recipient 'AB+) true]
       [else false])]))

;; Test cases
(check-expect (can-donate-to/cond? 'O- 'AB+) true)
(check-expect (can-donate-to/cond? 'O+ 'A-)  false)
(check-expect (can-donate-to/cond? 'A- 'AB+) true)
(check-expect (can-donate-to/cond? 'A+ 'AB-) false)
(check-expect (can-donate-to/cond? 'B- 'AB+) true)
(check-expect (can-donate-to/cond? 'B+ 'AB+) true)
(check-expect (can-donate-to/cond? 'AB- 'AB+) true)
(check-expect (can-donate-to/cond? 'AB+ 'AB-) false)

;; Question 4b)
;; Purpose: Same as can-donate-to/cond? but written with a Boolean expression (without using cond)
;; Contract: can-donate-to/cond? : Sym Sym -> Bool
;; Requires: donor and recipient {any of 'O- 'O+ 'A- 'A+ 'B- 'B+ 'AB- 'AB+}
;; Consumes two blood type(one from donor, another from recipient)
;; Produce true when donor can donate, else, false
(define (can-donate-to/bool? donor recipient)
  (or
   (symbol=? donor 'O-)
   (and (symbol=? donor 'O+)
        (or (symbol=? recipient 'O+)
            (symbol=? recipient 'A+)
            (symbol=? recipient 'B+)
            (symbol=? recipient 'AB+)))
   (and (symbol=? donor 'A-)
        (or (symbol=? recipient 'A-)
            (symbol=? recipient 'A+)
            (symbol=? recipient 'AB-)
            (symbol=? recipient 'AB+)))
   (and (symbol=? donor 'A+)
        (or (symbol=? recipient 'A+)
            (symbol=? recipient 'AB+)))
   (and (symbol=? donor 'B-)
        (or (symbol=? recipient 'B-)
            (symbol=? recipient 'B+)
            (symbol=? recipient 'AB-)
            (symbol=? recipient 'AB+)))
   (and (symbol=? donor 'B+)
        (or (symbol=? recipient 'B+)
            (symbol=? recipient 'AB+)))
   (and (symbol=? donor 'AB-)
        (or (symbol=? recipient 'AB-)
            (symbol=? recipient 'AB+)))
   (and (symbol=? donor 'AB+)
        (symbol=? recipient 'AB+))))

;; Test cases
(check-expect (can-donate-to/bool? 'O- 'AB+) true)
(check-expect (can-donate-to/bool? 'O+ 'A-)  false)
(check-expect (can-donate-to/bool? 'A- 'AB+) true)
(check-expect (can-donate-to/bool? 'A+ 'AB-) false)
(check-expect (can-donate-to/bool? 'B+ 'AB+) true)
(check-expect (can-donate-to/bool? 'AB- 'AB+) true)
(check-expect (can-donate-to/bool? 'AB+ 'AB-) false)

;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname robot) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;; ***********************************************
;; Hyeonbin Jung (21128068)            
;; CS 135 Fall 2025
;; Assignment 03, Problem 3
;; ***********************************************

;;---------------------------------------------------------------------------------------------------

;; Question 3(a): Write a data definition for the four directions defined above
;; (call this Direction). Use it to write a data definition for a robot’s State.

;; data definition of Direction
;; A Direction is a direction of the Robot is facing
;; A Direction is (anyof 'North 'South 'West 'East)

;; data definition of State
;; A State is given by robot's (x, y) position on an integer grid and the direction it's facing
;; A State is a (cons Int (cons Int (cons Direction empty)))

;;---------------------------------------------------------------------------------------------------

;; Question 3(b): Write helper functions for a robot’s State,
;; call them: mk-state, get-x, get-y, and get-direction.

;; Function mk-state
;; Purpose: consumes an Int, an Int, and a Direction
;; and produces a State
;; Contract: mk-state: Int Int Direction -> State
;; Direction and State are defined in 3(a)
;; A Direction is (anyof 'North 'South 'West 'East)
;; A State is a (cons Int (cons Int (cons Direction empty)))
(define (mk-state x y dir)
  (cons x (cons y (cons dir empty))))

;; Function get-x
;; Purpose: get-x consumes a State and produces its x-coordinate
;; Contract: get-x: State -> Int
;; Direction and State are defined in 3(a)
;; A Direction is (anyof 'North 'South 'West 'East)
;; A State is a (cons Int (cons Int (cons Direction empty)))
(define (get-x state)
  (first state))

;; Function get-y
;; Purpose: get-y consumes a State and produces its y-coordinate
;; Contract: get-y: State -> Int
;; Direction and State are defined in 3(a)
;; A Direction is (anyof 'North 'South 'West 'East)
;; A State is a (cons Int (cons Int (cons Direction empty)))
(define (get-y state)
  (first (rest state)))

;; Function get-direction
;; Purpose: get-direction consumes a State and produces its Direction
;; Contract: get-direction: State -> Direction
;; Direction and State are defined in 3(a)
;; A Direction is (anyof 'North 'South 'West 'East)
;; A State is a (cons Int (cons Int (cons Direction empty)))
(define (get-direction state)
  (first (rest (rest state))))

;; Test cases
(check-expect (get-x (mk-state 7 5 'South)) 7)
(check-expect (get-y (mk-state 7 5 'South)) 5)
(check-expect (get-x (mk-state 4 4 'East)) 4)
(check-expect (get-y (mk-state 4 4 'East)) 4)
(check-expect (get-direction (mk-state 3 4 'South)) 'South)
(check-expect (get-direction (mk-state 4 4 'East)) 'East)
;;---------------------------------------------------------------------------------------------------

;; Question 3(c): ) Using your helper functions, write a function robot-ctl.
;; The robot control function consumes a State and a command, in that order.
;; Commands are the symbols 'forward, 'turn-left, and 'turn-right.
;; The robot-ctl function produces a new State.
;;
;; Function: robot-ctl
;; Purpose: consumes a State and a Command and produces a changed State.
;; If Command is 'turn-left or 'turn-right, the robot’s Direction changes
;; If Command is 'forward', the position(x, y) of the robot changes according to it's Direction
;; and position(x, y)
;; A Command is (anyof 'forward 'turn-left 'turn-right)
;; Contract: robot-ctl: State Command -> State
;; Direction and State are defined in 3(a)
;; A Direction is (anyof 'North 'South 'West 'East)
;; A State is a (cons Int (cons Int (cons Direction empty)))

(define (robot-ctl state command) 
  (cond
    [(symbol=? command 'turn-left)
     (mk-state (get-x state) (get-y state) (turn-left (get-direction state)))]

    [(symbol=? command 'turn-right)
     (mk-state (get-x state) (get-y state) (turn-right (get-direction state)))]

    [(symbol=? command 'forward)
     (go-forward state)]))

(define (turn-left dir)
  (cond
    [(symbol=? dir 'North) 'West]
    [(symbol=? dir 'West)  'South]
    [(symbol=? dir 'South) 'East]
    [(symbol=? dir 'East)  'North]))

(define (turn-right dir)
  (cond
    [(symbol=? dir 'North) 'East]
    [(symbol=? dir 'East)  'South]
    [(symbol=? dir 'South) 'West]
    [(symbol=? dir 'West)  'North]))

(define (go-forward state)
  
(cond
  [(and (symbol=? (get-direction state) 'North)
        (< (get-y state) 10))
   (mk-state (get-x state) (+ (get-y state) 1) (get-direction state))]
  [(and (symbol=? (get-direction state) 'South)
        (> (get-y state) 0))
   (mk-state (get-x state) (- (get-y state) 1) (get-direction state))]
  [(and (symbol=? (get-direction state) 'East)
        (< (get-x state) 10))
   (mk-state (+ (get-x state) 1) (get-y state) (get-direction state))]
  [(and (symbol=? (get-direction state) 'West)
        (> (get-x state) 0))
   (mk-state (- (get-x state) 1) (get-y state) (get-direction state))]
  [else state]))

(check-expect (robot-ctl (mk-state 3 3 'North) 'turn-left)  (mk-state 3 3 'West))
(check-expect (robot-ctl (mk-state 3 3 'West)  'turn-left)  (mk-state 3 3 'South))
(check-expect (robot-ctl (mk-state 3 3 'South) 'turn-right) (mk-state 3 3 'West))
(check-expect (robot-ctl (mk-state 3 3 'East)  'turn-right) (mk-state 3 3 'South))
(check-expect (robot-ctl (mk-state 3 3 'North) 'forward) (mk-state 3 4 'North))
(check-expect (robot-ctl (mk-state 3 3 'South) 'forward) (mk-state 3 2 'South))
(check-expect (robot-ctl (mk-state 3 3 'East)  'forward) (mk-state 4 3 'East))
(check-expect (robot-ctl (mk-state 3 3 'West)  'forward) (mk-state 2 3 'West))
(check-expect (robot-ctl (mk-state 1 10 'North) 'forward) (mk-state 1 10 'North))
(check-expect (robot-ctl (mk-state 1 0  'South) 'forward) (mk-state 1 0  'South))
(check-expect (robot-ctl (mk-state 10 1 'East)  'forward) (mk-state 10 1 'East))
(check-expect (robot-ctl (mk-state 0  1 'West)  'forward) (mk-state 0  1 'West))

;;---------------------------------------------------------------------------------------------------
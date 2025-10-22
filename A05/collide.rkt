;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname collide) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;
;; ***********************************************
;; Hyeonbin Jung (21128068)            
;; CS 135 Fall 2025
;; Assignment 05, Problem 4
;; ***********************************************

;;---------------------------------------------------------------------------------------------------

;; Question 4(a): Write a new data definition for a robot’s State. You may assume that Direction
;; and Command have already been defined.
;; A State is a (list Nat Nat Direction Nat Nat)

;; Requires:
;; 3rd Nat should be bigger than 0
;; 3rd Nat >= 4th Nat 

;;---------------------------------------------------------------------------------------------------

;; Question 4(b): Write helper functions for a robot’s State, call them: mk-state, get-x, get-y,
;; get-direction, get-max-battery, get-cur-battery. Also, write a modified robot-ctl
;; function. 

;; mk-state : Nat Nat Direction Nat Nat -> State
(define (mk-state x y dir max-battery cur-battery)
  (list x y dir max-battery cur-battery))

;; Test cases for mk-state
(check-expect (mk-state 1 2 'North 10 5) (list 1 2 'North 10 5))
(check-expect (mk-state 0 0 'South 100 100) (list 0 0 'South 100 100))

;; get-x : State -> Nat
(define (get-x state)
  (first state))

;; Test cases for get-x
(check-expect (get-x (mk-state 5 7 'North 10 10)) 5)
(check-expect (get-x (mk-state 0 2 'East 20 15)) 0)

;; get-y : State -> Nat
(define (get-y state)
  (second state))

;; Test cases for get-y
(check-expect (get-y (mk-state 5 7 'North 10 10)) 7)
(check-expect (get-y (mk-state 1 0 'East 20 15)) 0)

;; get-direction : State -> Direction
(define (get-direction state)
  (third state))

;; Test cases for get-direction
(check-expect (get-direction (mk-state 1 2 'North 10 5)) 'North)
(check-expect (get-direction (mk-state 3 4 'West 50 20)) 'West)

;; get-max-battery : State -> Nat
(define (get-max-battery state)
  (first (rest (rest (rest state)))))

;; Test cases for get-max-battery
(check-expect (get-max-battery (mk-state 1 2 'North 10 5)) 10)
(check-expect (get-max-battery (mk-state 0 0 'East 100 80)) 100)

;; get-cur-battery : State -> Nat
(define (get-cur-battery state)
  (first (rest (rest (rest (rest state))))))

;; Test cases for get-cur-battery
(check-expect (get-cur-battery (mk-state 1 2 'North 10 5)) 5)
(check-expect (get-cur-battery (mk-state 0 0 'East 100 80)) 80)

;; (turn-left dir) produces a new Direction after turning left
;; turn-left : Direction -> Direction
(define (turn-left dir)
  (cond
    [(symbol=? dir 'North) 'West]
    [(symbol=? dir 'East) 'North]
    [(symbol=? dir 'South) 'East]
    [else 'South]))

;; Test cases for turn-left
(check-expect (turn-left 'North) 'West)
(check-expect (turn-left 'West) 'South)

;; (turn-right dir) produces a new Direction after turning right
;; turn-right : Direction -> Direction
(define (turn-right dir)
  (turn-left (turn-left (turn-left dir))))

;; Test cases for turn-right
(check-expect (turn-right 'North) 'East)
(check-expect (turn-right 'East) 'South)

;; robot-ctl : State Command -> State
;; Purpose: consume State and Command produce the robot's new State after executing a command
;; A Command is (anyof 'forward 'turn-left 'turn-right)
;; Reference: Sample Solution(https://student.cs.uwaterloo.ca/~cs135/assess/solutions/index.html)

(define MAX-COORDINATE 10)
(define MIN-COORDINATE 0)

(define (robot-ctl state cmd)
  (cond
    [(symbol=? cmd 'turn-left)
     (mk-state (get-x state)
               (get-y state)
               (turn-left (get-direction state))
               (get-max-battery state)
               (get-cur-battery state))]

    [(symbol=? cmd 'turn-right)
     (mk-state (get-x state)
               (get-y state)
               (turn-right (get-direction state))
               (get-max-battery state)
               (get-cur-battery state))]

    [(symbol=? (get-direction state) 'North)
     (mk-state (get-x state)
               (cond [(< (get-y state) MAX-COORDINATE) (+ (get-y state) 1)]
                     [else (get-y state)])
               (get-direction state)
               (get-max-battery state)
               (get-cur-battery state))]

    [(symbol=? (get-direction state) 'South)
     (mk-state (get-x state)
               (cond [(> (get-y state) MIN-COORDINATE) (- (get-y state) 1)]
                     [else (get-y state)])
               (get-direction state)
               (get-max-battery state)
               (get-cur-battery state))]

    [(symbol=? (get-direction state) 'West)
     (mk-state (cond [(> (get-x state) MIN-COORDINATE) (- (get-x state) 1)]
                     [else (get-x state)])
               (get-y state)
               (get-direction state)
               (get-max-battery state)
               (get-cur-battery state))]

    [else
     (mk-state (cond [(< (get-x state) MAX-COORDINATE) (+ (get-x state) 1)]
                     [else (get-x state)])
               (get-y state)
               (get-direction state)
               (get-max-battery state)
               (get-cur-battery state))]))

;; Test cases
(check-expect (robot-ctl (mk-state 0 0 'North 5 5) 'turn-left)
              (mk-state 0 0 'West 5 5))

(check-expect (robot-ctl (mk-state 0 0 'West 5 5) 'turn-right)
              (mk-state 0 0 'North 5 5))

(check-expect (robot-ctl (mk-state 0 0 'East 5 5) 'turn-right)
              (mk-state 0 0 'South 5 5))

(check-expect (robot-ctl (mk-state 0 6 'West 5 5) 'forward)
              (mk-state 0 6 'West 5 5))

(check-expect (robot-ctl (mk-state 3 0 'South 5 5) 'forward)
              (mk-state 3 0 'South 5 5))

(check-expect (robot-ctl (mk-state 2 10 'North 5 5) 'forward)
              (mk-state 2 10 'North 5 5))

(check-expect (robot-ctl (mk-state 10 10 'East 5 5) 'forward)
              (mk-state 10 10 'East 5 5))

(check-expect (robot-ctl (mk-state 2 6 'North 5 5) 'forward)
              (mk-state 2 7 'North 5 5))

(check-expect (robot-ctl (mk-state 2 6 'South 5 5) 'forward)
              (mk-state 2 5 'South 5 5))

(check-expect (robot-ctl (mk-state 2 6 'East 5 5) 'forward)
              (mk-state 3 6 'East 5 5))

(check-expect (robot-ctl (mk-state 2 6 'West 5 5) 'forward)
              (mk-state 1 6 'West 5 5))

;;---------------------------------------------------------------------------------------------------

;; Question 4(c): Write a predicate collide?
;; Function: collide?
;; Purpose: consume a list of commands for each robot and an
;; initial state for each robot, and produce true if the commands will cause the robots to
;; collide.
;; Contract: (listof Command) (listof Command) State State -> Bool

(define (collide? commands0 commands1 state0 state1)
  (cond

    [(and (empty? commands0) (empty? commands1))
     (and (= (get-x state0) (get-x state1))
          (= (get-y state0) (get-y state1)))]


    [(empty? commands0)
     (collide? commands0
                  (rest commands1)
                  state0
                  (robot-ctl state1 (first commands1)))]


    [(empty? commands1)
     (collide? (rest commands0)
                  commands1
                  (robot-ctl state0 (first commands0))
                  state1)]

    [else
     (collide? (rest commands0)
                  (rest commands1)
                  (robot-ctl state0 (first commands0))
                  (robot-ctl state1 (first commands1)))]))

;; Test cases
(check-expect (collide? empty empty (mk-state 3 2 'East 5 5)
(mk-state 8 2 'West 3 3))
false)
(check-expect (collide? empty (list 'forward 'forward 'forward)
(mk-state 3 2 'East 5 5) (mk-state 6 2 'West 3 3))
true)
(check-expect (collide? (list 'forward 'forward 'forward) empty
(mk-state 3 2 'East 5 5) (mk-state 6 2 'West 3 3))
true)
(check-expect (collide? (list 'forward) (list 'forward)
(mk-state 4 8 'South 5 5) (mk-state 6 7 'North 3 3))
false)
(check-expect (collide? (list 'turn-right) (list 'turn-left)
(mk-state 0 0 'East 5 5) (mk-state 1 1 'West 3 3))
false)



;;---------------------------------------------------------------------------------------------------

;; Question 4(d): Write a predicate collide-b?, similar to collide? from the previous,
;; part except using robots with batteries.
;; Function: collide-b?
;; Purpose: consume a list of commands for each robot and an
;; initial state for each robot, and produce true if the commands will cause the robots to
;; collide.
;; Contract: (listof Command) (listof Command) State State -> Bool

(define (collide-b? commands0 commands1 state0 state1)
  (cond
    [(and (empty? commands0) (empty? commands1))
     (and (= (get-x state0) (get-x state1))
          (= (get-y state0) (get-y state1)))]
    [(empty? commands0)
     (collide-b?
      commands0
      (rest commands1)
      state0
      (cond
        [(zero? (get-cur-battery state1))
         (mk-state (get-x state1)
                   (get-y state1)
                   (get-direction state1)
                   (get-max-battery state1)
                   (get-max-battery state1))]
        [else
         (mk-state (get-x (robot-ctl state1 (first commands1)))
                   (get-y (robot-ctl state1 (first commands1)))
                   (get-direction (robot-ctl state1 (first commands1)))
                   (get-max-battery state1)
                   (sub1 (get-cur-battery state1)))]))]
    [(empty? commands1)
     (collide-b?
      (rest commands0)
      commands1
      (cond
        [(zero? (get-cur-battery state0))
         (mk-state (get-x state0)
                   (get-y state0)
                   (get-direction state0)
                   (get-max-battery state0)
                   (get-max-battery state0))]
        [else
         (mk-state (get-x (robot-ctl state0 (first commands0)))
                   (get-y (robot-ctl state0 (first commands0)))
                   (get-direction (robot-ctl state0 (first commands0)))
                   (get-max-battery state0)
                   (sub1 (get-cur-battery state0)))])
      state1)]
    [else
     (collide-b?
      (cond
        [(zero? (get-cur-battery state0)) commands0]
        [else (rest commands0)])
      (cond
        [(zero? (get-cur-battery state1)) commands1]
        [else (rest commands1)])
      (cond
        [(zero? (get-cur-battery state0))
         (mk-state (get-x state0)
                   (get-y state0)
                   (get-direction state0)
                   (get-max-battery state0)
                   (get-max-battery state0))]
        [else
         (mk-state (get-x (robot-ctl state0 (first commands0)))
                   (get-y (robot-ctl state0 (first commands0)))
                   (get-direction (robot-ctl state0 (first commands0)))
                   (get-max-battery state0)
                   (sub1 (get-cur-battery state0)))])
      (cond
        [(zero? (get-cur-battery state1))
         (mk-state (get-x state1)
                   (get-y state1)
                   (get-direction state1)
                   (get-max-battery state1)
                   (get-max-battery state1))]
        [else
         (mk-state (get-x (robot-ctl state1 (first commands1)))
                   (get-y (robot-ctl state1 (first commands1)))
                   (get-direction (robot-ctl state1 (first commands1)))
                   (get-max-battery state1)
                   (sub1 (get-cur-battery state1)))]))]))

;; Test cases
;; Basic tests
(check-expect (collide-b? (list 'forward 'forward) (list 'forward
'forward 'forward)
(mk-state 2 2 'North 1 1) (mk-state 4 4 'West 3 3))
false)
(check-expect (collide-b? (list 'forward 'forward) (list 'forward
'forward 'forward 'forward)
(mk-state 2 2 'North 1 1) (mk-state 5 4 'West 3 3))
true)

;; additional tests
(check-expect
 (collide-b? (list 'forward 'turn-right 'forward)
             (list 'forward 'forward)
             (mk-state 5 5 'North 2 0)
             (mk-state 5 7 'South 2 2))
 false)
(check-expect
 (collide-b? (list 'forward 'turn-left 'forward 'forward)
             (list 'turn-right 'forward 'forward)
             (mk-state 1 1 'East 2 1)
             (mk-state 5 3 'South 3 3))
 false)
(check-expect
 (collide-b? (list 'forward 'forward 'turn-right 'forward)
             (list 'forward 'turn-left 'forward)
             (mk-state 8 8 'North 1 1)
             (mk-state 8 5 'South 4 4))
 false)

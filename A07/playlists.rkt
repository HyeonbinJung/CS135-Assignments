;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname playlists) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;
;; ***********************************************
;; Hyeonbin Jung (21128068)            
;; CS 135 Fall 2025
;; Assignment 07, Problem 3
;; ***********************************************

;;---------------------------------------------------------------------------------------------------

;; Helper functions
(define (mk-node label left right)
  (list label left right))

(define (get-label bt)
  (first bt))

(define (get-left bt)
  (second bt))

(define (get-right bt)
  (third bt))

;; Question 3 (a): Write the data definition for a BSongT

;; A BSongT (Binary Song Tree) is one of:
;; – empty
;; – (list (list Nat (listof Str)) BSongT BSongT)

;; Requires:
;;   For every node:
;;     all play counts in the left subtree are < this node’s play count
;;     all play counts in the right subtree are > this node’s play count

;; Question 3 (b): Write a function trending? that determines
;; if a song with a given title exists in the tree and meets or exceeds
;; a minimum threshold for trending status.

;; Purpose: Consumes a song title, a minimum play-count threshold,
;; and a BSongT.
;; produces true if a song with the given title exists in the tree and its play
;; otherwise, false.
;; Contract: String Nat BSongT -> Boolean

(define (trending? song minimum bt)
  (cond
    [(zero? (get-song-status song bt)) false]       
    [(>= (get-song-status song bt) minimum) true]     
    [else false]))           

(define (in? song lst)
  (cond
    [(empty? lst) false]
    [(string=? (first lst) song) true]
    [else (in? song (rest lst))]
    )
  )

(define (get-song-status song bt) 
  (cond [(empty? bt) 0]
        [(in? song (second (get-label bt))) (first (get-label bt))]
        [else (+ (get-song-status song (get-left bt)) (get-song-status song (get-right bt)))]))

;; Test cases
(define play-count-tree
(mk-node (list 1000 (list "Moonlight"))
(mk-node (list 500 (list "Sunrise" "Dawn")) empty empty)
(mk-node (list 1500 (list "Thunder")) empty
(mk-node (list 2000 (list "Storm")) empty empty))))
(check-expect (trending? "Thunder" 1000 play-count-tree) true)
(check-expect (trending? "Sunrise" 1000 play-count-tree) false)
(check-expect (trending? "Eclipse" 700 play-count-tree) false)

;; Question 3 (c): Write a function add-song that inserts a new song with a given play count into
;; the library.

;; Purpose: Consumes a song title, a play count,and a BSongT.
;; produces a new BSongT with a song title and play count added
;; Contract: Str Nat BSongT -> BSongT

(define (add-song song count bt)
  (cond [(empty? bt) (mk-node (list count (list song)) empty empty)]
        [(= (first (get-label bt)) count)
         (mk-node (list count (cons song (second (get-label bt))))
                               (get-left bt) (get-right bt))]
        [(< (first (get-label bt)) count)
         (mk-node (get-label bt) (get-left bt) (add-song song count (get-right bt)))]
        [else (mk-node (get-label bt) (add-song song count (get-left bt)) (get-right bt))])
  )

;; Test cases
(define library1
(mk-node (list 1000 (list "Moonlight"))
(mk-node (list 500 (list "Sunrise")) empty empty)
(mk-node (list 1500 (list "Thunder")) empty empty)))

(check-expect (add-song "First" 100 empty)
(mk-node (list 100 (list "First")) empty empty))

(check-expect
(add-song "Twilight" 1000 library1)
(mk-node (list 1000 (list "Twilight" "Moonlight"))
(mk-node (list 500 (list "Sunrise")) empty empty)
(mk-node (list 1500 (list "Thunder")) empty empty)))
(check-expect
(add-song "Rainbow" 750 library1)
(mk-node (list 1000 (list "Moonlight"))
(mk-node (list 500 (list "Sunrise")) empty
(mk-node (list 750 (list "Rainbow")) empty empty))
(mk-node (list 1500 (list "Thunder")) empty empty)))

;; Question 3 (d): Write a function update-plays that updates a song’s play count in the library.

;; Purpose: Consumes a song title, a new play count, and a BSongT
;; Produces a new BSongT with updates(remove the song from its current play count node and add
;; it to the node with the new play count. )
;; Contract: Str Nat BSongT -> BSongT

(define (delete song list)
  (cond
    [(empty? list) empty]
    [(string=? song (first list)) (delete song (rest list))]
    [else (cons (first list) (delete song (rest list)))]))

(define (delete-from-bt song bt)
  (cond
    [(empty? bt) empty]
    [(in? song (second (get-label bt)))
     (mk-node (list (first (get-label bt)) (delete song (second (get-label bt))))
                                                    (get-left bt) (get-right bt))]
    [else (mk-node (get-label bt)
                   (delete-from-bt song (get-left bt)) (delete-from-bt song (get-right bt)))]
    )
  )

(define (update-plays song count bt)
  (add-song song count (delete-from-bt song bt)))

;; Test cases
(define library2
(mk-node (list 1000 (list "Moonlight" "Sunset"))
(mk-node (list 500 (list "Sunrise")) empty empty)
(mk-node (list 1500 (list "Thunder")) empty empty)))
(check-expect (update-plays "Sunrise" 1500 library2)
(mk-node (list 1000 (list "Moonlight" "Sunset"))
(mk-node (list 500 empty) empty empty)
(mk-node (list 1500
(list "Sunrise" "Thunder"))
empty empty)))
(check-expect (update-plays "Crusher" 500 library2)
(mk-node (list 1000 (list "Moonlight" "Sunset"))
(mk-node (list 500 (list "Crusher" "Sunrise"))
empty empty)
(mk-node (list 1500 (list "Thunder")) empty
empty)))


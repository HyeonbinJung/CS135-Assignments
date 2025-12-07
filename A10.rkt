;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname sudoku) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;
;; ***********************************************
;; Hyeonbin Jung (21128068)            
;; CS 135 Fall 2025
;; Assignment 10, Problem 1,2,3,4,5
;; ***********************************************

;;---------------------------------------------------------------------------------------------------

;; 1 (a)
;; all-satisfy? : (X -> Bool) (matrixof X) -> Bool
(define (all-satisfy? pred? matrix)
  (local
    [(define (row-ok? row)
       (foldr (lambda (x acc) (and x acc))
              true
              (map pred? row)))]
    (foldr (lambda (x acc) (and x acc))
           true
           (map row-ok? matrix))))

;; Test cases
(check-expect (all-satisfy? integer? '((2 3 4) (5 6 7)))
true)
(check-expect (all-satisfy? integer? '((2 3 4) (5 six 7)))
false)

;; 1 (b)
;; any-satisfy?: (X -> Bool) (matrixof X) -> Bool
(define (any-satisfy? pred? matrix)
  (local
    [(define (row-has? row)
       (foldr (lambda (x acc) (or x acc))
              false
              (map pred? row)))]
    (foldr (lambda (x acc) (or x acc))
           false
           (map row-has? matrix))))

;; Test cases
(check-expect (any-satisfy? symbol? '((2 3 4) (5 6 7)))
false)
(check-expect (any-satisfy? symbol? '((2 3 4) (5 six 7)))
true)


;; 1 (c)
;; find-where: (X -> Bool) (matrixof X) -> (list Nat Nat)

(define (find-where pred? matrix)
  (local
    [
     (define (scan-row row col row-index)
       (cond
         [(empty? row) #false]
         [(pred? (first row)) (list col row-index)]
         [else (scan-row (rest row) (+ col 1) row-index)]))

     (define (scan-matrix mat row-index)
       (cond
         [(empty? mat) #false]
         [else
          (local [(define found (scan-row (first mat) 0 row-index))]
            (cond
              [(list? found) found] 
              [else (scan-matrix (rest mat) (+ row-index 1))]))]))]

    (scan-matrix matrix 0)))

;; Test cases
(define wherematrix '(( 1 2 3 4 )
( 4 5 (3 6) (1 2) )
( (7) 8 9 () )))
(check-expect (find-where list? wherematrix) '(2 1))
(check-expect (find-where empty? wherematrix) '(3 2))
(check-expect (find-where integer? wherematrix) '(0 0))

;;---------------------------------------------------------------------------------------------------

;; 2
;; strings->puzzle : (listof Str) -> (matrixof Cell)
(define (strings->puzzle los)
  (local
    [(define n (string-length (first los)))

     (define all-vals
       (build-list n (lambda (i) (add1 i))))

     (define (char->cell ch)
       (cond
         [(char=? ch #\?) all-vals]
         [else
          (list (- (char->integer ch)
                   (char->integer #\0)))]))

     (define (chars->row chars)
       (cond
         [(empty? chars) '()]
         [else
          (cons (char->cell (first chars))
                (chars->row (rest chars)))]))

     (define (string->row s)
       (chars->row (string->list s)))]
    
    (map string->row los)))

;; Test cases
(check-expect (strings->puzzle '("???"
"?3?"
"??2"))
'(( (1 2 3) (1 2 3) (1 2 3) )
( (1 2 3) (3) (1 2 3) )
( (1 2 3) (1 2 3) (2) )))
(check-expect (strings->puzzle '("??3?"
"??2?"
"?4??"
"????"))
'(( (1 2 3 4) (1 2 3 4) (3) (1 2 3 4) )
( (1 2 3 4) (1 2 3 4) (2) (1 2 3 4) )
( (1 2 3 4) (4) (1 2 3 4) (1 2 3 4) )
( (1 2 3 4) (1 2 3 4) (1 2 3 4) (1 2 3 4) )))

;;---------------------------------------------------------------------------------------------------

;; 3
(define (single? cell)
  (cond
    [(and (list? cell) (= (length cell) 1)) #t]
    [else #f]))

;; value inside single
(define (cell-val cell)
  (first cell))

;; cell remove 
(define (remove-val-cell v cell)
  (cond
    [(list? cell)
     (filter (lambda (x) (not (= x v))) cell)]
    [else cell]))

;; set a cell to v
(define (set-cell puzzle col row v)
  (build-list (length puzzle)
    (lambda (r)
      (build-list (length (first puzzle))
        (lambda (c)
          (cond
            [(and (= r row) (= c col)) v]
            [else (list-ref (list-ref puzzle r) c)]))))))

;; remove value v from row
(define (remove-from-row puzzle row v)
  (build-list (length puzzle)
    (lambda (r)
      (build-list (length (first puzzle))
        (lambda (c)
          (local [(define cell (list-ref (list-ref puzzle r) c))]
            (cond
              [(= r row) (remove-val-cell v cell)]
              [else cell])))))))

;; remove value v from column
(define (remove-from-col puzzle col v)
  (build-list (length puzzle)
    (lambda (r)
      (build-list (length (first puzzle))
        (lambda (c)
          (local [(define cell (list-ref (list-ref puzzle r) c))]
            (cond
              [(= c col) (remove-val-cell v cell)]
              [else cell])))))))

(define (remove-singles puzzle)
  (local
    [
     (define (remove-singles-step pz)
       (local
         [(define where (find-where single? pz))
          (define col (first where))
          (define row (second where))
          (define cell (list-ref (list-ref pz row) col))
          (define v (cell-val cell))
          (define p1 (set-cell pz col row v))
          (define p2 (remove-from-row p1 row v))
          (define p3 (remove-from-col p2 col v))]
         p3))]

    (cond
      [(not (any-satisfy? single? puzzle))
       puzzle]
      [else
       (remove-singles (remove-singles-step puzzle))])))

;; Test cases
(check-expect (remove-singles (strings->puzzle '("??3?"
"??2?"
"?4??"
"????")))
'(( (1 2 4) (1 2) 3 (1 2 4) )
( (1 3 4) (1 3) 2 (1 3 4) )
( (2 3) 4 1 (2 3) )
( (1 2 3) (1 2 3) 4 (1 2 3) )))

(check-expect
 (remove-singles (strings->puzzle '("1??" "?2?" "??3")))
 '((1  3  2)
   (3  2  1)
   (2  1  3)))

;;---------------------------------------------------------------------------------------------------

;; 4
(define (solve-latin pred? puzzle)
  (local
    [
     (define (nth lst n)
       (cond
         [(zero? n) (first lst)]
         [else (nth (rest lst) (sub1 n))]))

     (define (contradiction? p)
       (any-satisfy?
        (lambda (cell)
          (and (list? cell) (empty? cell)))
        p))

     (define (solved? p)
       (all-satisfy? integer? p))

     (define (choice-cell? cell)
       (and (list? cell)
            (not (empty? cell))))

     (define (try-choices choices p col row)
       (cond
         [(empty? choices) empty]
         [else
          (local
            [(define guess (first choices))
             (define guessed-puzzle
               (set-cell p col row (list guess)))
             (define result (solve-latin pred? guessed-puzzle))]
            (cond
              [(empty? result)
               (try-choices (rest choices) p col row)]
              [else
               result]))]))]

    (local
      [(define simplified (remove-singles puzzle))]
      (cond
        [(contradiction? simplified)
         empty]
        [(solved? simplified)
         (cond
           [(pred? simplified) simplified]
           [else empty])]
        [else
         (local
           [(define where (find-where choice-cell? simplified))
            (define col (first where))
            (define row (second where))
            (define choices (nth (nth simplified row) col))]
           (try-choices choices simplified col row))]))))

(define (yes x) true)

(define (no x) false)

(define (diagonal-has-2? p)
  (and (not (empty? p))
       (or (= 2 (first (first p)))
           (diagonal-has-2? (map rest (rest p))))))

;; Test cases
(define 23puzzle (strings->puzzle '("???"
"?3?"
"??2")))
(check-expect (solve-latin yes 23puzzle)
'((1 2 3)
(2 3 1)
(3 1 2)))
(define 324puzzle (strings->puzzle '("??3?"
"??2?"
"?4??"
"????")))
(check-expect (solve-latin yes 324puzzle)
'((1 2 3 4)
(4 1 2 3)
(3 4 1 2)
(2 3 4 1)))
(check-expect (solve-latin diagonal-has-2? 324puzzle)
'((1 2 3 4)
(4 3 2 1)
(2 4 1 3)
(3 1 4 2)))
(check-expect (solve-latin no 324puzzle) empty)

;;---------------------------------------------------------------------------------------------------

;; 5

(define (sudoku? sol)
  (local
    [
     (define (nth lst n)
       (cond
         [(zero? n) (first lst)]
         [else (nth (rest lst) (- n 1))]))

     (define (lists-same? a b)
       (cond
         [(and (empty? a) (empty? b)) true]
         [(or (empty? a) (empty? b)) false]
         [(= (first a) (first b))
          (lists-same? (rest a) (rest b))]
         [else false]))

     (define (block-values r c)
       (append
        (list (nth (nth sol r) c)
              (nth (nth sol r) (+ c 1))
              (nth (nth sol r) (+ c 2)))
        (list (nth (nth sol (+ r 1)) c)
              (nth (nth sol (+ r 1)) (+ c 1))
              (nth (nth sol (+ r 1)) (+ c 2)))
        (list (nth (nth sol (+ r 2)) c)
              (nth (nth sol (+ r 2)) (+ c 1))
              (nth (nth sol (+ r 2)) (+ c 2)))))

     (define (valid-block? vals)
       (lists-same? (sort vals <)
                    '(1 2 3 4 5 6 7 8 9)))

     (define row-starts '(0 3 6))
     (define col-starts '(0 3 6))

     (define (check-cols r cs)
       (cond
         [(empty? cs) true]
         [(valid-block? (block-values r (first cs)))
          (check-cols r (rest cs))]
         [else false]))

     (define (check-rows rs)
       (cond
         [(empty? rs) true]
         [(check-cols (first rs) col-starts)
          (check-rows (rest rs))]
         [else false]))]

    (check-rows row-starts)))

(define sample-sudoku (strings->puzzle '("?7????1?8"
"??5?1??8?"
"?6?7?5???"
"2??93????"
"3?1???8?6"
"????28??1"
"???2?7?1?"
"?1??4?2??"
"8?2????6?")))

(check-expect (sudoku? '((1 2 3 4 5 6 7 8 9)
(4 6 5 7 8 9 1 2 3)
(7 8 9 1 2 3 4 5 6)
(4 5 6 7 8 9 1 2 3)
(1 2 3 4 5 6 7 8 9)
(7 8 9 1 2 3 4 5 6)
(7 8 9 1 2 3 6 5 4)
(4 5 6 7 8 9 1 2 3)
(3 2 1 4 5 6 7 8 9))) true)

(check-expect (sudoku? '((1 2 3 4 5 6 7 8 9)
(4 5 6 7 8 9 1 2 3)
(7 8 9 1 2 3 4 5 6)
(4 5 6 7 8 9 1 2 3)
(1 2 3 4 5 6 7 8 9)
(7 8 9 1 2 3 4 5 6)
(7 8 9 1 2 3 4 5 7)
(4 5 6 7 8 9 1 2 3)
(1 2 3 4 5 6 7 8 9))) false)


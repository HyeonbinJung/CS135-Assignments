;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname filedir) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;
;; ***********************************************
;; Hyeonbin Jung (21128068)            
;; CS 135 Fall 2025
;; Assignment 08, Problem 3
;; ***********************************************

;;---------------------------------------------------------------------------------------------------

;; Import
(require "fs-print.rkt")


;; Problem 3 (a)
;; list-files: list-files: FileDir -> (listof Str)
;; Purpose: consume FileDir, produce a list of all file names that appear
;; in the FileDir and all of its subdirectories. 

(define (list-files fd)
  (local
    [
     (define (list-files-fdl lst)
       (cond
         [(empty? lst)
          empty]
         [else
          (append (list-files (first lst))
                  (list-files-fdl (rest lst)))]))
     ]
    (cond
      [(file? fd)
       (list (file-name fd))]

      [else   
       (list-files-fdl (dir-contents fd))])))

;; Test cases
(check-expect
 (list-files (make-file "a.txt" 10 5))
 (list "a.txt"))
(check-expect
 (list-files (make-dir "empty" empty))
 empty)
(check-expect
 (list-files sample-fs)
 (list
  "readme.txt"

  "marks.jpg"
  "timbit.jpg"
  "luna.jpg"
  "beach1.jpg"
  "beach2.jpg"
  "beach3.jpg"
  "oreo.jpg"
  "anna.jpg"
  "beach1.jpg"

  "eagles-hotel-california.mp3"
  "bee-gees-stayin-alive.mp3"

  "lady-gaga-bad-romance.mp3"
  "beyonce-single-ladies.mp3"

  "shopping.txt"
  "todo.txt"))


;; Problem 3 (b)
;; backup: Dir -> Dir
;; Purpose: consume a directory and produce a new directory where every file is duplicated with .bak.

(define (backup d)
  (local
    [
     (define (backup-fdl lst)
       (cond
         [(empty? lst)
          empty]

         [else
          (append (backup-fd (first lst))
                  (backup-fdl (rest lst)))]))
     (define (backup-fd fd)
       (cond
         [(file? fd)
          (list
           fd
           (make-file
            (string-append (file-name fd) ".bak")
            (file-size fd)
            (file-timestamp fd)))]
         [else
          (list
           (make-dir (dir-name fd)
                     (backup-fdl (dir-contents fd))))]))
     ]
    (make-dir (dir-name d)
              (backup-fdl (dir-contents d)))))


;; Test cases
(check-expect
 (backup (make-dir "root"
                   (list (make-file "a.txt" 10 100))))
 (make-dir "root"
           (list
            (make-file "a.txt" 10 100)
            (make-file "a.txt.bak" 10 100))))

(check-expect
 (backup (make-dir "empty" empty))
 (make-dir "empty" empty))


;; Problem 3 (c)
;; get-time: FileDir -> (anyof Nat false)
;; Purpose: consume FileDir, produces the largest timestamp of all files in the FileDir,
;; or false if it contains no files.

(define (get-time fd)
  (local
    [
     (define (fdl-helper fdl)
       (cond
         [(empty? fdl) #false]

         [else
          (local [(define first-time (dispat (first fdl)))
                  (define rest-time  (fdl-helper (rest fdl)))]

            (cond
              [(and (not (number? first-time))
                    (not (number? rest-time)))
               #false]

              [(not (number? first-time))
               rest-time]

              [(not (number? rest-time))
               first-time]

              [else
               (max first-time rest-time)]))]))

     (define (dispat fd)
       (cond
         [(file? fd) (file-timestamp fd)]
         [(dir? fd)  (fdl-helper (dir-contents fd))]))]

    (dispat fd)))


;; Problem 3 (d)
;; find: Str Dir -> (listof (listof Str))
;; Purpose: consume a name and a directory and produce the path to it. 
;; (produce empty if no matching File or Dir exists)


(define (find name dir)
  (local
    [(define (find-in fdlist path)
       (cond
         [(empty? fdlist) empty]
         [else
          (append
           (find-in (rest fdlist) path)  
           (find-one (first fdlist) path))]))
     (define (find-one fd path)
       (cond

         [(file? fd)
          (cond [(string=? (file-name fd) name)
                 (list (append path (list (file-name fd))))]
                [else empty])]

         [else
          (local [(define dname (dir-name fd))
                  (define new-path (append path (list dname)))
                  (define subs (find-in (dir-contents fd) new-path))]
            (cond [(string=? dname name)
                   (cons new-path subs)]
                  [else subs]))]))]

    (find-in (dir-contents dir)
             (list (dir-name dir)))))


;; Test cases
(check-expect (find "vacation" sample-fs)
(list (list "root" "photos" "vacation")))
(check-expect (find "shopping.txt" sample-fs)
(list (list "root" "notes" "shopping.txt")))
(check-expect (find "beach1.jpg" sample-fs)
(list (list "root" "photos" "beach1.jpg")
(list "root" "photos" "vacation" "beach1.jpg")))
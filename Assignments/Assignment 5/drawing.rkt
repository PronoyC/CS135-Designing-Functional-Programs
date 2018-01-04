;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname drawing) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;; ***************************************************
;;   Pronoy Chaudhuri (20709909)
;;   CS 135 Fall 2017
;;   Assignment 05, Problem 3
;; ***************************************************
;;

;; Ask Racket to give us access to the data definitions and functions in
;; the file drawinglib.rkt.
(require "drawinglib.rkt")

;; A demonstration of the drawing in the assignment.
(define samplepic (list
                   (make-square135 (make-posn 0 0) 50 '(255 0 0))
                   (make-square135 (make-posn 50 50) 50 '(0 0 255))
                   (make-circle135 (make-posn 50 50) 25 '(0 255 0))))

;; Type this line into the interactions window to see the picture:
;; (draw-picture samplepic 100 100)

;; 3(a)

;; example-drawing displays a preset image
;; example-drawing: Drawing
;; No examples can be provided

(define example-drawing
  (list
   (make-square135 (make-posn 0 0) 150 '(255 0 0))
   (make-square135 (make-posn 0 0) 75 '(255 255 255))
   (make-square135 (make-posn 75 0) 75 '(255 255 255))
   (make-circle135 (make-posn 75 75) 75 '(255 0 0))
   (make-square135 (make-posn 150 150) 150 '(255 165 0))
   (make-square135 (make-posn 150 150) 75 '(255 255 255))
   (make-square135 (make-posn 225 150) 75 '(255 255 255))
   (make-circle135 (make-posn 225 225) 75 '(255 165 0))
   (make-square135 (make-posn 150 0) 150 '(0 255 255))
   (make-square135 (make-posn 150 0) 75 '(255 255 255))
   (make-square135 (make-posn 225 0) 75 '(255 255 255))
   (make-circle135 (make-posn 225 75) 75 '(0 255 255))
   (make-square135 (make-posn 0 150) 150 '(255 192 203))
   (make-square135 (make-posn 0 150) 75 '(255 255 255))
   (make-square135 (make-posn 75 150) 75 '(255 255 255))
   (make-circle135 (make-posn 75 225) 75 '(255 192 203))
   (make-circle135 (make-posn 50 50) 15 '(255 255 255))
   (make-circle135 (make-posn 100 50) 15 '(255 255 255))
   (make-circle135 (make-posn 55 55) 5 '(0 0 255))
   (make-circle135 (make-posn 105 55) 5 '(0 0 255))
   (make-circle135 (make-posn 50 200) 15 '(255 255 255))
   (make-circle135 (make-posn 100 200) 15 '(255 255 255))
   (make-circle135 (make-posn 55 195) 5 '(0 0 255))
   (make-circle135 (make-posn 105 195) 5 '(0 0 255))
   (make-circle135 (make-posn 200 200) 15 '(255 255 255))
   (make-circle135 (make-posn 250 200) 15 '(255 255 255))
   (make-circle135 (make-posn 195 195) 5 '(0 0 255))
   (make-circle135 (make-posn 245 195) 5 '(0 0 255))
   (make-circle135 (make-posn 200 50) 15 '(255 255 255))
   (make-circle135 (make-posn 250 50) 15 '(255 255 255))
   (make-circle135 (make-posn 195 55) 5 '(0 0 255))
   (make-circle135 (make-posn 245 55) 5 '(0 0 255))
   (make-circle135 (make-posn 150 150) 50 '(255 255 0))
   (make-circle135 (make-posn 125 140) 9 '(0 0 0))
   (make-circle135 (make-posn 175 140) 9 '(0 0 0))
   (make-circle135 (make-posn 150 160) 15 '(0 0 0))
   (make-square135 (make-posn 148 145) 2 '(255 255 255))
   (make-square135 (make-posn 150 145) 2 '(255 255 255))))

;; Tests:
;; (draw-picture example-drawing 300 300)
;; produces baby pakman getting caught by all four ghosts :)

;; 3(b)

;; (cull drawing m n) simplifies a drawing by keeping
;; select amount of squares and circles based on parameters
;; m and n respectively
;; cull: Drawing Nat Nat -> Drawing
;; Examples:
(check-expect (cull example-drawing 0 0) empty)
(check-expect (cull example-drawing 2 2)
              (list
               (make-square135 (make-posn 0 0) 150 (list 255 0 0))
               (make-square135 (make-posn 0 0) 75 (list 255 255 255))
               (make-circle135 (make-posn 75 75) 75 (list 255 0 0))
               (make-circle135 (make-posn 225 225) 75 (list 255 165 0))))

(define (cull drawing m n)
  (cond
    [(empty? drawing) empty]
    [(and (> m 0) (square135? (first drawing)))
     (cons (first drawing) (cull (rest drawing) (- m 1) n))]
    [(and (> n 0) (circle135? (first drawing)))
     (cons (first drawing) (cull (rest drawing) m (- n 1)))]
    [else (cull (rest drawing) m n)]))

;; Tests:
(check-expect (cull example-drawing 1 0)
              (list (make-square135 (make-posn 0 0) 150 '(255 0 0))))
(check-expect (cull example-drawing 0 1)
              (list (make-circle135 (make-posn 75 75) 75 (list 255 0 0))))

;; 3(c)

;; (ring-radius n) calculates radius of particular ring
;; ring-radius: Nat -> Num
;; Examples:
(check-expect (ring-radius 5) 20)
(check-expect (ring-radius 1) 100)

(define (ring-radius n)
  (/ 100 n))
  
;; (circle-maker n radius) produces n number of alternating red and white
;; rings for a drawing while ensuring the distance between rings are the same
;; using radius
;; circle-maker: Nat -> Drawing
;; Examples:
(check-expect (circle-maker 0 50) empty)
(check-expect (circle-maker 2 50)
              (list (make-circle135 (make-posn 100 100) 100 '(255 0 0))
                    (make-circle135 (make-posn 100 100) 50 '(255 255 255))))
              
(define (circle-maker n radius)
  (cond
    [(<= n 0) empty]
    [(and (even? n)(> n 0))
     (cons (make-circle135 (make-posn 100 100) (* n radius) '(255 0 0))
           (circle-maker (- n 1) radius))]
    [else
     (cons (make-circle135 (make-posn 100 100) (* n radius) 
                           '(255 255 255)) (circle-maker (- n 1) radius))])) 

;; (bullseye n) produces n number of alternating red and white
;; rings for a drawing
;; bullseye: Nat -> Drawing
;; Examples:
(check-expect (bullseye 1)
              (list (make-circle135 (make-posn 100 100) 100 '(255 255 255))))
(check-expect (bullseye 2)
              (list (make-circle135 (make-posn 100 100) 100 '(255 0 0))
                    (make-circle135 (make-posn 100 100) 50 '(255 255 255))))

(define (bullseye n)
  (circle-maker n (ring-radius n)))

;; Tests:
(check-expect (bullseye 4)
              (list (make-circle135 (make-posn 100 100) 100 '(255 0 0))
                    (make-circle135 (make-posn 100 100) 75 '(255 255 255))
                    (make-circle135 (make-posn 100 100) 50 '(255 0 0))
                    (make-circle135 (make-posn 100 100) 25 '(255 255 255))))
(check-expect (bullseye 5)
              (list (make-circle135 (make-posn 100 100) 100 '(255 255 255))
                    (make-circle135 (make-posn 100 100) 80 '(255 0 0))
                    (make-circle135 (make-posn 100 100) 60 '(255 255 255))
                    (make-circle135 (make-posn 100 100) 40 '(255 0 0))
                    (make-circle135 (make-posn 100 100) 20 '(255 255 255))))

;; 3(d)

;; (square row column colour) produces a square at a particular position using
;; row and column number, with a distinct colour
;; square: Nat Nat Colour -> Square135
;; Examples:
(check-expect (square 1 1 '(0 0 0))
              (make-square135 (make-posn 10 10) 10 '(0 0 0)))
              
(define (square row column colour)
  (make-square135 (make-posn (* 10 row) (* 10 column)) 10 colour))

;; (circle row column colour) produces a circle at a particular position using
;; row and column number, with a distinct colour
;; circle: Nat Nat Colour -> Circle135
;; Examples:
(check-expect (circle 1 1 '(255 0 0))
              (make-circle135 (make-posn 15 15) 5 '(255 0 0)))

(define (circle row column colour)
  (make-circle135 (make-posn (+ 5 (* 10 row))
                             (+ 5 (* 10 column))) 5 colour))

;; (single-row row n c1 c2) uses row and the value of n to determine the
;; order of objects in a single row and assigns them respective colours
;; single-row: Nat Nat Nat Colour Colour -> Drawing
;; Examples:
(check-expect (single-row 1 1 1 '(0 0 0) '(255 0 0))
              (list (make-square135 (make-posn 10 10) 10(list 0 0 0))))  

(define (single-row row n n-old c1 c2)
  (cond
    [(zero? (+ n 1)) empty]
    [(= n-old n) (single-row row (- n 1) n-old c1 c2)]
    [(even? (+ row n)) (cons (square row n c1)
                             (single-row row (- n 1) n-old c1 c2))]
    [else (cons (circle row n c2)
                (single-row row (- n 1) n-old c1 c2))])) 

;; (multiple-rows n row c1 c2) creates n number of rows, drafting a drawing and
;; adding colours c1 and c2 to appropriate elements
;; multiple-rows: Nat Nat Colour Colour -> Drawing
;; Examples:
(check-expect (multiple-rows 1 1 '(255 0 0) '(0 0 0) 1) empty)

(define (multiple-rows n row c1 c2 n-old)
  (cond
    [(= row n) empty]
    [else (append (single-row row n n-old c1 c2)
                  (multiple-rows n (+ row 1) c1 c2 n-old))]))

;; (checkerboard n c1 c2) produces a checkerboard that alternates between
;; squares with one colour (c1) and circles with another (c2) with number of
;; rows/columns provided by n
;; checkerboard: Nat Colour Colour -> Drawing
;; Examples:
(check-expect (checkerboard 1 '(255 0 0) '(0 0 0))
              (list (make-circle135 (make-posn 5 15) 5 (list 0 0 0)))) 

(define (checkerboard n c1 c2)
  (multiple-rows n 0 c1 c2 n))








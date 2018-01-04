;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname nestlist) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;
;; ***************************************************
;;   Pronoy Chaudhuri (20709909)
;;   CS 135 Fall 2017
;;   Assignment 08, Problem 3
;; ***************************************************
;;

;; 3(a)

;; (nfoldr func1 func2 val lon) combines two functions
;; (denoted by "func1" and "func2") that are applied to
;; a nested list (denoted by "lon") with a base case value of y
;; nfoldr: (X Y -> Y) (Y Y -> Y) Y Nested-Listof-X -> Y
;; Examples:
(check-expect (count-items '(1 (2 3) () ((4)))) 4)
(check-expect (flatten '(1 (2 3) () ((4)))) '(1 2 3 4))
(check-expect (nfoldr f g b '()) b)

(define (nfoldr func1 func2 val lon)
  (cond
    [(empty? lon) val]
    [(list? (first lon)) (func2 (nfoldr func1 func2 val (first lon))
                                (nfoldr func1 func2 val (rest lon)))] 
    [else (func1 (first lon) (nfoldr func1 func2 val (rest lon)))]))

;; Tests:
(check-expect (nfoldr f g b '(1 2 3)) (f 1 (f 2 (f 3 b))))
(check-expect (nfoldr f g b '(1 (2 3) 4)) (f 1 (g (f 2 (f 3 b)) (f 4 b))))
(check-expect (nfoldr f g b '(1 (2 3) () ((4))))
              (f 1 (g (f 2 (f 3 b)) (g b (g (g (f 4 b) b) b)))))

;; Functions Used:
(define (count-items nln) (nfoldr (lambda (x y) (add1 y)) + 0 nln))
(define (flatten lst) (nfoldr cons append empty lst))
(define f +)
(define g -)
(define b 2)

;; 3(b)

;; (nfilter func1 lon) applies a function (denoted by "func1") to a nested list
;; (denoted by "lon") to produce a list that is a subset or equal to lon
;; nfilter: (X -> Bool) Nested-Listof-X -> Nested-Listof-X
;; Examples:
(check-expect (nfilter odd? '(1 (2 3) () ((4)))) '(1 (3) () (())))
(check-expect (nfilter integer? empty) empty)

(define (nfilter func1 lon)
  (nfoldr (lambda (x y) (cond [(func1 x) (cons x y)] [else y]))
          (lambda (x y) (cons x y)) empty lon))

;; Tests:
(check-expect (nfilter positive? '(1 (-2 3) () (4 -5))) '(1 (3) () (4)))
(check-expect (nfilter integer? '(1.1 ("happy" 2))) '((2)))

;; 3(c)

;; (nmap func1 lon) applies a function (denoted by "func1") to a nested list
;; (denoted by "lon")
;; nmap: (X -> Y) Nested-Listof-X -> Nested-Listof-Y
;; Examples:
(check-expect (nmap sqr '(1 (2 3) () ((4)))) '(1 (4 9) () ((16))))
(check-expect (nmap add1 empty) empty)

(define (nmap func1 lon)
  (nfoldr (lambda (x y) (cons (func1 x) y)) cons empty lon))

;; Tests:
(check-expect (nmap sub1 '(3 (54 3 (2)) 1 -1)) '(2 (53 2 (1)) 0 -2))
(check-expect (nmap abs '(-3 (54 3 (2)) -1 -10)) '(3 (54 3 (2)) 1 10))

;; 3(d)

;; (nreverse lon) reverses the elements in a nested list (denoted by "lon")
;; nreverse: Nested-Listof-X -> Nested-Listof-X
;; Examples:
(check-expect (nreverse '(1 (2 3) () ((4)))) '(((4)) () (3 2) 1))
(check-expect (nreverse '((1 (2 3)) 4 (5 (6 7 8) 9)))
              '((9 (8 7 6) 5) 4 ((3 2) 1)))
(check-expect (nreverse empty) empty)

(define (nreverse lon)
  (nfoldr (lambda (x y) (append y (list x)))
          (lambda (x y) (append y (list x))) empty lon))

;; Tests:
(check-expect (nreverse '(1 3 (3 9 4 0) 5 (6 (7))))
              '(((7) 6) 5 (0 4 9 3) 3 1))
(check-expect (nreverse '(1 1 1 1 1 2 1 1 1))
              '(1 1 1 2 1 1 1 1 1))

;; 3(e)

;; Constants:
(define min-height 1)

;; (nheight lon) determines the height of a nested list (denoted by "lon")
;; nheight: Nested-Listof-X -> Nat
;; Examples:
(check-expect (nheight '()) 1)
(check-expect (nheight '(a b c)) 1)
(check-expect (nheight '((1 a) (2 b) (3 c))) 2)

(define (nheight lon)
  (nfoldr (lambda (x y) (max min-height y))
          (lambda (x y) (max (add1 x) y)) min-height lon))

;; Tests:
(check-expect (nheight '(1 (2 3) () ((4)))) 3)
(check-expect (nheight '((1 (2 3)) 4 (5 (6 7 8) 9))) 3)

;; 3(f)

;; (prune lon) removes all empty lists within a nested list (denoted by "lon")
;; prune: Nested-Listof-X -> Nested-Listof-X
;; Examples:
(check-expect (prune '(1 (2 3 ()) ( (()) (4) ()(())))) '(1 (2 3) ((4))))
(check-expect (prune empty) empty)

(define (prune lon)
  (nfoldr (lambda (x y) (cons x y))
          (lambda (x y) (cond [(empty? x) y] [else (cons x y)])) empty lon))

;; Tests:
(check-expect (prune '(()((())()))) '())
(check-expect (prune '(1 (56 5) () ((6 4)) (3 ()))) '(1 (56 5) ((6 4)) (3)))
                        



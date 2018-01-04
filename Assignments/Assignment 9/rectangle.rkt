;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname rectangle) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Pronoy Chaudhuri (20709909)
;; CS135 Fall 2017
;; Assignment 09, Question 02
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require "rectanglelib.rkt")

(define-struct cell (num used?))
;; A Cell is a (make-cell Nat Bool)

;; A Grid is a (listof (listof Cell))
;; requires: the grid contains a non-empty list of non-empty lists,
;;  all the same length.

(define-struct rect (x y w h))
;; A Rect is a (make-rect Nat Nat Nat Nat)

(define-struct state (grid rects))
;; A State is a (make-state Grid (listof Rect))


;; Here are a couple of constants that can be used to define
;; the puzzle in the assignment, and a random larger puzzle.

(define puzz '((0 0 0 0 0 5 0)
               (0 0 0 0 0 2 2)
               (0 3 0 6 3 2 0)
               (4 0 0 0 0 0 0)
               (0 0 0 4 0 4 0)
               (2 0 6 0 2 4 0)
               (0 0 0 0 0 0 0)))

(define big-puzz '((4 0 7 0 0 0 0 0 0 0 0 21 0)
                   (0 3 2 0 0 0 0 0 0 0 0 0 2)
                   (0 0 0 0 0 0 0 2 3 0 0 0 0)
                   (0 0 0 20 0 0 0 0 0 0 0 0 5)
                   (0 2 0 0 0 0 0 4 0 0 0 0 0)
                   (0 0 3 0 0 0 0 0 0 0 0 0 0)
                   (3 0 0 0 0 5 2 4 0 0 0 0 0)
                   (0 0 0 0 0 2 0 6 0 0 0 0 0)
                   (0 0 0 20 0 0 0 0 0 0 0 0 0)
                   (0 0 0 0 0 0 0 0 0 0 0 0 0)
                   (0 0 0 0 0 0 0 0 0 0 0 24 0)
                   (0 0 0 0 4 0 4 0 0 0 4 0 0)
                   (0 0 3 0 0 0 0 0 0 0 8 0 2)))

(define mega-puzzle '((4 0 7 0 0 1 0 0 0 0 0 21 0)
                      (0 3 2 0 0 0 0 0 0 0 0 0 2)
                      (0 0 0 0 0 0 0 2 3 0 0 0 0)
                      (0 0 0 20 0 0 0 0 0 0 0 0 5)
                      (0 2 0 0 0 0 0 4 0 0 1 0 0)
                      (0 0 3 0 0 0 0 0 0 0 0 0 0)
                      (3 0 0 0 0 22 2 4 0 0 0 0 0)
                      (0 0 0 0 0 2 0 6 0 0 0 0 0)
                      (0 0 0 20 0 0 0 0 0 0 0 0 0)
                      (0 0 0 0 0 0 0 0 0 0 0 0 0)
                      (1 0 0 0 0 0 0 1 0 0 0 24 0)
                      (0 0 0 0 4 0 4 0 0 0 4 0 0)
                      (0 0 3 0 0 0 0 0 1 0 8 0 2)))

;; 2(a)

;; (map2d f lon) consumes a function (denoted by f) and a (listof list)
;; (denoted by lon) to produce a new (listof list) where f is applied to
;; every element in lon
;; map2d: Func (listof (listof Any)) -> (listof (listof Any))
;; Examples:
(check-expect (map2d add1 '((3 4 5) (10 9 8))) '((4 5 6) (11 10 9)))
(check-expect (map2d sub1 '((3 4 5) (10 9 8))) '((2 3 4) (9 8 7)))
(check-expect (map2d add1 empty) '())

(define (map2d f lon)
  (map (lambda (x) (map f x)) lon))

;; Tests:
(check-expect (map2d abs '((-3 -4 5) (-10 8 -9))) '((3 4 5) (10 8 9)))
(check-expect (map2d floor '((3.2 4.6 6.1) (10.2 9.3 8))) '((3 4 6) (10 9 8)))

;; 2(b)

;; (construct-puzzle lon) consumes a (listof (listof Nat)) (denoted by lon)
;; to produce a State representing the intital state of the puzzle
;; construct-puzzle: (listof (listof Nat)) -> State
;; Examples:
(check-expect (construct-puzzle '((3 4) (10 9)))
              (make-state (list (list (make-cell 3 false) (make-cell 4 false))
                                (list (make-cell 10 false) (make-cell 9 false)))
                          empty))
(check-expect (construct-puzzle '((3) (5)))
              (make-state (list (list (make-cell 3 false))
                                (list (make-cell 5 false)))
                          empty))

(define (construct-puzzle lon)
  (make-state (map2d (lambda (x) (make-cell x false)) lon) empty))

;; Tests:
(check-expect (construct-puzzle '((4 3) (2 1)))
              (make-state (list (list (make-cell 4 false) (make-cell 3 false))
                                (list (make-cell 2 false) (make-cell 1 false)))
                          empty))
(check-expect (construct-puzzle '((21 29 28) (54 109 36)))
              (make-state (list (list (make-cell 21 false) (make-cell 29 false)
                                      (make-cell 28 false))
                                (list (make-cell 54 false) (make-cell 109 false)
                                      (make-cell 36 false))) empty))

;; 2(c)

;; (solved? state1) consumes a State (denoted by state1) to produce a Bool
;; that indicates whether the puzzle described by the state is fully solved
;; solved?: State -> Bool
;; Examples:
(check-expect (solved? (make-state
                        (list (list (make-cell 3 false) (make-cell 4 false))
                              (list (make-cell 10 false) (make-cell 9 false)))
                        empty)) false)
(check-expect (solved? (make-state
                        (list (list (make-cell 3 true) (make-cell 4 true))
                              (list (make-cell 10 true) (make-cell 9 true)))
                        empty)) true)

(define (solved? state1)
  (local
    ;; (list-of-bool state1) determines if a cell has been used in a State
    ;; and puts the answer as a replacement for each cell
    ;; list-of-bool: State -> (listof (listof Bool))
    [(define (list-of-bool state1)
       (map2d cell-used? (state-grid state1)))]
    (foldr (lambda (x y) (cond
                           [(member? false x) false]
                           [else y])) true (list-of-bool state1))))

;; Tests:
(check-expect (solved? (make-state (list (list (make-cell 3 true))
                                         (list (make-cell 32 false)))
                                   empty)) false)
(check-expect (solved? (make-state (list (list (make-cell 3 true))
                                         (list (make-cell 32 true)))
                                   empty)) true)

;; 2(d)

;; (get-first-unused grid1) consumes a Grid (denoted by grid1) and
;; finds the topmost, leftmost cell in the grid that isn’t marked as used
;; get-first-unused: Grid -> (list Nat Nat)
;; Examples:
(check-expect (get-first-unused (list (list (make-cell 3 true))
                                      (list (make-cell 32 false)))) (list 1 0))
(check-expect (get-first-unused (list (list (make-cell 3 false)
                                            (make-cell 4 false))
                                      (list (make-cell 10 false)
                                            (make-cell 9 false)))) (list 0 0))

(define (get-first-unused grid1)
  (local
    ;; (list-of-bool grid2) determines if a cell has been used in a Grid
    ;; and puts the answer as a replacement for each cell
    ;; list-of-bool: Grid -> (listof (listof Bool))
    [(define (list-of-bool grid2)
       (map2d cell-used? grid2))
     ;; (find-row grid3 x y) determines the row in which an unused cell is
     ;; present, starting from the top while updating the row value
     ;; (denoted by x)
     ;; find-row: (listof (listof Bool)) Nat Nat -> (listof Bool) Nat Nat
     (define (find-row grid3 x y)
       (cond
         [(not (foldr (lambda (x y)
                        (cond
                          [(not x) false]
                          [else y]))
                      true (first grid3)))
          (find-col (first grid3) x 0)]
         [else (find-row (rest grid3) (add1 x) 0)]))
     ;; (find-col row x y) determines the column in which an unused cell is
     ;; present, starting from the left while updating the col valye
     ;; (denoted by y) and produces the coordinates of the unused cell
     ;; find-col: (listof Bool) Nat Nat -> (listof Nat Nat)
     (define (find-col row x y)
       (cond
         [(equal? false (first row)) (list x y)]
         [else (find-col (rest row) x (add1 y))]))]
    (find-row (list-of-bool grid1) 0 0)))

;; Tests:
(check-expect (get-first-unused (list (list (make-cell 21 true)
                                            (make-cell 29 true)
                                            (make-cell 28 false))
                                      (list (make-cell 54 false)
                                            (make-cell 109 true)
                                            (make-cell 36 false)))) (list 0 2))
(check-expect (get-first-unused (list (list (make-cell 28 true)
                                            (make-cell 29 true))
                                      (list (make-cell 42 true)
                                            (make-cell 31 false)))) (list 1 1))

;; 2(e)

;; (neighbours state1) consumes a State (denoted by state1) and produces a list
;; of new States that might legitimately follow from the given state after
;; adding a single new rectangle. Otherwise, it produces an empty list.
;; neighbours: State -> (listof State)
;; Examples:
(check-expect (neighbours (make-state
                           (list (list (make-cell 3 false) (make-cell 4 false))
                                 (list (make-cell 10 false) (make-cell 9 false)
                                       )) empty)) '())
(check-expect (neighbours (make-state
                           (list (list (make-cell 2 false) (make-cell 0 false))
                                 (list (make-cell 0 false) (make-cell 0 false))
                                 ) empty))
              (list
               (make-state
                (list
                 (list (make-cell 2 true) (make-cell 0 false))
                 (list (make-cell 0 true) (make-cell 0 false)))
                (list (make-rect 0 0 1 2)))
               (make-state
                (list
                 (list (make-cell 2 true) (make-cell 0 true))
                 (list (make-cell 0 false) (make-cell 0 false)))
                (list (make-rect 0 0 2 1)))))
                           
(define (neighbours state1) '())

;; Tests:
(check-expect (neighbours (make-state
                           (list (list (make-cell 5 true) (make-cell 6 false)
                                       (make-cell 1 true))
                                 (list (make-cell 42 false) (make-cell 1 false)
                                       (make-cell 56 true))) empty)) '())
(check-expect (neighbours (make-state
                           (list (list (make-cell 0 true) (make-cell 5 false)
                                       (make-cell 0 false))
                                 (list (make-cell 0 false) (make-cell 3 false)
                                       (make-cell 0 false))) empty)) empty)
(check-expect (neighbours (make-state
                           (list (list (make-cell 3 false) (make-cell 0 false)
                                       (make-cell 0 false) (make-cell 0 false))
                                 (list (make-cell 0 false) (make-cell 0 false)
                                       (make-cell 0 false) (make-cell 0 false))
                                 (list (make-cell 0 false) (make-cell 0 false)
                                       (make-cell 0 false) (make-cell 0 false)))
                           empty))
              (list
               (make-state
                (list
                 (list (make-cell 3 true) (make-cell 0 false)
                       (make-cell 0 false) (make-cell 0 false))
                 (list (make-cell 0 true) (make-cell 0 false)
                       (make-cell 0 false) (make-cell 0 false))
                 (list (make-cell 0 true) (make-cell 0 false)
                       (make-cell 0 false) (make-cell 0 false)))
                (list (make-rect 0 0 1 3)))
               (make-state
                (list
                 (list (make-cell 3 true) (make-cell 0 true)
                       (make-cell 0 true) (make-cell 0 false))
                 (list (make-cell 0 false) (make-cell 0 false)
                       (make-cell 0 false) (make-cell 0 false))
                 (list (make-cell 0 false) (make-cell 0 false)
                       (make-cell 0 false) (make-cell 0 false)))
                (list (make-rect 0 0 3 1)))))

;; 2(f)

;; (solve-rectangle-puzzle lon) consumes a (listof (listof Nat)) (denoted by
;; lon) and attempts to solve the puzzle, producing either the list of
;; rectangles if a solution exists, or false if there is no solution
;; solve-rectangle-puzzle: (listof (listof Nat)) -> (anyof (listof Rect) false)
;; Examples:
(check-expect (solve-rectangle-puzzle puzz) (list
                                             (make-rect 5 5 2 2)
                                             (make-rect 4 5 1 2)
                                             (make-rect 1 5 3 2)
                                             (make-rect 0 5 1 2)
                                             (make-rect 0 4 4 1)
                                             (make-rect 5 3 2 2)
                                             (make-rect 5 2 2 1)
                                             (make-rect 4 2 1 3)
                                             (make-rect 4 1 2 1)
                                             (make-rect 2 1 2 3)
                                             (make-rect 1 1 1 3)
                                             (make-rect 6 0 1 2)
                                             (make-rect 1 0 5 1)
                                             (make-rect 0 0 1 4)))
(check-expect (solve-rectangle-puzzle (list (list 0))) false)

(define (solve-rectangle-puzzle lon) false)

;; Tests:
(check-expect (solve-rectangle-puzzle big-puzz) (list
                                                 (make-rect 11 12 2 1)
                                                 (make-rect 3 12 8 1)
                                                 (make-rect 0 12 3 1)
                                                 (make-rect 9 11 4 1)
                                                 (make-rect 5 11 4 1)
                                                 (make-rect 5 8 8 3)
                                                 (make-rect 4 8 1 4)
                                                 (make-rect 7 7 6 1)
                                                 (make-rect 4 7 2 1)
                                                 (make-rect 0 7 4 5)
                                                 (make-rect 6 6 1 2)
                                                 (make-rect 1 6 5 1)
                                                 (make-rect 7 5 2 2)
                                                 (make-rect 1 4 1 2)
                                                 (make-rect 0 4 1 3)
                                                 (make-rect 7 3 2 2)
                                                 (make-rect 2 3 1 3)
                                                 (make-rect 12 2 1 5)
                                                 (make-rect 7 1 1 2)
                                                 (make-rect 3 1 4 5)
                                                 (make-rect 2 1 1 2)
                                                 (make-rect 1 1 1 3)
                                                 (make-rect 12 0 1 2)
                                                 (make-rect 9 0 3 7)
                                                 (make-rect 8 0 1 3)
                                                 (make-rect 1 0 7 1)
                                                 (make-rect 0 0 1 4)))
(check-expect (solve-rectangle-puzzle mega-puzzle) false)
(check-expect (solve-rectangle-puzzle '((3 0 0 0 0 0 0)
                                        (0 0 0 2 2 2 0)
                                        (0 2 2 0 0 6 0)
                                        (6 0 2 0 0 0 0)
                                        (0 8 0 0 0 2 6)
                                        (0 0 0 0 0 0 0)
                                        (0 6 0 0 0 0 0)))
              (list
               (make-rect 1 6 6 1)
               (make-rect 5 4 1 2)
               (make-rect 1 4 4 2)
               (make-rect 1 3 2 1)
               (make-rect 3 2 3 2)
               (make-rect 2 1 1 2)
               (make-rect 1 1 1 2)
               (make-rect 0 1 1 6)
               (make-rect 6 0 1 6)
               (make-rect 5 0 1 2)
               (make-rect 4 0 1 2)
               (make-rect 3 0 1 2)
               (make-rect 0 0 3 1)))
              


                                      
           
      
       
                                          
 
                                 
    

    





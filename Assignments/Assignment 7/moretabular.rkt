;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname moretabular) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;
;; ***************************************************
;;   Pronoy Chaudhuri (20709909)
;;   CS 135 Fall 2017
;;   Assignment 07, Problem 3
;; ***************************************************
;;

;; A Table is a (listof (listof Num))
;; requires: each sub-list has the same length

;; 3(a)

;; (mirror table) consumes a table (denoted by "table") and
;; reverses the elements in each row
;; mirror: Table -> Table
;; Examples:
(check-expect (mirror '((-3.2 4.5 7) (13 3 -3)))
              (list (list 7 4.5 -3.2) (list -3 3 13)))
(check-expect (mirror empty) empty)

(define (mirror table)
  (cond
    [(empty? table) empty]
    [else
     ;; (mirror/acc lon acc) uses a list (denoted by "lon") and accumulates
     ;; using acc to reverse a list
     ;; mirror/acc: (listof Any) (listof Any) -> (listof Any)
     (local [(define (mirror/acc lon acc)
               (cond
                 [(empty? lon) acc]
                 [else (mirror/acc (rest lon)
                                   (cons (first lon) acc))]))]
       (cons (mirror/acc (first table) empty)
             (mirror (rest table))))]))

;; Tests:
(check-expect (mirror (list (list 1 2 -5) (list 2 -2 3)))
              (list (list -5 2 1) (list 3 -2 2)))
(check-expect (mirror (list (list 39 10 1000) (list 3 -2 69)))
              (list (list 1000 10 39) (list 69 -2 3)))

;; 3(b)

;; A (listof Func) is a (listof (anyof (Num -> Num) (Num -> Int) (Num -> Nat)))
;; A Func is (anyof (Num -> Num) (Num -> Int) (Num -> Nat))

;; (element-apply-many flon table) takes a list of functions (denoted by "flon")
;; and creates a list of tables with transformations applied to the table
;; (denoted by "table") by respective functions in flon
;; element-apply-many: (listof Func) Table -> (listof Table)
;; Examples:
(check-expect (element-apply-many (list abs floor)
                                  '(( 7 4.5 -3.2)(-3 3 13)))
              (list (list (list 7 4.5 3.2)
                          (list 3 3 13))
                    (list (list 7 4 -4)
                          (list -3 3 13))))
(check-expect (element-apply-many empty empty) empty)
(check-expect (element-apply-many empty (list (list 1 2 3))) empty)
(check-expect (element-apply-many (list abs floor) empty) (list empty empty))

(define (element-apply-many flon table)
  (cond
    [(empty? flon) empty]
    [else
     ;; (apply flon lon) consumes a function (denoted by "flon") and
     ;; applies the function to all elements in a list (denoted by "lon")
     ;; apply: Func (listof Num) -> (listof Num)
     (local [(define (apply flon lon)
               (cond
                 [(empty? lon) empty]
                 [else (cons (flon (first lon))
                             (apply flon (rest lon)))]))]
       ;; (make-table flon table) consumes a function (denoted by "flon")
       ;; and applies the function to all elements in a table (denoted by
       ;; "table")
       ;; make-table: Func Table -> Table
       (local [(define (make-table flon table)
                 (cond
                   [(empty? table) empty]
                   [else
                    (append (list (apply flon (first table)))
                            (make-table flon (rest table)))]))]
         (cons (make-table (first flon) table)
               (element-apply-many (rest flon) table))))]))
     
;; Tests:
(check-expect (element-apply-many
               (list add1 sub1)
               (list (list 8 3 4 9) (list 3 7 5 6) (list -1 1 -3 0)))
              (list (list (list 9 4 5 10) (list 4 8 6 7) (list 0 2 -2 1))
                    (list (list 7 2 3 8) (list 2 6 4 5) (list -2 0 -4 -1))))
(check-expect (element-apply-many
               (list -)
               (list (list 8 3 4 9) (list 3 7 5 6) (list -1 1 -3 0)))
              (list (list (list -8 -3 -4 -9) (list -3 -7 -5 -6) (list 1 -1 3 0))
                    ))

;; 3(c)

;; (apply-function f arg) produces the result of f with the given argument arg.
;; apply-function: (X → Y) X → Y

(define (apply-function f arg)
  (f arg))

;; (scale-smallest table num1) consumes a table (denoted by "table") and
;; a real number (denoted by "num1") and produces a second function that
;; multiplies a number by the smallest element of the table, and adds the offset
;; scale-smallest: Table Num -> Num
;; requires: table must be non-empty (at least one column and row)
;;           num1 must be a real number
;; Examples:
(check-expect (apply-function (scale-smallest '((7 4.5 3.2)(-3 3 13)) 2.4) 7)
              -18.6)
(check-expect (apply-function (scale-smallest '((1 2) (-1 -2)) 2.9) 3) -3.1)

(define (scale-smallest table num1)
  ;; (min-row min-so-far lon) determines the smallest value in a row (denoted
  ;; by "lon"), accumulating with min-so-far
  ;; min-row: Num (listof Num) -> Num
  (local
    [(define (min-row min-so-far lon)
       (cond
         [(empty? lon) min-so-far]
         [(<= (first lon) min-so-far) (min-row (first lon) (rest lon))]
         [else (min-row min-so-far (rest lon))]))]
    ;; (min-table min-so-far table) determines the smallest value in a table
    ;; (denoted by "table"), accumulating with min-so-far
    ;; min-table: Num Table -> Num
    (local
      [(define (min-table min-so-far table)
         (cond
           [(empty? table) min-so-far]
           [else (min-table (min-row min-so-far (first table)) (rest table))]))]
      ;; (function arg) multiplies the minimum value of a table by a chosen num
      ;; (denoted by "arg") and adds the offset value (denoted by "num1" from
      ;; scale-smallest)
      ;; f: Num -> Num
      (local
        [(define (function arg)
           (+ (* (min-table (first (first table)) table) arg) num1))] function))
    ))

;; Tests:
(check-expect (apply-function (scale-smallest '((35 9001) (1 1)) 5) 3) 8)
(check-expect (apply-function (scale-smallest '((5 2) (2 5)) 2) 2) 6)



                                  


              
                                  
                     

;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname tabular) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;
;; ***************************************************
;;   Pronoy Chaudhuri (20709909)
;;   CS 135 Fall 2017
;;   Assignment 06, Problem 1
;; ***************************************************
;;

;; A Table is a (listof (listof Num))
;; requires: each sub-list has the same length

;; 1(a)

;; (mult-by-row num row) multiples all numbers in a row by a given num
;; mult-by-row: Num (listof Num) -> (listof Num)
;; Examples:
(check-expect (mult-by-row 5 (list 3 7 5 6)) (list 15 35 25 30))
(check-expect (mult-by-row 5 empty) empty)

(define (mult-by-row num row)
  (cond
    [(empty? row) empty]
    [else
     (cons (* num (first row)) (mult-by-row num (rest row)))])) 

;; (mult-by num table) multiplies all numbers in a table
;; by a given num
;; mult-by: Num Table -> Table
;; Examples:
(check-expect (mult-by 0 (list (list 8 3 4 9) (list 3 7 5 6) (list -1 1 -3 0)))
              (list (list 0 0 0 0) (list 0 0 0 0) (list 0 0 0 0)))
(check-expect (mult-by 1 (list (list 8 3 4 9) (list 3 7 5 6) (list -1 1 -3 0)))
              (list (list 8 3 4 9) (list 3 7 5 6) (list -1 1 -3 0)))
(check-expect (mult-by 156 empty) empty)

(define (mult-by num table)
  (cond
    [(empty? table) empty]
    [else
     (cons (mult-by-row num (first table))
           (mult-by num (rest table)))]))

;; Tests:
(check-expect (mult-by 3 (list (list 1 2 3) (list 5 6 4) (list -3 5 -2)))
              (list (list 3 6 9) (list 15 18 12) (list -9 15 -6)))
(check-expect (mult-by -1 (list (list 1 2 3) (list 5 6 4) (list -3 5 -2)))
              (list (list -1 -2 -3) (list -5 -6 -4) (list 3 -5 2)))
(check-expect (mult-by -45 (list (list 10) (list -3) (list 25)))
              (list (list -450) (list 135) (list -1125)))

;; 1(b)

;; (get-elem-from-column column row) finds a value at a particular column
;; when given a row
;; get-elem-from-column: Nat (listof Num) -> Num
;; Examples:
(check-expect (get-elem-from-column 1 (list 2 3 4 5)) 3)
(check-expect (get-elem-from-column 0 (list 2 3)) 2)
(check-expect (get-elem-from-column 1 empty) empty)
(check-expect (get-elem-from-column 56 (list 2 3 4 5)) empty)

(define (get-elem-from-column column row)
  (cond
    [(empty? row) empty]
    [(zero? column) (first row)]
    [else (get-elem-from-column (- column 1) (rest row))]))

;; (get-elem row column table) finds a value at a particular row and column
;; within a table and produces it. If the row and column do not exist, result
;; will be false
;; get-elem: Nat Nat Table -> Num
;; Examples:
(check-expect (get-elem 1 1 (list (list 8 3 4) (list 3 7 5) (list -1 1 -3))) 7)
(check-expect (get-elem 4 5 (list (list 8 3) (list 3 7) (list -1 1))) false)
(check-expect (get-elem 5 4 empty) false)

(define (get-elem row column table)
  (cond
    [(empty? table) false]
    [(zero? row)
     (cond
       [(empty? (get-elem-from-column column (first table))) false]
       [else (get-elem-from-column column (first table))])]
    [else (get-elem (- row 1) column (rest table))]))

;; Tests:
(check-expect (get-elem 2 3
                        (list (list 1 2 3 4) (list 4 5 6 7) (list 8 9 8 11)))
              11)
(check-expect (get-elem 1 4
                        (list (list 1 2 3 4) (list 4 5 6 7) (list 8 9 8 11)))
              false)
(check-expect (get-elem 0 3
                        (list (list 1 2 3 4) (list 4 5 6 7) (list 8 9 8 11)))
              4)

;; 1(c)

;; (col column table) produces all values of a table that fall under a given
;; column
;; col: Nat Table -> (listof Num)
;; Examples:
(check-expect (col 2 (list (list 8 3 4 9) (list 3 7 5 6) (list -1 1 -3 0)))
              (list 4 5 -3))
(check-expect (col 3 (list (list 0) (list 1) (list 2) (list 5))) empty)
(check-expect (col 2 empty) empty)

(define (col column table)
  (cond
    [(empty? table) empty]
    [else
     (cond
       [(empty? (get-elem-from-column column (first table))) empty]
       [else (cons (get-elem-from-column column (first table))
                   (col column (rest table)))])]))

;; Tests:
(check-expect (col 3 (list (list 1 2 3 4) (list 4 5 6 7) (list -8 9 8 11)))
              (list 4 7 11))
(check-expect (col 5 (list (list 1 2 3 4) (list 4 5 6 7) (list -8 9 8 11)))
              empty)
(check-expect (col 0 (list (list 1 2 3 4) (list 4 5 6 7) (list -8 9 8 11)))
              (list 1 4 -8))

;; 1(d)

;; (sum-by-row row1 row2) adds all numbers between two rows pairwise
;; sum-by-row: (listof Num) (listof Num) -> (listof Num)
;; Examples:
(check-expect (sum-by-row (list 1 2 3 4) (list 3 7 5 6)) (list 4 9 8 10))
(check-expect (sum-by-row empty empty) empty)

(define (sum-by-row row1 row2)
  (cond
    [(empty? row1) empty]
    [else
     (cons (+ (first row1) (first row2)) (sum-by-row (rest row1) (rest row2)))]
    )) 

;; (sum-tables table1 table2) consumes two tables of same dimension and
;; produces a table that is the result of adding up the elements pairwise
;; between the two tables
;; sum-tables: Table Table -> Table
;; Examples:
(check-expect (sum-tables (list (list 1 2 3) (list 2 1 3))
                          (list (list 5 6 7) (list 20 9 -8)))
              (list (list 6 8 10) (list 22 10 -5)))
(check-expect (sum-tables empty empty) empty)

(define (sum-tables table1 table2)
  (cond
    [(empty? table1) empty]
    [else
     (cons (sum-by-row (first table1) (first table2))
           (sum-tables (rest table1) (rest table2)))]))

;; Tests:
(check-expect (sum-tables (list (list 1 2 3) (list 5 6 4) (list -3 5 -2))
                          (list (list 1 2 3) (list 5 6 4) (list -3 5 -2)))
              (list (list 2 4 6) (list 10 12 8) (list -6 10 -4)))
(check-expect (sum-tables (list (list 1 2 3) (list 5 6 4) (list -3 5 -2))
                          (list (list 5 6 -4) (list 10 30 25) (list 5 6 2)))
              (list (list 6 8 -1) (list 15 36 29) (list 2 11 0)))
                          
                   
                   
                          
    
                          





    
              
    
    
                        


                       




              

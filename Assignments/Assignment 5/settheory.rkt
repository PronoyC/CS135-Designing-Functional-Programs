;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname settheory) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;
;; ***************************************************
;;   Pronoy Chaudhuri (20709909)
;;   CS 135 Fall 2017
;;   Assignment 05, Problem 1
;; ***************************************************
;;

;; A NumSet is a (listof Num)
;; requires: the numbers are strictly increasing

;; 1(a)

;; (union numset1 numset2) consumes two NumSets to make
;; one NumbSet containing values from either NumSets
;; union: NumSet NumSet -> NumSet
;; Examples:
(check-expect (union (list 1 2 3 4) empty) (list 1 2 3 4))
(check-expect (union empty (list 1 2 3 4)) (list 1 2 3 4))
(check-expect (union (list 1 2 3 4) (list 5 6 7 8)) (list 1 2 3 4 5 6 7 8))
              
(define (union numset1 numset2)
  (cond
    [(and (empty? numset1) (cons? numset2)) numset2]
    [(and (cons? numset1) (empty? numset2)) numset1]
    [(and (cons? numset1) (cons? numset2))
     (cond
       [(member? (first numset1) numset2)
        (union (rest numset1) numset2)]
       [(< (first numset1) (first numset2))
        (cons (first numset1) (union (rest numset1) numset2))]
       [else (cons (first numset2) (union numset1 (rest numset2)))])]))

;; Tests:
(check-expect (union (list 5 6 7 8) (list 1 2 3 4)) (list 1 2 3 4 5 6 7 8))
(check-expect (union (list 1 3 5 7) (list 2 4 6 8)) (list 1 2 3 4 5 6 7 8))
(check-expect (union (list 1 3 5 7) (list 1 2 4 6 8)) (list 1 2 3 4 5 6 7 8))
(check-expect (union (list 5 6 7 8 10) (list 1 2 3)) (list 1 2 3 5 6 7 8 10))
(check-expect (union (list 1 3 5 7 10) (list 2 6 8)) (list 1 2 3 5 6 7 8 10))

;; 1(b)

;; (intersection numset1 numset2) consumes two NumSets to make
;; one NumbSet containing values from both NumSets
;; intersection: NumSet NumSet -> NumSet
;; Examples:
(check-expect (intersection (list 1 2 3 4) empty) empty)
(check-expect (intersection empty (list 1 2 3 4)) empty)
(check-expect (intersection (list 1 2 3 4) (list 2 6 7)) (list 2))

(define (intersection numset1 numset2)
  (cond
    [(and (empty? numset1) (cons? numset2)) empty]
    [(and (cons? numset1) (empty? numset2)) empty]
    [(and (cons? numset1) (cons? numset2))
     (cond [(member? (first numset1) numset2)
            (cons (first numset1) (intersection (rest numset1) numset2))]
           [else (intersection (rest numset1) numset2)])]))

;; Tests:
(check-expect (intersection (list 1 5 6 7) (list 1 2 6)) (list 1 6))
(check-expect (intersection (list 6 7) (list 1 2 6)) (list 6))
(check-expect (intersection (list 2 6 7) (list 1 2 6)) (list 2 6))
(check-expect (intersection (list 1 2 3 4) (list 3 4 7)) (list 3 4))
(check-expect (intersection (list 1 2 3) (list 3 4 10)) (list 3))

;; 1(c)

;; (difference numset1 numset2) consumes two NumSets to make
;; one NumbSet containing values from numset1 that are not in numset2
;; difference: NumSet NumSet -> NumSet
;; Examples:
(check-expect (difference (list 1 2 3 4) empty) (list 1 2 3 4))
(check-expect (difference empty (list 1 2 3 4)) empty)
(check-expect (difference (list 1 2 3 4) (list 2 6 7)) (list 1 3 4))

(define (difference numset1 numset2)
  (cond
    [(and (empty? numset1) (cons? numset2)) empty]
    [(and (cons? numset1) (empty? numset2)) numset1]
    [(and (cons? numset1) (cons? numset2))
     (cond [(not (member? (first numset1) numset2))
            (cons (first numset1) (difference (rest numset1) numset2))]
           [else (difference (rest numset1) numset2)])]))

;; Tests:
(check-expect (difference (list 5 6 7) (list 1 2 6)) (list 5 7))
(check-expect (difference (list 6 7) (list 1 2 6)) (list 7))
(check-expect (difference (list 2 6 7) (list 1 2 6)) (list 7))
(check-expect (difference (list 1 2 3 4) (list 3 4 7)) (list 1 2))
(check-expect (difference (list 1 2 3) (list 3 4 10)) (list 1 2))

;; 1(d)

;; (symmetric-difference numset1 numset2) consumes two NumSets to make
;; one NumbSet that has values that both NumSets do not share
;; symmetric-difference: NumSet NumSet -> NumSet
;; Examples:
(check-expect (symmetric-difference (list 1 2 3 4) empty) (list 1 2 3 4))
(check-expect (symmetric-difference empty (list 1 2 3 4)) (list 1 2 3 4))
(check-expect (symmetric-difference (list 1 2 3) (list 2 6 7)) (list 1 3 6 7))

(define (symmetric-difference numset1 numset2)
  (difference (union numset1 numset2) (intersection numset1 numset2)))

;; Tests:
(check-expect (symmetric-difference (list 5 6 7) (list 1 2 6)) (list 1 2 5 7))
(check-expect (symmetric-difference (list 6 7) (list 1 2 6)) (list 1 2 7))
(check-expect (symmetric-difference (list 2 6 7) (list 1 2 6)) (list 1 7))
(check-expect (symmetric-difference (list 1 2 3 4) (list 3 4 7)) (list 1 2 7))
(check-expect (symmetric-difference (list 1 2 3) (list 3 4 10)) (list 1 2 4 10))
          


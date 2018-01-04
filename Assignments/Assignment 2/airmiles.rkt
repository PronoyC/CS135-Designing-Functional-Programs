;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname airmiles) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;; ***************************************************
;;   Pronoy Chaudhuri (20709909)
;;   CS 135 Fall 2017
;;   Assignment 02, Problem 2
;; ***************************************************
;;

;; 2

;; (calc-airmiles dollar card store) displays air miles earned
;; dependent on the three components [dollar card store]
;; calc-airmiles: Num Sym Sym -> Nat
;; requires: dollar >= 0
;;           card has to be either 'standard or 'premium
;; Examples:
(check-expect (calc-airmiles 15 'standard 'sponsor) 1)
(check-expect (calc-airmiles 20 'standard 'non-sponsor) 1)
(check-expect (calc-airmiles 10 'premium 'sponsor) 1)
(check-expect (calc-airmiles 15 'premium 'non-sponsor) 1)

(define (calc-airmiles dollar card store)
  (cond
    [(and (eq? card 'standard) (eq? store 'sponsor))
     (floor (/ dollar 15))]
    [(and (eq? card 'premium) (eq? store 'sponsor))
     (floor (/ dollar 10))]
    [(eq? card 'standard) (floor (/ dollar 20))]
    [(eq? card 'premium) (floor (/ dollar 15))]
    [else 0]))

;; Tests:
(check-expect (calc-airmiles 44 'standard 'sponsor) 2)
(check-expect (calc-airmiles 39 'standard 'non-sponsor) 1)
(check-expect (calc-airmiles 14 'premium 'sponsor) 1)
(check-expect (calc-airmiles 29 'premium 'non-sponsor) 1)
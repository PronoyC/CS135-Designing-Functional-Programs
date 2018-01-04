;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname bonus) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;; ***************************************************
;;   Pronoy Chaudhuri (20709909)
;;   CS 135 Fall 2017
;;   Assignment 04, Problem 5
;; ***************************************************
;;

(require "a04lib.rkt")

;; 5

;; (sum-of-digits nat) consumes a nat to determine
;; the sum of its digits
;; sum-of-digits: Nat -> Bool
;; Examples:
(check-expect (sum-of-digits 3014) 8)
(check-expect (sum-of-digits 1234) 10)
(check-expect (sum-of-digits 6) 6)

(define (sum-of-digits nat)
  (cond
    [(<= (other-digits nat) 9)
     (+ (other-digits nat) (last-digit nat))]
    [else (sum-of-digits
           (+ (last-digit nat)
              (other-digits nat)))]))
  
;; (div-by-3-alt? nat) consumes a nat to determine
;; if it is divisible by 3 using the sum of its digits
;; div-by-3-alt?: Nat -> Bool
;; Examples:
(check-expect (div-by-3-alt? 0) true)
(check-expect (div-by-3-alt? 1) false)
(check-expect (div-by-3-alt? 2) false)


(define (div-by-3-alt? nat)
  (cond
    [(= (modulo (sum-of-digits nat) 3) 0) true]
    [else false])) 

(check-expect (div-by-3-alt? 3) true)
(check-expect (div-by-3-alt? 10) false)
(check-expect (div-by-3-alt? 23) false)
(check-expect (div-by-3-alt? 15485863) false)

;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ca) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;
;; ***************************************************
;;   Pronoy Chaudhuri (20709909)
;;   CS 135 Fall 2017
;;   Assignment 09, Problem 1
;; ***************************************************
;;

;; Constants:
(define base 2)
(define white 0)
(define black 1)

;; 1(a)

;; Constants:
(define first-bit 1)
(define second-bit 2)
(define third-bit 4)

;; (apply-rule a b c r) consumes four Nats (denoted by
;; a b c and r), to apply a given CA rule and produce
;; 0 or 1 depending on the colour of the resulting square
;; apply-rule: Nat Nat Nat Nat -> Nat
;; requires: output must produce either 0 or 1
;;           r must be a Nat value between [0, 255]
;; Examples:
(check-expect (apply-rule 0 0 0 86) 0)
(check-expect (apply-rule 1 1 0 86) 1)

(define (apply-rule a b c r)
  (local
    ;; (bit-search a b c) coverts binary values a b and c
    ;; into a singular Nat form
    ;; bit-search: Nat Nat Nat -> Nat
    ;; requires: a b c must be either 0 or 1
    [(define (bit-search a b c)
       (+ (* first-bit c) (* second-bit b) (* third-bit a)))]
    (cond
      [(odd? (floor (/ r (expt base (bit-search a b c))))) black]
      [else white])))

;; Tests:
(check-expect (apply-rule 1 1 1 86) 0)
(check-expect (apply-rule 1 0 0 86) 1)

;; 1(b)

;; (next-row lon r) consumes a row in CA (denoted by lon) and a rule
;; (denoted by r) to produce a new list with same length of lon, with result of
;; applying rule to all elements in lon
;; next-row: (listof (anyof 0 1)) Nat -> (listof (anyof 0 1))
;; requires: (listof (anyof 0 1) must have minimum length of 1
;;           r must be a Nat value between [0, 255]
;; Examples:
(check-expect (next-row (list 0) 86) (list 0))
(check-expect (next-row (list 0 0) 86) (list 0 0))
(check-expect (next-row (list 1 1 1) 86) (list 0 0 1))

(define (next-row lon r)
  (local
    ;; (middle lon r) applies a rule (denoted by r) onto the middle values of a
    ;; list (denoted by lon)
    ;; middle: (listof (anyof 0 1))  Nat -> (listof (anyof 0 1))
    [(define (middle lon r)
       (cond
         [(empty? (rest (rest lon))) (cons (apply-rule (first lon)
                                                       (second lon) white r)
                                           empty)]
         [else (cons (apply-rule (first lon) (second lon) (third lon) r)
                     (middle (rest lon) r))]))]
    (cond
      [(= black (length lon)) (list (apply-rule white (first lon) white r))]
      [else
       (append (list (apply-rule white (first lon) (second lon) r))
               (middle lon r))])))

;; Tests:
(check-expect (next-row (list 1 0 1 0 1) 86) (list 1 0 1 0 1))
(check-expect (next-row (list 1 1 0 1) 86) (list 0 1 0 1))
(check-expect (next-row (list 1) 86) (list 1))

;; 1(c)

;; (iterate f b n) consumes a function f, a base value b and a Nat n to produce
;; a list of length n containing a relationship between these values
;; iterate: Func Num Nat -> (listof Num)
;; Examples:
(check-expect (iterate abs 2 3) '(2 2 2))
(check-expect (iterate sqr 2 4) '(2 4 16 256))
(check-expect (iterate sqr 2 0) '())

(define (iterate f b n)
  (cond
    [(zero? n) empty]
    [else (cons b (iterate f (f b) (sub1 n)))]))

;; Tests:
(check-expect (iterate add1 5 2) '(5 6))
(check-expect (iterate sub1 3 12) '(3 2 1 0 -1 -2 -3 -4 -5 -6 -7 -8))

;; 1(d)

;; (run-automaton lon r n) takes a row of 0s and 1s (denoted by lon), a rule
;; (denoted by r) and number of generations (denoted by n) to produce a list
;; of n lists where the first list is lon and the other rows having r applied
;; run-automaton: (listof (anyof 0 1)) Nat Nat -> (listof (listof (anyof 0 1)))
;; requires: lon must have a minimum length of 1
;;           r must be a Nat value between [0, 255]
;; Examples:
(check-expect (run-automaton (list 0 0 1 0) 86 0) empty)
(check-expect (run-automaton (list 0) 86 3) (list (list 0) (list 0) (list 0)))
              
(define (run-automaton lon r n)
  (iterate (lambda (x) (next-row x r)) lon n))

;; Tests:
(check-expect (run-automaton (list 1 1 1) 86 3) (list (list 1 1 1) (list 0 0 1)
                                                       (list 0 1 1)))
(check-expect (run-automaton (list 0 0 1 0) 86 2) (list (list 0 0 1 0)
                                                         (list 0 1 1 1)))

              
  
    





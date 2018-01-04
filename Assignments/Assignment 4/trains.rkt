;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname trains) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;; ***************************************************
;;   Pronoy Chaudhuri (20709909)
;;   CS 135 Fall 2017
;;   Assignment 04, Problem 4
;; ***************************************************
;;

(require "a04lib.rkt")

(define test-train2  (cons (make-unit 'B 2)
                          (cons (make-unit 'T 6)
                                (cons (make-unit 'C 5)
                                (cons (make-unit 'C 7) empty)))))

;; The unit structure is defined in a04lib.rkt.  The require
;; statement, above, is all that's needed to have it take
;; effect here. The following comment is here just so the
;; type definitions that follow make sense.

;; (define-struct unit (type serial))

;; -------- Q4a --------------
;; A Unit-Type is ... one of 'L, 'B, 'T, 'P, or 'C

;; A Unit is ... one of 'L, 'B, 'T, 'P or 'C

;; A Train is ... 0 <= # of coupled units


;; -------- Q4b --------------

;; string->train works by...
;; taking a string of unit types and converts them into a list of chars
;; using string->list and converts char values into (make-unit Sym serial)
;; so they are now symbols. Then, the serial value is converted into a number
;; from a list (default-serial-numbers) and added to (make-unit Sym serial).

;; 4(c)

;; (headed-by? train unit-type) checks if first unit of train
;; is the same as user-chosen unit-type
;; headed-by?: Train Unit-Type -> Bool
;; Examples:
(check-expect (headed-by? test-train 'L) true)
(check-expect (headed-by? test-train 'P) false)

(define (headed-by? train unit-type1)
  (cond
    [(eq? (unit-type (first train)) unit-type1) true]
    [else false]))

;; Tests:
(check-expect (headed-by? test-train2 'B) true)
(check-expect (headed-by? test-train2 'P) false)

;; 4(d)

;; (ends-with-caboose? train) checks if train has a single caboose and it is
;; at the end of the train
;; ends-with-caboose?: Train -> Bool

;; Examples:

(check-expect (ends-with-caboose? test-train) true)
(check-expect (ends-with-caboose? (cons (make-unit 'C 5) empty)) true)
(check-expect (ends-with-caboose? empty) false)

(define (ends-with-caboose? train)
  (cond
    [(empty? train) false]
    [(and (symbol=? (unit-type (first train)) 'C)
          (empty? (rest train))) true]
    [(symbol=? (unit-type (first train)) 'C) false]
    [else (ends-with-caboose? (rest train))])) 

          
;; Tests:
(check-expect (ends-with-caboose? test-train2) false)
(check-expect (ends-with-caboose? (cons (make-unit 'B 6)
                                        (cons (make-unit 'C 7) empty))) true)

;; 4(e)

;; (remove-unit train serial1) removes units from train with a particular
;; serial number (serial1)
;; remove-unit: Train Nat -> Train
;; Examples:
(check-expect (remove-unit test-train 3)
              (cons (make-unit 'L 2)
                                (cons (make-unit 'C 5) empty)))
(check-expect (remove-unit test-train 7) test-train)
(check-expect (remove-unit empty 5) empty)

(define (remove-unit train serial1)
  (cond
    [(empty? train) train]
    [(= (unit-serial (first train)) serial1)
     (remove-unit (rest train) serial1)] 
    [else (cons (first train) (remove-unit (rest train) serial1))]))

;; Tests:
(check-expect (remove-unit test-train2 7) (cons (make-unit 'B 2)
                          (cons (make-unit 'T 6)
                                (cons (make-unit 'C 5) empty))))
(check-expect (remove-unit test-train2 1) test-train2)

;; 4(f)

;; (removes-all-head-L train) removes all 'L that a train
;; starts with
;; removes-all-head-L: Train -> Train
;; Examples:
(check-expect (removes-all-head-L test-train) (cons (make-unit 'B 3)
                                (cons (make-unit 'C 5) empty)))
(check-expect (removes-all-head-L empty) empty)

(define (removes-all-head-L train)
  (cond
    [(empty? train) train]  
    [(headed-by? train 'L) (removes-all-head-L (rest train))]
    [else (cons (first train) (rest train))]))

;; (removes-middle-values train) removes values from train that
;; contain 'B,'T, and 'P and stop at any other value
;; removes-middle-values: Train -> Train
;; Examples:
(check-expect (removes-middle-values test-train) (cons (make-unit 'C 5) empty))
(check-expect (removes-middle-values empty) empty)

(define (removes-middle-values train)
  (cond
    [(empty? (removes-all-head-L train)) train]
    [(or (headed-by? (removes-all-head-L train) 'B)
         (headed-by? (removes-all-head-L train) 'T)
         (headed-by? (removes-all-head-L train) 'P))
     (removes-middle-values (rest train))]
    [else (cons (first train) (rest train))]))

;; (proper-train? train) checks if train starts with 'L(s), has a middle of
;; 'B(s) (and/or) 'T(s) (and/or) 'P(s) and ends with 'C(s)
;; proper-train?: Train -> Bool
;; Examples:
(check-expect (proper-train? test-train) true)
(check-expect (proper-train? test-train2) true)
(check-expect (proper-train? empty) true)

(define (proper-train? train)
  (cond
    [(empty? train) true]
    [(headed-by? (removes-middle-values train) 'C)
     (proper-train? (removes-middle-values (rest train)))]
    [else false]))

;; Tests:
(check-expect (proper-train? (cons (make-unit 'B 2)
                          (cons (make-unit 'C 6)
                                (cons (make-unit 'T 5)
                                (cons (make-unit 'L 7) empty))))) false)
(check-expect (proper-train? (cons (make-unit 'L 2)
                          (cons (make-unit 'T 6)
                                (cons (make-unit 'L 5)
                                (cons (make-unit 'C 7) empty))))) false)
(check-expect (proper-train? (cons (make-unit 'C 2)
                          (cons (make-unit 'T 6)
                                (cons (make-unit 'B 5)
                                (cons (make-unit 'L 7) empty))))) false)
(check-expect (proper-train? (cons (make-unit 'L 2)
                          (cons (make-unit 'L 6)
                                (cons (make-unit 'B 5)
                                (cons (make-unit 'C 7) empty))))) true)


    

    
  
  




   
;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname blood) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;; ***************************************************
;;   Pronoy Chaudhuri (20709909)
;;   CS 135 Fall 2017
;;   Assignment 02, Problem 4
;; ***************************************************
;;

;; 4(a)

;; (can-donate-to/cond? donor recipient) checks if a donor's blood type is
;; acceptable for the recipient's blood type using the two components [donor
;; recipient]
;; can-donate-to/cond?: Sym Sym -> Bool
;; requires: input of a valid blood type symbol
;; Examples:
(check-expect (can-donate-to/cond? 'A+ 'A+) true)
(check-expect (can-donate-to/cond? 'O+ 'O-) false)

(define (can-donate-to/cond? donor recipient)
  (cond
    [(eq? donor recipient) true]
    [(eq? recipient 'AB+) true]
    [(eq? donor 'O-) true]
    [(eq? donor 'AB+) false]
    [(eq? donor 'AB-) false]
    [(eq? recipient 'O-) false]
    [(eq? recipient 'O+) false]
    [(eq? donor 'A+) false]
    [(eq? recipient 'A-) false]
    [(eq? donor 'B+) false]
    [(eq? recipient 'B-) false]
    [(eq? donor 'O+)
     (cond
       [(eq? recipient 'AB-) false]
       [else true])]
    [(eq? donor 'A-)
     (cond
       [(eq? recipient 'B+) false]
       [else true])]
    [(eq? donor 'B-)
     (cond
       [(eq? recipient 'A+) false]
       [else true])]))

;; Tests:
(check-expect (can-donate-to/cond? 'O+ 'B+) true)
(check-expect (can-donate-to/cond? 'B+ 'AB+) true)
(check-expect (can-donate-to/cond? 'AB+ 'B+) false)
(check-expect (can-donate-to/cond? 'O- 'A+) true)
(check-expect (can-donate-to/cond? 'A+ 'O+) false)
(check-expect (can-donate-to/cond? 'A+ 'A-) false)
(check-expect (can-donate-to/cond? 'B+ 'A-) false)
(check-expect (can-donate-to/cond? 'B+ 'B-) false)
(check-expect (can-donate-to/cond? 'AB- 'B-) false)
(check-expect (can-donate-to/cond? 'O+ 'AB-) false)
(check-expect (can-donate-to/cond? 'A- 'B+) false)
(check-expect (can-donate-to/cond? 'A- 'B-) false)
(check-expect (can-donate-to/cond? 'B- 'A+) false)
(check-expect (can-donate-to/cond? 'A- 'AB-) true)
(check-expect (can-donate-to/cond? 'AB- 'B-) false)
(check-expect (can-donate-to/cond? 'B- 'AB-) true)

;; 4(b)

;; (can-donate-to/bool? donor recipient) checks if a donor's blood type is
;; acceptable for the recipient's blood type using the two components [donor
;; recipient]
;; can-donate-to/bool?: Sym Sym -> Bool
;; requires: input of a valid blood type symbol
;; Examples:
(check-expect (can-donate-to/bool? 'O+ 'A+) true)
(check-expect (can-donate-to/bool? 'O+ 'B-) false)

(define (can-donate-to/bool? donor recipient)
  (or
   (symbol=? donor recipient)
   (symbol=? donor 'O-)
   (symbol=? recipient 'AB+)
   (and (symbol=? donor 'O+) (or (symbol=? recipient 'A+)
                                 (symbol=? recipient 'B+)))
   (and (symbol=? donor 'A-) (or (symbol=? recipient 'A+)
                                 (symbol=? recipient 'AB-)))
   (and (symbol=? donor 'B-) (or (symbol=? recipient 'B+)
                                 (symbol=? recipient 'AB-)))))
    
;; Tests:
(check-expect (can-donate-to/bool? 'O+ 'B+) true)
(check-expect (can-donate-to/bool? 'B+ 'AB+) true)
(check-expect (can-donate-to/bool? 'AB+ 'B+) false)
(check-expect (can-donate-to/bool? 'O- 'A+) true)
(check-expect (can-donate-to/bool? 'A+ 'O+) false)
(check-expect (can-donate-to/bool? 'A+ 'A-) false)
(check-expect (can-donate-to/bool? 'B+ 'A-) false)
(check-expect (can-donate-to/bool? 'B+ 'B-) false)
(check-expect (can-donate-to/bool? 'AB- 'B-) false)
(check-expect (can-donate-to/bool? 'O+ 'AB-) false)
(check-expect (can-donate-to/bool? 'A- 'B+) false)
(check-expect (can-donate-to/bool? 'B- 'A+) false)
(check-expect (can-donate-to/bool? 'A- 'B-) false)
(check-expect (can-donate-to/bool? 'AB- 'B-) false)

       
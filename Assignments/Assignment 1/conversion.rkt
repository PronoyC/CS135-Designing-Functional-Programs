;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname conversion) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;; ***************************************************
;;   Pronoy Chaudhuri (20709909)
;;   CS 135 Fall 2017
;;   Assignment 01, Problem 2
;; ***************************************************
;;

;; Problem 2 (a)

; Purpose: (m/s->mph) converts metres per second to miles per hour,
;          consuming m as metres per second

; m/s->mph: Num -> Num

; Examples:
(check-expect (m/s->mph 0) 0)
(check-expect (m/s->mph 3.3528) 7.5) 

; Definition:
(define (m/s->mph m)
  (* (/ m 1609.344) 3600))

; Tests:
(check-expect (m/s->mph 20.34032) 45.5)
(check-expect (m/s->mph 15.42288) 34.5)

;; Problem 2 (b)

; Purpose: (mph->S/mfn) converts miles per hour to Smoots
;          per millifortnight, consuming m as miles per hour

; mph->S/mfn: Num -> Num 

; Notes: (/ (* 3600 1.7018) (* 1609.344 1209.6)) determines how many
;        miles are in one smoot

; Examples:
(check-expect (mph->S/mfn 0) 0)
(check-expect (mph->S/mfn 1) 317.746)

; Definition:
(define (mph->S/mfn m)
 (/ (/ (* m 1609.344) 1.7018) (/ 3600 1209.6)))

; Tests:
(check-expect (mph->S/mfn 58) 18429.2)
(check-expect (mph->S/mfn 23) 7308.15)

;; Problem 2 (c)

; Purpose: (mpg->L/100km g) converts miles per gallon to litres
;          per 100km, consuming g as gallons

; mpg->L/100km: Num -> Num

; Examples:
(check-expect (mpg->L/100km 1) 235.215)
(check-expect (mpg->L/100km 25) 9.40858)

; Definition:
(define (mpg->L/100km m)
  (/ 3.785411784 (* m (/ 1609.344 100000))))

; Tests:
(check-expect (mpg->L/100km 0.1) 2352.15)
(check-expect (mpg->L/100km 2500) 0.09408583)
  

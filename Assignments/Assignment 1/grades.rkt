;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname grades) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;; ***************************************************
;;   Pronoy Chaudhuri (20709909)
;;   CS 135 Fall 2017
;;   Assignment 01, Problem 3
;; ***************************************************
;;

;; Problem 3 (a)

; Purpose: (final-cs135-grade) determines CS 135 grade, consuming first as
;          first midterm mark, second as second midterm
;          mark, final as final exam mark and assignments as
;          assignment mark

; final-cs135-grade: Num Num Num Num -> Num

; Examples:
(check-expect (final-cs135-grade 0 0 0 0) 5)
(check-expect (final-cs135-grade 100 100 100 100) 100)

; Definition:
(define (final-cs135-grade first second final assignments part)
  (+ (* 0.10 first) (* 0.20 second) (* 0.45 final) (* 0.20 assignments) (* 0.05 part)))

; Tests:
(check-expect (final-cs135-grade 60 50 60 60) 60)
(check-expect (final-cs135-grade 62 53 58 39) 55.7)


;; Problem 3 (b)

; Purpose: (cs135-final-exam-grade-needed) determines final exam mark
;          required to pass CS 135 with a 60%, consuming first as first
;          midterm mark, second as second midterm mark and assignments
;          as assignment mark

; cs135-final-exam-grade-needed: Num Num Num -> Num

; Examples:
(check-expect (cs135-final-exam-grade-needed 60 100 100) 20)
(check-expect (cs135-final-exam-grade-needed 60 50 60) 60)

; Definition:
(define (cs135-final-exam-grade-needed first second assignments)
(/ (- 60(+ (* 0.10 first) (* 0.20 second) (* 0.20 assignments)) 5) 0.45))

; Tests:
(check-expect (cs135-final-exam-grade-needed 70 75 75) 40)
(check-expect (cs135-final-exam-grade-needed 90 50 0) 80)
  
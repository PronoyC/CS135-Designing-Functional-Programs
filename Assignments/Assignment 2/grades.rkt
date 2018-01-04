;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname grades) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;; ***************************************************
;;   Pronoy Chaudhuri (20709909)
;;   CS 135 Fall 2017
;;   Assignment 02, Problem 3
;; ***************************************************
;;

(define midterm1-weight 0.10)
(define midterm2-weight 0.20)
(define final-weight 0.45)
(define assignment-weight 0.20)
(define part-weight 0.05)

;; 3

;; (final-cs135-grade-normal midterm1-grade midterm2-grade final-grade
;; assignment-grade part-grade) determines a final CS 135 grade based on the
;; five components [midterm1-grade midterm2-grade final-grade
;; assignment-grade part-grade]
;; final-cs135-grade-normal: Nat Nat Nat Nat Nat -> Num
;; requires: midterm1-grade   <= 100
;;           midterm2-grade   <= 100
;;           final-grade      <= 100
;;           assignment-grade <= 100
;;           part-grade       <= 100
;; Examples:
(check-expect (final-cs135-grade-normal 100 100 100 100 100) 100)
(check-expect (final-cs135-grade-normal 60 50 70 40 60) 58.5)

(define (final-cs135-grade-normal midterm1-grade midterm2-grade final-grade
                                  assignment-grade part-grade)
  (+ (* midterm1-weight midterm1-grade)(* midterm2-weight midterm2-grade)
     (* final-weight final-grade)(* assignment-weight assignment-grade)
     (* part-weight part-grade)))

;; (final-cs135-grade-min midterm1-grade midterm2-grade final-grade
;; assignment-grade) examines the weighted exam average against assignment
;; marks for CS 135 and finds the smallest value between the two, using the four
;; components [midterm1-grade midterm2-grade final-grade assignment-grade]
;; final-cs135-grade-min: Nat Nat Nat Nat -> Num
;; requires: midterm1-grade   <= 100
;;           midterm2-grade   <= 100
;;           final-grade      <= 100
;;           assignment-grade <= 100
;; Examples:
(check-expect (final-cs135-grade-min 60 60 60 40) 40)
(check-expect (final-cs135-grade-min 70 70 70 80) 70)

(define (final-cs135-grade-min midterm1-grade midterm2-grade final-grade
                               assignment-grade)
  (cond
    [(> (+ (* (/ midterm1-weight 0.75) midterm1-grade) (* (/ midterm2-weight
                                                       0.75) midterm2-grade)
           (* (/ final-weight 0.75) final-grade)) assignment-grade)
     assignment-grade]
    [else (+ (* (/ midterm1-weight 0.75) midterm1-grade) (* (/ midterm2-weight
                                                         0.75) midterm2-grade)
             (* (/ final-weight 0.75) final-grade))]))

;; (final-cs135-grade midterm1-grade midterm2-grade final-grade assignment-grade
;; part-grade) determines a final CS 135 grade based on the five components
;; [midterm1-grade midterm2-grade final-grade assignment-grade part-grade]
;; including conditions that if the weighted exam average or assignments grade
;; is less than 50, the student receives a failing grade of 46 or lower
;; final-cs135-grade: Nat Nat Nat Nat Nat -> Num
;; requires: midterm1-grade   <= 100
;;           midterm2-grade   <= 100
;;           final-grade      <= 100
;;           assignment-grade <= 100
;;           part-grade       <= 100
;; Examples:
(check-expect (final-cs135-grade 100 100 100 100 100) 100)
(check-expect (final-cs135-grade 48 48 48 48 48) 46)

(define (final-cs135-grade midterm1-grade midterm2-grade final-grade
                           assignment-grade part-grade)
  (cond
    [(and (> 50 (final-cs135-grade-min midterm1-grade midterm2-grade final-grade
                                       assignment-grade))
          (>= (final-cs135-grade-normal midterm1-grade midterm2-grade
                                        final-grade assignment-grade part-grade)
              46)) 46]
    [else (final-cs135-grade-normal midterm1-grade midterm2-grade final-grade
                                    assignment-grade part-grade)])) 

;; Tests:
(check-expect (final-cs135-grade 46 47 48 49 50) 46)
(check-expect (final-cs135-grade 60 50 60 40 30) 46)
(check-expect (final-cs135-grade 0 0 0 0 5) 0.25)
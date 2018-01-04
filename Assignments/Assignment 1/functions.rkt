;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname functions) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;; ***************************************************
;;   Pronoy Chaudhuri (20709909)
;;   CS 135 Fall 2017
;;   Assignment 01, Problem 1
;; ***************************************************
;;

;; Problem 1 (a)
(define (distance x1 y1 x2 y2)
  (+ (abs (- x1 x2)) (abs (- y1 y2))))

;; Problem 1 (b)
(define (Stirling n)
  (* (expt n (+ n (/ 1 2))) (exp (- 1 n))))

;; Problem 1 (c)
(define (logit p)
  (log (/ p (- 1 p))))

;; Problem 1 (d)
(define (freq base-frequency interval)
  (* base-frequency (expt 2 (/ interval 12))))

;; Problem 1 (e)
(define (d1 maturity rate volatility spot-price strike-price)
  (*(/ 1 (* volatility (sqrt maturity)))
    (+ (log (/ spot-price strike-price))
    (* (+ rate (/ (expt volatility 2) 2)) maturity))))

;; Problem 1 (f)

(define (height initial-velocity time)
 (- (* initial-velocity time) (* 0.5 9.8 (expt time 2))))
  

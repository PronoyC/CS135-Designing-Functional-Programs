;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname creditcheck) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;; ***************************************************
;;   Pronoy Chaudhuri (20709909)
;;   CS 135 Fall 2017
;;   Assignment 03, Problem 3
;; ***************************************************
;;

;; A Date is a (make-date Nat Nat Nat)
;; requires: year/month/day corresponds to a valid date
;; (in the Gregorian calendar)
(define-struct date (year month day))

;; A Transaction is a (make-transaction Date Num Sym)
(define-struct transaction (tdate amount category))

;; An Account is a (make-account Str Date Num Num Sym)
;; requires: 0 < threshold < limit
(define-struct account (name expires limit threshold exception))

;; 3(a)

;; (date<=? date-1 date-2) compares two dates (date-1 and date-2)
;; and evalutes if the first date is before the second date chronologically
;; date<=?: date date -> Bool
;; Examples:
(check-expect (date<=? (make-date 2016 10 31) (make-date 2017 10 31)) true)

(define (date<=? date-1 date-2)
  (cond
    [(< (date-year date-1) (date-year date-2)) true]
    [(< (date-month date-1) (date-month date-2)) true]
    [(< (date-day date-1) (date-day date-2)) true]
    [(and
      (= (date-year date-1) (date-year date-2))
      (= (date-month date-1) (date-month date-2))
      (= (date-day date-1) (date-day date-2))) true]
    [else false]))

;; Tests:
(check-expect (date<=? (make-date 2017 09 31) (make-date 2017 10 31)) true)
(check-expect (date<=? (make-date 2017 10 30) (make-date 2017 10 31)) true)
(check-expect (date<=? (make-date 2017 10 30) (make-date 2017 10 30)) true)
(check-expect (date<=? (make-date 2017 10 31) (make-date 2016 10 31)) false)

;; 3(b)

;; (approve? transaction account) ensures a transaction
;; complies with account specifications using components (transaction, account)
;; approve?: transaction account -> Bool
;; Examples:
(check-expect (approve? (make-transaction (make-date 2017 10 29) 150 'food)
                        (make-account "Bob" (make-date 2017 10 30) 151 55 'cs))
              true)

(define (approve? transaction account)
  (cond
    [(and (<= (transaction-amount transaction) (account-limit account))
          (date<=? (transaction-tdate transaction) (account-expires account)))
     true]
    [else false]))

;; Tests:
(check-expect (approve? (make-transaction (make-date 2017 10 30) 150 'food)
                        (make-account "Bob" (make-date 2017 10 29) 151 55 'cs))
              false)
(check-expect (approve? (make-transaction (make-date 2017 10 30) 151 'food)
                        (make-account "Bob" (make-date 2017 10 31) 150 55 'cs))
              false)
(check-expect (approve? (make-transaction (make-date 2017 10 30) 150 'food)
                        (make-account "Bob" (make-date 2017 10 29) 149 55 'cs))
              false)

;; 3(c)

;; (alert? transaction account) alerts user if transaction is approved but
;; transaction-amount in transaction exceeds account-threshload in account
;; and the transaction-category in transaction does not match account-exception
;; in account
;; alert?: transaction account -> Bool
;; Examples:
(check-expect (alert? (make-transaction (make-date 2017 10 29) 150 'food)
                      (make-account "Bob" (make-date 2017 10 30) 151 55 'cs))
              true)

(define (alert? transaction account)
  (cond
    [(and (approve? transaction account)
          (> (transaction-amount transaction) (account-threshold account))
          (not (eq? (transaction-category transaction) (account-exception
                                                        account)))) true]
    [else false]))

;; Tests:
(check-expect (approve? (make-transaction (make-date 2017 10 30) 150 'cs)
                        (make-account "Bob" (make-date 2017 10 29) 151 55 'cs))
              false)
(check-expect (alert? (make-transaction (make-date 2017 10 29) 150 'food)
                      (make-account "Bob" (make-date 2017 10 30) 151 55 'cs))
              true)
(check-expect (alert? (make-transaction (make-date 2017 10 29) 152 'food)
                      (make-account "Bob" (make-date 2017 10 30) 151 55 'cs))
              false)

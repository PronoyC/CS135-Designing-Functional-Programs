;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname intro) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;
;; ***************************************************
;;   Pronoy Chaudhuri (20709909)
;;   CS 135 Fall 2017
;;   Assignment 08, Problem 2
;; ***************************************************
;;

;; An association list (AL) is one of:
;; * empty
;; * (cons (list Num Str) AL)

;; 2(a)

;; (keep-ints lon) consumes a list (denoted by "lon")
;; to produce a list of the integers in lon
;; keep-ints: (listof Any) -> (listof Int)
;; Examples:
(check-expect (keep-ints empty) empty)
(check-expect (keep-ints (list 'a 1 "b" 2)) (list 1 2))

(define (keep-ints lon)
  (filter integer? lon))

;; Tests:
(check-expect (keep-ints (list 'a 0.5 "b" "hi")) empty)  
(check-expect (keep-ints (list 54 "hi" 55 2.5)) (list 54 55))

;; 2(b)

;; (contains? elem lon) consumes a value (denoted by "elem")
;; and a list (denoted by "lon") to determine if elem is in lon
;; contains?: Any (listof Any) -> Bool
;; Examples:
(check-expect (contains? 8 empty) false)
(check-expect (contains? 'fun '(racket is fun)) true)
(check-expect (contains? 5 (list 1 2 3)) false)

(define (contains? elem lon)
  (cond
    [(empty? (filter (lambda (lon) (equal? elem lon)) lon)) false]
    [else true]))

;; Tests:
(check-expect (contains? 0 empty) false)
(check-expect (contains? 6 (list 'hi 6 "tango")) true)
(check-expect (contains? "cs" '(cs with racket is horrible)) false)

;; 2(c)

;; (lookup-al k alst) produces the value corresponding to key k,
;; or false if k not present
;; lookup-al: Num AL -> (anyof Str false)
;; Examples:
(check-expect (lookup-al 314 empty) false)
(check-expect (lookup-al 0 (list (list 1 "hey") (list 2 "there"))) false)
(check-expect (lookup-al 2 (list (list 3 "i") (list 2 "love") (list 314 "cs")))
              "love")

(define (lookup-al k alst)
  (cond
    [(cons? (filter (lambda (alst) (equal? k (first alst))) alst))
     (second (first (filter (lambda (alst) (equal? k (first alst))) alst)))]
    [else false]))

;; Tests:
(check-expect (lookup-al 42 (list (list 2 "rackets") (list 5 "logo")
                                  (list 1 "logo") (list 42 "is")
                                  (list 43 "lambda"))) "is")
(check-expect (lookup-al 52 (list (list 2 "rackets") (list 5 "logo")
                                  (list 1 "logo") (list 42 "is")
                                  (list 43 "lambda"))) false)
(check-expect (lookup-al 43 (list (list 2 "rackets") (list 5 "logo")
                                  (list 1 "logo") (list 42 "is")
                                  (list 43 "lambda"))) "lambda")

;; 2(d)

;; (extract-keys alst) consumes an AL (denoted by "alst") and produces
;; all the keys of alst in the order they appear in alst
;; extract-keys: AL -> (listof Num)
;; Examples:
(check-expect (extract-keys empty) empty)
(check-expect (extract-keys (list (list 1 "hey") (list 2 "there"))) (list 1 2))
(check-expect (extract-keys (list (list 3 "i") (list 2 "love") (list 314 "cs")))
              (list 3 2 314))

(define (extract-keys alst)
  (map first alst))

;; Tests:
(check-expect (extract-keys (list (list 2 "rackets") (list 5 "logo")
                                  (list 1 "logo") (list 42 "is")
                                  (list 43 "lambda"))) (list 2 5 1 42 43))
(check-expect (extract-keys (list (list 9292 "h") (list -1 "i") (list 230 "b")))
              (list 9292 -1 230))
(check-expect (extract-keys (list (list 123 "r") (list 456 "ac")
                                  (list 7890 "ke") (list 10 "t")))
              (list 123 456 7890 10))

;; 2(e)

;; Constants:
(define base-case 0)

;; (sum-positive lon) consumes a (listof Int) (denoted by "lon") and
;; produces the sum of the positive integers in lon
;; sum-positive: (listof Int) -> Int
;; Examples:
(check-expect (sum-positive empty) 0)
(check-expect (sum-positive (list 5 -3 4)) 9)

(define (sum-positive lon)
  (foldr + base-case (filter positive? lon)))

;; Tests:
(check-expect (sum-positive (list -6 -4)) 0)
(check-expect (sum-positive (list 6 -4 5 -5 3)) 14)

;; 2(f)

;; Constants:
(define offset 1)
  
;; (countup-to n b) produces a list from n...b
;; countup-to: Int Int -> (listof Int)
;; requires: n <= b
;; Examples:
(check-expect (countup-to 6 8) (list 6 7 8))
(check-expect (countup-to -8 -6) (list -8 -7 -6))
(check-expect (countup-to -8 1) (list -8 -7 -6 -5 -4 -3 -2 -1 0 1))

(define (countup-to n b)
  (build-list (+ (- b n) offset) (lambda (val) (+ val n))))

;; Tests:
(check-expect (countup-to 12 30)
              (list 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30))
(check-expect (countup-to -16 -1)
              (list -16 -15 -14 -13 -12 -11 -10 -9 -8 -7 -6 -5 -4 -3 -2 -1))
(check-expect (countup-to 0 2) (list 0 1 2))

;; 2(g)

;; (shout los) consumes a (listof Str) (denoted by "los") and produces the
;; same los with all string elements in upper-case
;; shout: (listof Str) -> (listof Str)
;; Examples:
(check-expect (shout empty) empty)
(check-expect (shout '("get" "off" "my" "lawn")) '("GET" "OFF" "MY" "LAWN"))
(check-expect (shout '("get" "OFF" "my" "lawn")) '("GET" "OFF" "MY" "LAWN"))

(define (shout los)
  (map (lambda (los) (list->string (map char-upcase (string->list los)))) los))

;; Tests:
(check-expect (shout '("hEllo" "ThErE")) '("HELLO" "THERE"))
(check-expect (shout '("i love president feridun"))
              '("I LOVE PRESIDENT FERIDUN"))
(check-expect (shout '("cs" "is" "life")) '("CS" "IS" "LIFE"))

;; 2(h)

;; (make-validator lon) consumes a (listof Any) (denoted by "lon") and
;; produces a predicate function. The produced function consumes a single item
;; of type Any and produces a Boolean that determines if the item appears in
;; the list that was consumed by make-validator
;; make-validator: (listof Any) -> (Any -> Bool)
;; Examples:
(check-expect (primary-colour? 'red) true)
(check-expect (primary-colour? 'yellow) false)
(check-expect (my-empty? empty) true)
(check-expect (my-empty? 7) false)

(define (make-validator lon)
  (local
    [(define (f m)
       (cond
         [(and (empty? lon) (empty? m)) true]
         [(empty? (filter (lambda (lon) (equal? m lon)) lon)) false]
         [else true]))]
    f))

;; Tests:
(check-expect (legend? 'feridun) true)
(check-expect (legend? 'einstein) false)
(check-expect (difficulty? 'easy) true)
(check-expect (difficulty? 'yes) false)
(check-expect (my-fav-numbers? 420) true)
(check-expect (my-fav-numbers? 2) false)

;; Predicates Created:
(define primary-colour? (make-validator '(red blue green)))
(define my-empty? (make-validator empty))
(define legend? (make-validator '(vasiga feridun mrgoose)))
(define difficulty? (make-validator '(easy medium hard)))
(define my-fav-numbers? (make-validator '(420 3.14 1)))



  
  



                            


              

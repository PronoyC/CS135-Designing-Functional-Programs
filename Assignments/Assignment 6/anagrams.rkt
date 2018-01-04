;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname anagrams) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;
;; ***************************************************
;;   Pronoy Chaudhuri (20709909)
;;   CS 135 Fall 2017
;;   Assignment 06, Problem 2
;; ***************************************************
;;

;; (contains? elem list) consumes an element and a list and checks
;; if elem is in list
;; contains?: Any (listof Any) -> Bool
;; Examples:
(check-expect (contains? 'fun (cons 'racket (cons 'is (cons 'fun empty)))) true)
(check-expect (contains? 'fun (cons 'racket (cons 'cs (cons 'na empty)))) false)
(check-expect (contains? 'test empty) false)

(define (contains? elem lst)
  (cond [(empty? lst) false]
        [(equal? elem (first lst)) true]
        [else (contains? elem (rest lst))]))

;; 2(a)

;; (insert-chars char1 sort-rest) inserts a char into the list (sort-rest)
;; so the entire list is sorted
;; insert-chars: Char (listof Char) -> (listof Char)
;; Examples:
(check-expect (insert-chars #\r (list #\a #\c #\e #\c #\a #\r))
              (list #\a #\c #\e #\c #\a #\r #\r))
(check-expect (insert-chars #\A empty) (list #\A))

(define (insert-chars char1 sort-rest)
  (cond
    [(empty? sort-rest) (list char1)]
    [(char<=? char1 (first sort-rest)) (cons char1 sort-rest)]
    [else (cons (first sort-rest) (insert-chars char1 (rest sort-rest)))]))


;; (sort-chars lst) consumes a list and produces the same list in sorted order
;; sort-chars: (listof Char) -> (listof Char)
;; Examples:
(check-expect (sort-chars (list #\o #\r #\d #\e #\r))
              (list #\d #\e #\o #\r #\r))
(check-expect (sort-chars empty) empty)

(define (sort-chars lst)
  (cond
    [(empty? lst) empty]
    [else (insert-chars (first lst) (sort-chars (rest lst)))]))

;; Tests:
(check-expect (sort-chars (string->list "racecar"))
              (list #\a #\a #\c #\c #\e #\r #\r))
(check-expect (sort-chars (string->list "sTOry"))
              (list #\O #\T #\r #\s #\y))
              
;; 2(b)

;; (anagrams/sort? string1 string2) consumes two strings (string1 and string2)
;; to determine if they are both anagrams of each other
;; anagrams/sort?: Str Str -> Bool
;; Examples:
(check-expect (anagrams/sort? "racecar" "racecar") true)
(check-expect (anagrams/sort? "hi" "racecar") false)

(define (anagrams/sort? string1 string2)
  (cond
    [(equal? (sort-chars (string->list string1))
             (sort-chars (string->list string2))) true]
    [else false]))

;; Tests:
(check-expect (anagrams/sort? "satan" "santa") true)
(check-expect (anagrams/sort? "piece" "peace") false)
(check-expect (anagrams/sort? "Racecar" "racecar") false)

;; 2(c)

;; (freq? lst elem num) finds how many times elem appears in a list (lst)
;; and creates a list with first element being elem and second element being
;; how many times elem apears
;; freq?: (listof Any) Any Nat -> (listof Any Nat)
;; Examples:
(check-expect (freq? (list "red" "red" 'hi) "red" 0) (list "red" 2))
(check-expect (freq? (list "red" "red" 'hi) 'hi 0) (list 'hi 1))
(check-expect (freq? empty 42 0) (list 42 0))

(define (freq? lst elem num)
  (cond
    [(empty? lst) (list elem num)]
    [(equal? elem (first lst)) (freq? (rest lst) elem (+ num 1))]
    [else (freq? (rest lst) elem num)]))

;; (freq?/acc lst elem lst2) accumulates results of (freq? lst elem num)
;; and ignores elems that are a part of lst2 (already checked elems)
;; freq?/acc: (Listof Any) (Listof Any) -> (Listof Any Nat)
;; Examples:
(check-expect (freq?/acc empty empty) empty)
(check-expect (freq?/acc (list "alpha" 'alpha 42) empty)
              (list (list "alpha" 1) (list 'alpha 1) (list 42 1)))

(define (freq?/acc lst lst2)
  (cond
    [(empty? lst) empty]
    [(contains? (first lst) lst2) (freq?/acc (rest lst) lst2)]
    [else (cons (freq? lst (first lst) 0)
                (freq?/acc (rest lst)
                           (cons (first lst) lst2)))]))
                  
;; (freq-count lst) consumes a list (lst) and produces a list
;; that pairs an object in the lst with a number that indicates how many
;; times that object occurs in the lst
;; freq-count: (listof Any) -> (listof (listof Any Num))
;; Examples:
(check-expect (freq-count (list "hello" 44 "hello"))
              (list (list "hello" 2) (list 44 1)))
(check-expect (freq-count empty) empty)

(define (freq-count lst)
  (freq?/acc lst empty))

;; Tests:
(check-expect (freq-count (list "alpha" 'alpha 42))
              (list (list "alpha" 1) (list 'alpha 1) (list 42 1)))
(check-expect (freq-count (list 2 "hi" 'hi 2 2 "hi"))
              (list (list 2 3) (list "hi" 2) (list 'hi 1)))

;; 2(d)

;; (freq-eq-check? lst1 lst2) checks if lst1 is a subset of lst2
;; freq-eq-check?: (listof (listof Any Nat)) (listof (listof Any Nat)) -> Bool
;; Examples:
(check-expect (freq-eq-check? empty (list (list 'hi 2))) true)
(check-expect (freq-eq-check? (list (list 'hi 1)) (list (list 'hi 2))) false)
(check-expect (freq-eq-check? (list (list 'hi 2)) (list (list 'hi 2))) true)

(define (freq-eq-check? lst1 lst2)
  (cond
    [(empty? lst1) true]
    [(contains? (first lst1) lst2) (freq-eq-check? (rest lst1) lst2)]
    [else false]))

;; (freq-equiv? lst1 lst2) checks if two lists are equal, regardless of order
;; of elements
;; freq-equiv?: (listof (listof Any Nat)) (listof (listof Any Nat)) -> Bool
;; Examples:
(check-expect (freq-equiv? '((red 5) (blue 9) ("string" 0))
                           '((blue 9) (red 5) ("string" 0))) true)
(check-expect (freq-equiv? '((red 5) (blue 6)) '((blue 5) (red 6))) false)
(check-expect (freq-equiv? empty empty) true)

(define (freq-equiv? lst1 lst2)
  (cond
    [(and (freq-eq-check? lst1 lst2) (freq-eq-check? lst2 lst1)) true]
    [else false]))

;; Tests:
(check-expect (freq-equiv? '((blue 5) (red 4)) '((red 4) (blue 5))) true)
(check-expect (freq-equiv? '((blue 5)) '((red 4) (blue 5))) false)
(check-expect (freq-equiv? '((red 4) (blue 5)) '((blue 5))) false)

;; 2(e)

;; (anagrams/count? string1 string2) checks if string1 and string2 are anagrams
;; of each other
;; anagrams/count?: Str Str -> Bool
;; Examples:
(check-expect (anagrams/count? "racecar" "racecar") true)
(check-expect (anagrams/count? "hi" "racecar") false)

(define (anagrams/count? string1 string2)
  (cond
    [(freq-equiv? (freq-count (string->list string1))
                  (freq-count (string->list string2))) true]
    [else false]))

;; Tests:
(check-expect (anagrams/count? "satan" "santa") true)
(check-expect (anagrams/count? "piece" "peace") false)
(check-expect (anagrams/count? "Racecar" "racecar") false)
    









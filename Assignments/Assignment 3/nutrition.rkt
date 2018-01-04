;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname nutrition) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;; ***************************************************
;;   Pronoy Chaudhuri (20709909)
;;   CS 135 Fall 2017
;;   Assignment 03, Problem 2
;; ***************************************************
;;

;; A Nutri-Fact is a (make-nutri-fact Str Num Num Num Num Num)
;; requires: 0 < serving
;; fat + carbs + protein <= serving
;; 0 <= sugar <= carbs
;; 0 <= fat, protein
(define-struct nutri-fact (name serving fat carbs sugar protein))

;; 2(a)

;; my-nutri-fact-fn: nutri-fact -> Any
(define (my-nutri-fact-fn info)
  (... (nutri-fact-name info)...
       (nutri-fact-serving info)...
       (nutri-fact-fat info)...
       (nutri-fact-carbs info)...
       (nutri-fact-sugar info)...
       (nutri-fact-protein info)... ))

;; 2(b)

;; (ratio nutri-fact new-serving) creates a ratio from new-serving value
;; and nutri-fact
;; adjust: nutri-fact Num -> Num
;; requires: 0 <= new-serving
;; Examples:
(check-expect (ratio (make-nutri-fact "Honey Nut Cheerios" 29 1.5 23 9 2)58)
              2)
(check-expect (ratio (make-nutri-fact "Honey Nut Cheerios" 29 1.5 23 9 2)29)
              1)

(define (ratio nutri-fact new-serving)
  (/ new-serving (nutri-fact-serving nutri-fact)))

;; (resize nutri-fact new-serving) scales nutri-fact
;; by the given new-serving
;; resize: nutri-fact Num -> nutri-fact
;; requires: 0 <= new-serving
;; Examples:
(check-expect (resize (make-nutri-fact "Honey Nut Cheerios" 29 1.5 23 9 2) 58)
              (make-nutri-fact "Honey Nut Cheerios" 58 3 46 18 4))
(check-expect (resize (make-nutri-fact "Honey Nut Cheerios" 29 1.5 23 9 2) 29)
              (make-nutri-fact "Honey Nut Cheerios" 29 1.5 23 9 2))

(define (resize nutri-fact new-serving)
  (make-nutri-fact
   (nutri-fact-name nutri-fact)
   new-serving
   (* (nutri-fact-fat nutri-fact) (ratio nutri-fact new-serving))
   (* (nutri-fact-carbs nutri-fact) (ratio nutri-fact new-serving))
   (* (nutri-fact-sugar nutri-fact) (ratio nutri-fact new-serving))
   (* (nutri-fact-protein nutri-fact) (ratio nutri-fact new-serving))))

;; Tests:
(check-expect (resize (make-nutri-fact "Honey Nut Cheerios" 29 1.5 23 9 2) 14.5)
              (make-nutri-fact "Honey Nut Cheerios" 14.5 0.75 11.5 4.5 1))

;; 2(c)

;; (calories nutri-fact) calculates calories from nutri-fact values
;; calories: nutri-fact -> Num
;; Examples:
(check-expect (calories (make-nutri-fact "Honey Nut Cheerios" 29 1.5 23 9 2))
              113.5)

(define (calories nutri-fact)
  (+
   (* (nutri-fact-fat nutri-fact) 9)
   (* (nutri-fact-carbs nutri-fact) 4)
   (* (nutri-fact-protein nutri-fact) 4)))

;; Tests:
(check-expect (calories (make-nutri-fact "Honey Nut Cheerios" 14.5 0.75 11.5
                                         4.5 1)) 56.75)

;; 2(d)

;; (ratio-for-diet nutri-fact-1 nutri-fact-2) creates ratio to convert
;; nutri-fact-1 to be appropriately proportioned to nutri-fact-2
;; adjust: nutri-fact nutri-fact -> Num
;; Examples:
(check-expect (ratio-for-diet
               (make-nutri-fact "Honey Nut Cheerios" 29 1.5 23 9 2)
               (make-nutri-fact "Honey Nut Cheerios" 58 1.5 23 9 2))
              2)
(check-expect (ratio-for-diet
               (make-nutri-fact "Honey Nut Cheerios" 29 1.5 23 9 2)
               (make-nutri-fact "Honey Nut Cheerios" 29 1.5 23 9 2))
              1)

(define (ratio-for-diet nutri-fact-1 nutri-fact-2)
  (/ (nutri-fact-serving nutri-fact-2) (nutri-fact-serving nutri-fact-1)))

;; (choose-for-diet nutri-fact-1 nutri-fact-2) chooses the best diet for user
;; based on values provided by nutri-fact-1 and nutri-fact-2
;; choose-for-diet: nutri-fact nutri-fact -> nutri-fact
;; Examples:
(check-expect (choose-for-diet
               (make-nutri-fact "Honey Nut Cheerios" 29 1.5 23 9 2)
               (make-nutri-fact "Honey Nut Cheerios" 29 1.5 23 10 2))
              (make-nutri-fact "Honey Nut Cheerios" 29 1.5 23 9 2))
(check-expect (choose-for-diet
               (make-nutri-fact "Honey Nut Cheerios" 58 1.5 23 10 2)
               (make-nutri-fact "Honey Nut Cheerios" 29 1.5 23 9 2))
              (make-nutri-fact "Honey Nut Cheerios" 58 1.5 23 10 2))

(define (choose-for-diet nutri-fact-1 nutri-fact-2)
  (cond
    [(> (* (nutri-fact-sugar nutri-fact-1)
           (ratio-for-diet nutri-fact-1 nutri-fact-2))
        (nutri-fact-sugar nutri-fact-2))
     nutri-fact-2]
    [(< (* (nutri-fact-sugar nutri-fact-1)
           (ratio-for-diet nutri-fact-1 nutri-fact-2))
        (nutri-fact-sugar nutri-fact-2))
     nutri-fact-1]
    [(> (* (nutri-fact-protein nutri-fact-1)
           (ratio-for-diet nutri-fact-1 nutri-fact-2))
        (nutri-fact-protein nutri-fact-2))
     nutri-fact-1]
    [(< (* (nutri-fact-protein nutri-fact-1)
           (ratio-for-diet nutri-fact-1 nutri-fact-2))
        (nutri-fact-protein nutri-fact-2))
     nutri-fact-2]
    [(> (* (nutri-fact-carbs nutri-fact-1)
           (ratio-for-diet nutri-fact-1 nutri-fact-2))
        (nutri-fact-carbs nutri-fact-2))
     nutri-fact-2]
    [(< (* (nutri-fact-carbs nutri-fact-1)
           (ratio-for-diet nutri-fact-1 nutri-fact-2))
        (nutri-fact-carbs nutri-fact-2))
     nutri-fact-1]
    [(> (* (nutri-fact-fat nutri-fact-1)
           (ratio-for-diet nutri-fact-1 nutri-fact-2))
        (nutri-fact-fat nutri-fact-2))
     nutri-fact-2]
    [(< (* (nutri-fact-fat nutri-fact-1)
           (ratio-for-diet nutri-fact-1 nutri-fact-2))
        (nutri-fact-fat nutri-fact-2))
     nutri-fact-1]
    [else nutri-fact-1]))

;; Tests:
(check-expect (choose-for-diet
               (make-nutri-fact "Honey Nut Cheerios" 29 1.5 23 9 2)
               (make-nutri-fact "Honey Nut Cheerios" 58 1.5 23 8 2))
              (make-nutri-fact "Honey Nut Cheerios" 58 1.5 23 8 2))
(check-expect (choose-for-diet
               (make-nutri-fact "Honey Nut Cheerios" 58 1.5 23 10 2)
               (make-nutri-fact "Honey Nut Cheerios" 29 1.5 23 5 200))
              (make-nutri-fact "Honey Nut Cheerios" 29 1.5 23 5 200))
(check-expect (choose-for-diet
               (make-nutri-fact "Honey Nut Cheerios" 58 1.5 23 22 50)
               (make-nutri-fact "Honey Nut Cheerios" 29 1.5 23 11 2))
              (make-nutri-fact "Honey Nut Cheerios" 58 1.5 23 22 50))
(check-expect (choose-for-diet
               (make-nutri-fact "Honey Nut Cheerios" 29 1.5 23 10 2)
               (make-nutri-fact "Honey Nut Cheerios" 87 1.5 23 30 6))
              (make-nutri-fact "Honey Nut Cheerios" 87 1.5 23 30 6))
(check-expect (choose-for-diet
               (make-nutri-fact "Honey Nut Cheerios" 58 1.5 23 18 4)
               (make-nutri-fact "Honey Nut Cheerios" 29 1.5 23 9 2))
              (make-nutri-fact "Honey Nut Cheerios" 58 1.5 23 18 4))
(check-expect (choose-for-diet
               (make-nutri-fact "Honey Nut Cheerios" 58 1.5 23 10 2)
               (make-nutri-fact "Honey Nut Cheerios" 29 1.5 11.5 5 1))
              (make-nutri-fact "Honey Nut Cheerios" 58 1.5 23 10 2))
(check-expect (choose-for-diet
               (make-nutri-fact "Honey Nut Cheerios" 58 3 23 10 2)
               (make-nutri-fact "Honey Nut Cheerios" 58 1.5 23 10 2))
              (make-nutri-fact "Honey Nut Cheerios" 58 1.5 23 10 2))
(check-expect (choose-for-diet
               (make-nutri-fact "Honey Nut Cheerios" 29 1.5 23 10 2)
               (make-nutri-fact "Honey Nut Cheerios" 29 1.5 23 10 2))
              (make-nutri-fact "Honey Nut Cheerios" 29 1.5 23 10 2))
       
;; 2(e)

;; (valid-nutri-fact? nutri-fact) determines if a value is a nutri-fact or not
;; valid-nutri-fact? Any -> Bool
;; Examples:
(check-expect (valid-nutri-fact?
               (make-nutri-fact "Honey Nut Cheerios" 29 1.5 23 9 2)) true) 

(define (valid-nutri-fact? nutri-fact)
  (nutri-fact? nutri-fact))

;; Tests:
(check-expect (valid-nutri-fact? 'hello) false)
   


  
  


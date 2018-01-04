;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname battle) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;; ***************************************************
;;   Pronoy Chaudhuri (20709909)
;;   CS 135 Fall 2017
;;   Assignment 03, Problem 4
;; ***************************************************
;;

;; A Card is a (make-card Nat Sym)
;; requires: 1 <= strength <= 9
;; color is one of: ’red ’yellow ’green ’blue ’purple ’brown
(define-struct card (strength colour))

;; A Hand is a (make-hand Card Card Card)
(define-struct hand (c1 c2 c3))

;; 4

;; (colour hand) analyzes card colours
;; from hand to check if all are the same colour
;; colour: hand -> Bool
;; Examples:
(check-expect (colour (make-hand (make-card 1 'red)
                                 (make-card 3 'red)
                                 (make-card 5 'red))) true)
(check-expect (colour (make-hand (make-card 1 'red)
                                 (make-card 3 'red)
                                 (make-card 5 'blue))) false)
                        
(define (colour hand)
  (cond
    [(and (eq? (card-colour (hand-c1 hand)) (card-colour (hand-c2 hand)))
          (eq? (card-colour (hand-c2 hand)) (card-colour (hand-c3 hand)))
          (eq? (card-colour (hand-c3 hand)) (card-colour (hand-c1 hand))))
     true]
    [else false]))

;; (three-of-a-kind hand) analyzes card strengths
;; from hand to check if all are the same strength
;; three-of-a-kind: hand -> Bool
;; Examples:
(check-expect (three-of-a-kind (make-hand (make-card 3 'red)
                                          (make-card 3 'red)
                                          (make-card 3 'blue))) true)
(check-expect (three-of-a-kind (make-hand (make-card 1 'red)
                                          (make-card 3 'red)
                                          (make-card 5 'red))) false)
                        
(define (three-of-a-kind hand)
  (cond
    [(and (= (card-strength (hand-c1 hand)) (card-strength (hand-c2 hand)))
          (= (card-strength (hand-c2 hand)) (card-strength (hand-c3 hand)))
          (= (card-strength (hand-c3 hand)) (card-strength (hand-c1 hand))))
     true]
    [else false]))

;; (colour-run hand) analyzes card strengths and colours
;; from hand to check if hand forms a run and all cards have the same colour
;; colour-run: hand -> Bool
;; Examples:
(check-expect (colour-run (make-hand (make-card 3 'red)
                                     (make-card 4 'red)
                                     (make-card 5 'red))) true)
(check-expect (colour-run (make-hand (make-card 1 'red)
                                     (make-card 3 'red)
                                     (make-card 5 'red))) false)
                        
(define (colour-run hand)
  (cond
    [(and (colour hand) (run hand)) true]
    [else false]))


;; (run hand) analyzes card strengths from hand to check if cards contain
;; consecutive strength numbers
;; run: hand -> Bool
;; Examples:
(check-expect (run (make-hand (make-card 4 'red)
                              (make-card 3 'red)
                              (make-card 5 'red))) true)
(check-expect (run (make-hand (make-card 5 'red)
                              (make-card 4 'red)
                              (make-card 3 'red))) true)
(check-expect (run (make-hand (make-card 5 'red)
                              (make-card 3 'red)
                              (make-card 4 'red))) true)
(check-expect (run (make-hand (make-card 1 'red)
                              (make-card 3 'red)
                              (make-card 5 'blue))) false)
                        
(define (run hand)
  (cond
    [(and (= (card-strength (hand-c1 hand))
             (+ (min (card-strength (hand-c2 hand))
                     (card-strength (hand-c3 hand))) 1))
          (= (card-strength (hand-c1 hand))
             (- (max (card-strength (hand-c2 hand))
                     (card-strength (hand-c3 hand))) 1))) true]
    [(and (= (card-strength (hand-c2 hand))
             (+ (min (card-strength (hand-c1 hand))
                     (card-strength (hand-c3 hand))) 1))
          (= (card-strength (hand-c2 hand))
             (- (max (card-strength (hand-c1 hand))
                     (card-strength (hand-c3 hand))) 1))) true]
    [(and (= (card-strength (hand-c3 hand))
             (+ (min (card-strength (hand-c2 hand))
                     (card-strength (hand-c1 hand))) 1))
          (= (card-strength (hand-c3 hand))
             (- (max (card-strength (hand-c2 hand))
                     (card-strength (hand-c1 hand))) 1))) true]
    [else false]))

;; (sum hand) adds up card strengths from hand
;; sum: hand -> Num
;; Examples:
(check-expect (sum (make-hand (make-card 3 'red)
                              (make-card 4 'red)
                              (make-card 5 'red))) 12)

(define (sum hand)
  (+  (card-strength (hand-c1 hand))
      (card-strength (hand-c2 hand))
      (card-strength (hand-c3 hand))))

;; (battle hand-1 hand-2) consumes hand-1 and hand-2 to
;; determine best hand between the two hands
;; battle: hand hand -> Sym
;; Examples:
(check-expect (battle (make-hand (make-card 3 'red)
                                 (make-card 3 'red)
                                 (make-card 3 'red))
                      (make-hand (make-card 3 'red)
                                 (make-card 3 'red)
                                 (make-card 3 'red))) 'player1)
(check-expect (battle (make-hand (make-card 3 'red)
                                 (make-card 3 'red)
                                 (make-card 3 'red))
                      (make-hand (make-card 3 'red)
                                 (make-card 4 'red)
                                 (make-card 5 'red))) 'player2)

(define (battle hand-1 hand-2)
  (cond
    [(and (colour-run hand-1) (not (colour-run hand-2))) 'player1]
    [(and (colour-run hand-2) (not (colour-run hand-1))) 'player2]
    [(and (three-of-a-kind hand-1) (not (three-of-a-kind hand-2))) 'player1]
    [(and (three-of-a-kind hand-2) (not (three-of-a-kind hand-1))) 'player2]
    [(and (colour hand-1) (not (colour hand-2))) 'player1]
    [(and (colour hand-2) (not (colour hand-1))) 'player2]
    [(and (run hand-1) (not (run hand-2))) 'player1]
    [(and (run hand-2) (not (run hand-1))) 'player2]
    [(> (sum hand-1) (sum hand-2)) 'player1]
    [(> (sum hand-2) (sum hand-1)) 'player2]
    [else 'player1]))

;; Tests:
(check-expect (battle (make-hand (make-card 3 'red)
                                 (make-card 4 'red)
                                 (make-card 5 'red))
                      (make-hand (make-card 3 'red)
                                 (make-card 3 'red)
                                 (make-card 3 'red))) 'player1)

(check-expect (battle (make-hand (make-card 3 'red)
                                 (make-card 3 'blue)
                                 (make-card 3 'red))
                      (make-hand (make-card 3 'red)
                                 (make-card 4 'blue)
                                 (make-card 5 'red))) 'player1)

(check-expect (battle (make-hand (make-card 3 'red)
                                 (make-card 4 'blue)
                                 (make-card 5 'red))
                      (make-hand (make-card 3 'red)
                                 (make-card 3 'blue)
                                 (make-card 3 'red))) 'player2)

(check-expect (battle (make-hand (make-card 3 'red)
                                 (make-card 4 'red)
                                 (make-card 7 'red))
                      (make-hand (make-card 3 'red)
                                 (make-card 4 'blue)
                                 (make-card 3 'red))) 'player1)

(check-expect (battle (make-hand (make-card 3 'red)
                                 (make-card 4 'blue)
                                 (make-card 7 'red))
                      (make-hand (make-card 3 'red)
                                 (make-card 4 'red)
                                 (make-card 3 'red))) 'player2)

(check-expect (battle (make-hand (make-card 4 'purple)
                                 (make-card 6 'red)
                                 (make-card 5 'blue))
                      (make-hand (make-card 3 'red)
                                 (make-card 4 'blue)
                                 (make-card 3 'purple))) 'player1)

(check-expect (battle (make-hand (make-card 3 'purple)
                                 (make-card 6 'red)
                                 (make-card 5 'blue))
                      (make-hand (make-card 3 'red)
                                 (make-card 4 'blue)
                                 (make-card 5 'purple))) 'player2)

(check-expect (battle (make-hand (make-card 4 'purple)
                                 (make-card 7 'red)
                                 (make-card 5 'blue))
                      (make-hand (make-card 3 'red)
                                 (make-card 4 'blue)
                                 (make-card 3 'purple))) 'player1)

(check-expect (battle (make-hand (make-card 3 'purple)
                                 (make-card 4 'red)
                                 (make-card 3 'blue))
                      (make-hand (make-card 4 'red)
                                 (make-card 7 'blue)
                                 (make-card 5 'purple))) 'player2)


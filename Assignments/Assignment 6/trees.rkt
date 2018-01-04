;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname trees) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;
;; ***************************************************
;;   Pronoy Chaudhuri (20709909)
;;   CS 135 Fall 2017
;;   Assignment 06, Problem 3
;; ***************************************************
;;

(define-struct node (key val left right))
;; A Node is a (make-node Num Str BT BT)
;; A binary tree (BT) is one of:
;; - empty
;; - Node

(define exampleBT-1
  (make-node 1 "a"
             (make-node 7 "b" empty empty)
             (make-node 3 "c" (make-node 7 "d" empty empty) empty)))

(define exampleBT-2
  (make-node 2 "a"
             (make-node 1 "b" (make-node 2 "c" (make-node 6 "d" empty empty)
                                         empty) empty)
             (make-node 3 "c" (make-node 7 "d" empty empty) empty)))

(define exampleBT-3
  (make-node 3 "a"
             (make-node 4 "a" (make-node 5 "b" empty empty) empty)
             (make-node 3 "c" (make-node 9 "d" empty empty) empty)))

;; 3(a)

;; (height bt) consumes a bt (binary tree) and produces the height of the tree
;; height: BT -> Nat
;; Examples:
(check-expect (height exampleBT-1) 3)
(check-expect (height empty) 0)

(define (height bt)
  (cond
    [(empty? bt) 0]
    [else (max (+ (height (node-left bt)) 1)
               (+ (height (node-right bt)) 1))]))

;; Tests:
(check-expect (height exampleBT-2) 4)
(check-expect (height exampleBT-3) 3)

;; 3(b)

;; (find-in-tree bt lst) consumes a bt (binary tree) to find a key using
;; elements of lst
;; find-in-tree: BT (listof Sym) -> (anyof Num false)
;; requires: (listof Sym) has (anyof empty 'L 'R)
;; Examples:
(check-expect (find-in-tree exampleBT-1 '(R L)) 7)
(check-expect (find-in-tree exampleBT-1 empty) 1)
(check-expect (find-in-tree empty '(R L)) false)

(define (find-in-tree bt lst)
  (cond
    [(empty? lst) (node-key bt)]
    [(empty? bt) false]
    [(symbol=? 'R (first lst)) (find-in-tree (node-right bt) (rest lst))]
    [else (find-in-tree (node-left bt) (rest lst))]))

;; Tests:
(check-expect (find-in-tree exampleBT-2 '(L L L)) 6)
(check-expect (find-in-tree exampleBT-3 '(R L)) 9)

;; 3(c)

;; (prune bt key) consumes a bt (binary tree) and removes a subtree with
;; a particular key
;; prune: BT Num -> BT
;; Examples:
(check-expect (prune exampleBT-1 1) empty)
(check-expect (prune exampleBT-1 7)
              (make-node 1 "a" empty (make-node 3 "c" empty empty)))
(check-expect (prune empty 55) empty)

(define (prune bt key)
  (cond
    [(empty? bt) empty]
    [(= (node-key bt) key) empty]
    [else (make-node (node-key bt) (node-val bt)
                     (prune (node-left bt) key)
                     (prune (node-right bt) key))]))

;; Tests:
(check-expect (prune exampleBT-2 3)
              (make-node 2 "a"
                         (make-node 1 "b"
                                    (make-node 2 "c"
                                               (make-node 6 "d" empty empty)
                                               empty) empty) empty))
(check-expect (prune exampleBT-3 3) empty)
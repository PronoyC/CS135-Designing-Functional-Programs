;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname trie) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;
;; ***************************************************
;;   Pronoy Chaudhuri (20709909)
;;   CS 135 Fall 2017
;;   Assignment 07, Problem 2
;; ***************************************************
;;

(require "a07lib.rkt")

;; 2(a)

(define a-tnode
  (make-tnode #\a false
              (list (make-tnode #\t true empty))))

(define c-tnode
  (make-tnode #\c false
              (list (make-tnode #\o false
                                (list (make-tnode #\o true empty)
                                      (make-tnode #\w true empty)))
                    (make-tnode #\s false
                                (list
                                 (make-tnode #\1 false
                                                  (list (make-tnode #\1 false
                                                                    (list (make-tnode #\5 true empty)
                                                                          (make-tnode #\6 true empty)))
                                                        (make-tnode #\3 false
                                                                    (list (make-tnode #\5 true empty)
                                                                          (make-tnode #\6 true empty))))))))))

(define a-c-trie (make-trie (list a-tnode c-tnode)))

;; 2(b)

;; trie-template: Trie -> Any

;;(define (trie-template trie1)
;;(...list-tnode-template (trie-children trie1)...)

;; list-tnode-template: (listof TNode) -> Any

;;(define (list-tnode-template tlon)
;;  (cond
;;    [(empty? tlon)...]
;;    [else (...(tnode-template (first tlon))...
;;              (list-tnode-template (rest tlon))...)]))

;; tnode-template: TNode -> Any

;;(define (tnode-template child)
;; (.... (tnode-key child)...
;;       (tnode-ends-word? child)...
;;       (list-tnode-template (rest child))...))

;; 2(c)

;; (trie-check word tlon) takes a list of chars (denoted by "word) and checks
;; if it matches the list of TNodes in a way that produces a word (denoted by
;; "tlon")
;; trie-check: (listof Str) (listof TNode) -> Bool
;; Examples:
(check-expect (trie-check empty empty) false)
(check-expect (trie-check (list #\a #\t) (trie-children a-c-trie)) true)
(check-expect (trie-check (list #\a) (trie-children a-c-trie)) false)

(define (trie-check word tlon)
  (cond
    [(or (empty? tlon) (empty? word)) false]
    [(and (empty? (rest word)) (tnode-ends-word? (first tlon))
          (char=? (first word) (tnode-key (first tlon)))) true]
    [(char=? (first word) (tnode-key (first tlon))) (trie-check
                                                     (rest word)
                                                     (tnode-children
                                                      (first tlon)))]
    [else (trie-check word (rest tlon))]))
                                             
;; (in-trie? word trie1) checks if a string (denoted by "word") exists in a
;; Trie (denoted by "trie1")
;; in-trie?: Str Trie -> Bool
;; Examples:
(check-expect (in-trie? "cs" a-c-trie) false)
(check-expect (in-trie? "cs115" a-c-trie) true)
(check-expect (in-trie? " " blank-trie) false)

(define (in-trie? word trie1)
  (trie-check (string->list word) (trie-children trie1)))

;; Tests:
(check-expect (in-trie? "cower" a-c-trie) false)
(check-expect (in-trie? "cat" c-d-trie) true)
(check-expect (in-trie? "actress" toydict-trie) true)
(check-expect (in-trie? "ho" h-u-trie) false)
(check-expect (in-trie? "ha" h-u-trie) true)

;; 2(d)

;; (tnode-lon tlon clon-so-far lon-so-far listed?) goes through all children of
;; a trie (denoted by "tlon") and uses the parameters of clon-so-far and 
;; lon-so-far to measure (listof Char) and (listof Str) respectively, while 
;; using listed? to check if a word has been listed before
;; tnode-lon: (listof TNode) (listof Char) (listof Str) Bool -> (listof Str)
;; Examples:
(check-expect (tnode-lon empty empty empty false) empty)
(check-expect (tnode-lon (trie-children a-c-trie) empty empty false)
              (list "at" "coo" "cow" "cs115" "cs116" "cs135" "cs136"))
               
(define (tnode-lon tlon clon-so-far lon-so-far listed?)
  (cond
    [(empty? tlon) lon-so-far]
    [(empty? (rest tlon)) 
     (append (build-word (first tlon) clon-so-far lon-so-far) empty)]
    [listed? (append (build-word (first tlon) clon-so-far lon-so-far)
                     (tnode-lon (rest tlon) clon-so-far empty true))]
    [else (append (build-word (first tlon) clon-so-far lon-so-far)
                  (tnode-lon (rest tlon) clon-so-far lon-so-far false))]))

;; (build-word tlon clon-so-far lon-so-far) produces list of words in a TNode,
;; (denoted by "tlon") and uses clon-so-far and lon-so-far as accumulators
;; to measure (listof Char) and (listof Str) respectively 
;; build-word: TNode (listof Char) (listof Str) -> (listof Str)
;; Examples:
(check-expect (build-word (first (trie-children a-c-trie)) empty empty)
              (list "at"))
(check-expect (build-word (first (trie-children c-d-trie)) empty empty)
              (list "cat" "catch" "cater" "catnip" "cattle"))

(define (build-word tlon clon-so-far lon-so-far)
  (cond
    [(and (tnode-ends-word? tlon) (cons? (tnode-children tlon)))
     (tnode-lon (tnode-children tlon)
                (append clon-so-far (list (tnode-key tlon)))
                (append lon-so-far
                        (list (list->string (append clon-so-far
                                                    (list (tnode-key tlon))))))
                true)]
    [(tnode-ends-word? tlon)
     (tnode-lon empty empty
                (append lon-so-far
                        (list (list->string (append clon-so-far
                                                    (list (tnode-key tlon))))))
                false)]
    [else (tnode-lon (tnode-children tlon)
                     (append clon-so-far (list (tnode-key tlon))) lon-so-far
                     false)]))
                        
;; (list-words trie1) consumes a Trie (denoted by "trie1") and produces
;; an alphabetically-sorted list of all words in trie1
;; list-words: Trie -> (listof Str)
;; Examples:
(check-expect (list-words a-c-trie)
              (list "at" "coo" "cow" "cs115" "cs116" "cs135" "cs136"))
(check-expect (list-words blank-trie) empty)

(define (list-words trie1)
  (tnode-lon (trie-children trie1) empty empty false))

;; Tests:
(check-expect (list-words h-u-trie)
              (list "ha" "hat" "he" "hot" "use"))
(check-expect (list-words c-d-trie)
              (list "cat" "catch" "cater" "catnip" "cattle" "dig" "dog"
                    "dogfish" "donald" "donut" "doze"))

;; 2(e)

;; (insert-lon clon-so-far tlon) takes (listof Char) (denoted by "clon-so-far")
;; and (listof TNode) (denoted by "tlon") to insert clon-so-far values into tlon
;; insert-lon: (listof Char) (listof TNode) -> (listof TNode)
;; Examples:
(check-expect (insert-lon empty (trie-children a-c-trie))
              (trie-children a-c-trie))
(check-expect (insert-lon (string->list "h") (trie-children blank-trie))
              (list (make-tnode #\h #true '())))

(define (insert-lon clon-so-far tlon)
  (cond
    [(empty? clon-so-far) tlon]
    [(empty? tlon) (list (build-a-word clon-so-far tlon))]
    [(char=? (tnode-key (first tlon)) (first clon-so-far))
     (cons (insert-clon clon-so-far (first tlon)) (rest tlon))]
    [(char>? (tnode-key (first tlon)) (first clon-so-far))
     (cons (build-a-word clon-so-far (rest tlon)) tlon)]
    [else (cons (first tlon) (insert-lon clon-so-far (rest tlon)))]))

;; (insert-clon clon-so-far tlon) takes (listof Char) (denoted by "clon-so-far")
;; and a TNode (denoted by "tlon") and produces a TNode from clon-so-far,
;; going through the pre-existing tlon
;; insert-clon: (listof Char) TNode -> TNode
;; Examples:
(check-expect (insert-clon (string->list "hi") (first (trie-children a-c-trie)))
              (make-tnode #\h #false (list (make-tnode #\i #true '())
                                           (make-tnode #\t #true '()))))
(check-expect (insert-clon (string->list "goose")
                           (first (trie-children a-c-trie)))
              (make-tnode #\g #false
                          (list
                           (make-tnode #\o #false
                                       (list
                                        (make-tnode
                                         #\o #false
                                         (list
                                          (make-tnode #\s #false
                                                      (list
                                                       (make-tnode
                                                        #\e #true
                                                        '())))))))
                           (make-tnode #\t #true '()))))

(define (insert-clon clon-so-far tlon)
  (make-tnode (first clon-so-far)
              (cond
                [(or (tnode-ends-word? tlon) (empty? (rest clon-so-far))) true]
                [else false])
              (insert-lon (rest clon-so-far) (tnode-children tlon))))

;; (build-a-word clon-so-far tlon) takes (listof Char) (denoted by
;; "clon-so-far") and (listof TNode) (denoted by "tlon) to produce a TNode
;; that contains the chars from clon-so-far in the appropriate order
;; build-a-word: (listof Char) (listof TNode) -> TNode
;; Examples:
(check-expect (build-a-word (string->list "hi") a-c-trie)
              (make-tnode #\h #false (list (make-tnode #\i #true '()))))
(check-expect (build-a-word (string->list "goose") a-c-trie)
              (make-tnode #\g #false
                          (list
                           (make-tnode #\o #false
                                       (list
                                        (make-tnode #\o #false
                                                    (list
                                                     (make-tnode #\s #false
                                                                 (list
                                                                  (make-tnode
                                                                   #\e #true
                                                                   '()))))))))))
(define (build-a-word clon-so-far tlon)
  (cond
    [(empty? (rest clon-so-far)) (make-tnode (first clon-so-far) true empty)]
    [else (make-tnode (first clon-so-far) false
                      (list (build-a-word (rest clon-so-far) tlon)))]))

;; (insert-word word trie1) consumes a word (denoted by "word") and inserts
;; that word into a trie (denoted by "trie1")
;; insert-word: Str Trie -> Trie
;; Examples:
(check-expect (list-words (insert-word "hated" h-u-trie))
              (list "ha" "hat" "hated" "he" "hot" "use"))
(check-expect (list-words (insert-word "feridun" empty))
              (list "feridun"))

(define (insert-word word trie1)
  (cond
    [(empty? trie1) (make-trie (list (build-a-word (string->list word) empty)))]
    [else
     (make-trie (insert-lon (string->list word) (trie-children trie1)))]))

;; Tests:
(check-expect (list-words (insert-word "ho" h-u-trie))
              (list "ha" "hat" "he" "ho" "hot" "use"))
(check-expect (list-words (insert-word "him" h-u-trie))
              (list "ha" "hat" "he" "him" "hot" "use"))

;; 2(f)

;; (insert-some-words slon trie1) inserts strings from a (listof Str)
;; (denoted by "slon") into a Trie (denoted by "trie1")
;; insert-some-words: (listof Str) Trie -> Trie
;; Examples:
(check-expect (list-words (insert-some-words (list "hog" "hoot") h-u-trie))
              (list "ha" "hat" "he" "hog" "hoot" "hot" "use"))
(check-expect (insert-some-words empty a-c-trie) a-c-trie)

(define (insert-some-words slon trie1)
  (cond [(empty? slon) trie1]
        [else (insert-some-words (rest slon) (insert-word (first slon) trie1))]
        ))

;; Tests:
(check-expect (list-words (insert-some-words (list "cs126" "cs127") c-d-trie))
             (list "cat" "catch" "cater" "catnip" "cattle" "cs126" "cs127"
                   "dig" "dog" "dogfish" "donald" "donut" "doze"))
(check-expect (list-words (insert-some-words (list "attention" "hey") a-c-trie))
              (list "at" "attention" "coo" "cow" "cs115" "cs116" "cs135"
                    "cs136" "hey"))

;; 2(g)

;; (list-check clon-so-far word) determines if (listof Char) (denoted by
;; "clon-so-far") contains the given (listof Char) (denoted by "word")
;; list-check: (listof Char) (listofChar) -> Bool
;; Examples:
(check-expect (list-check empty empty) true)
(check-expect (list-check (string->list "hi") empty) false)
(check-expect (list-check (string->list "hi") (string->list "high")) true)

(define (list-check clon-so-far word)
  (cond
    [(empty? clon-so-far) true]
    [(empty? word) false]
    [(char=? (first clon-so-far) (first word))
     (list-check (rest clon-so-far) (rest word))]
    [else false]))

;; (list-compiler clon-so-far word) takes (listof Char) (denoted by
;; "clon-so-far") and (listof Str) (denoted by "word") in a Trie
;; to display all words in Trie that start with the same (listof Char) as
;; clon-so-far
;; list-compiler: (listof Char) (listof Str) -> (listof Str)
;; Examples:
(check-expect (list-compiler (string->list "don") (list-words c-d-trie))
              (list "donald" "donut"))
(check-expect (list-compiler (string->list "") (list-words h-u-trie))
              (list "ha" "hat" "he" "hot" "use"))
(check-expect (list-compiler empty empty) empty)

(define (list-compiler clon-so-far word)
  (cond
    [(empty? word) empty]
    [(empty? clon-so-far) word]
    [(list-check clon-so-far (string->list (first word)))
     (append (list (first word))
             (list-compiler clon-so-far (rest word)))]
    [else (list-compiler clon-so-far (rest word))]))
                   

;; (list-completions word trie1) consumes a string (denoted by "word") and
;; produces a sorted list of strings consisting of all words in the Trie
;; (denoted by "trie1") that begin with that prefix
;; list-completions: Str Trie -> (listof Str)
;; Examples:
(check-expect (list-completions "don" c-d-trie) (list "donald" "donut"))
(check-expect (list-completions "" h-u-trie) (list "ha" "hat" "he" "hot" "use"))

(define (list-completions word trie1)
  (list-compiler (string->list word) (list-words trie1)))

;; Tests:
(check-expect (list-completions "ha" h-u-trie) (list "ha" "hat"))
(check-expect (list-completions "go" h-u-trie) empty)
  




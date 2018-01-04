;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname pronunciation) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;; ***************************************************
;;   Pronoy Chaudhuri (20709909)
;;   CS 135 Fall 2017
;;   Assignment 05, Problem 2
;; ***************************************************
;;

;; Ask Racket to give us access to the code in this file.
;; Do not remove this line.
(require "pronunciationlib.rkt")

;; The data definitions as given in the question.

;; A Vowel is a (list Sym (anyof 0 1 2))

;; A Phoneme is an (anyof Sym Vowel)

;; A Pronunciation is a (listof Phoneme)
;; requires: the list contains exactly one vowel with a stress of 1

;; A Dictionary is a (listof (list Str Pronunciation))
;; requires: The strings in each sub-list appear in alphabetical
;;           order in the Dictionary.

;; 2(a)

;; (num-counter word list-pronoun) consumes a word and Pronounciation
;; to determine how many syllables are in it
;; num-counter: Str Pronunciation -> Nat
;; Examples:
(check-expect (num-counter "describe" '(D (IH 0) S K R (AY 1) B)) 2)
(define (num-counter word list-pronun)
  (cond
    [(empty? list-pronun) 0]
    [(cons? (first list-pronun)) (+ 1 (num-counter word (rest list-pronun)))]
    [else (num-counter word (rest list-pronun))]))  
  

;; (num-syllables word dictionary) consumes a word and dictionary
;; to tell the numbers of syllables in the word
;; num-syllables: Str Dictionary -> Nat
;; Examples:
(check-expect (num-syllables "hello" empty) 0)
(check-expect (num-syllables "significantly" toy-dictionary) 5)

(define (num-syllables word dictionary)
  (cond
    [(empty? dictionary) 0]
    [(eq? word (first (first dictionary)))
     (num-counter word (first (rest (first dictionary))))]
    [else (num-syllables word (rest dictionary))]))

;; Tests:
(check-expect (num-syllables "google" toy-dictionary) 0)
(check-expect (num-syllables "describe" toy-dictionary) 2)

;; 2(b)

;; (types-of-syllable pronunciation-list) take a Pronunciation and
;; creates a list of Nums that show stressed and unstressed syllables
;; types-of-syllable: Pronunciation -> (listof Num)
;; Examples:
(check-expect (types-of-syllable '(D (IH 0) S K R (AY 1) B)) (list 0 1))

(define (types-of-syllable pronunciation-list)
  (cond
    [(empty? pronunciation-list) empty]
    [(cons? (first pronunciation-list))
     (cons (second (first pronunciation-list))
           (types-of-syllable (rest pronunciation-list)))]
    [else (types-of-syllable (rest pronunciation-list))]))

;; (find-stress-pattern list-nat dictionary) finds words in
;; dictionary with a stress pattern by list-nat
;; find-stress-pattern (listof Nat) Dictionary -> (listof Str)
;; Examples:
(check-expect (find-stress-pattern '(0 1) toy-dictionary)
              (list "adopt" "concrete" "deprive" "describe" "petite"))
(check-expect (find-stress-pattern '(0 1) empty) empty)

(define (find-stress-pattern listof-nat dictionary)
  (cond
    [(empty? dictionary) empty]
    [(equal? listof-nat (types-of-syllable (second (first dictionary))))
          (cons (first (first dictionary))
                (find-stress-pattern listof-nat (rest dictionary)))]
    [else (find-stress-pattern listof-nat (rest dictionary))]))

;; Tests:
(check-expect (find-stress-pattern '(1 0) toy-dictionary)
              (list "actress" "awful" "billion" "smugly" "ugly"))
(check-expect (find-stress-pattern '(1) toy-dictionary)
              (list "blurt" "cold" "cook" "dirt" "fault" "five" "swapped"
                    "wake"))

;; 2(c)

;; (find-word word dictionary) find Pronunciation of word
;; find-word: Str Dictionary -> Pronunciation
;; Examples:
(check-expect (find-word "describe" toy-dictionary) '(D (IH 0) S K R (AY 1) B))

(define (find-word word dictionary)
  (cond
    [(empty? dictionary) empty]
    [(string=? word (first (first dictionary)))
     (second (first dictionary))]
    [else (find-word word (rest dictionary))]))

;; (end-suffix pronunciation-list) consumes a pronunciation, producing
;; all syllables after primary stress
;; end-suffix: Pronunciation -> Pronunciation
;; Examples:
(check-expect (end-suffix (find-word "describe" toy-dictionary))
              '((AY 1) B))

(define (end-suffix pronunciation-list)
  (cond
    [(empty? pronunciation-list) empty]
    [(and (cons? (first pronunciation-list))
      (= 1 (second (first pronunciation-list))))
     pronunciation-list]
    [else (end-suffix (rest pronunciation-list))]))

;; (fr-precheck word dictionary pronunciation-list) consumes
;; a word, dictionary, and pronounciation-list for a word
;; to find rhyming words
;; fr-precheck: Str Dictionary Pronunciation -> (listof Str)
;; Examples:
(check-expect (fr-precheck "ugly" toy-dictionary
                           (find-word "ugly" toy-dictionary))
              (list "smugly"))

(define (fr-precheck word dictionary pronunciation-suffix)
  (cond
    [(empty? dictionary) empty]
    [(equal? pronunciation-suffix (end-suffix (second (first dictionary))))
     (cond
       [(string=? word (first (first dictionary)))
        (fr-precheck word (rest dictionary) pronunciation-suffix)]
       [else (cons (first (first dictionary))
                   (fr-precheck word (rest dictionary) pronunciation-suffix))])]
    [else (fr-precheck word (rest dictionary) pronunciation-suffix)]))


;; (find-rhymes word dictionary) consumes a word and dictionary to find suffix
;; and compares suffix to other suffixes in dictionary to determine which match
;; find-rhymes: Str Dictionary -> (listof Str)
;; Examples:
(check-expect (find-rhymes "ugly" toy-dictionary)
               (list "smugly"))
(check-expect (find-rhymes "google" empty) empty)

(define (find-rhymes word dictionary)
  (fr-precheck word dictionary (end-suffix (find-word word dictionary))))

;; Tests:
(check-expect (find-rhymes "five" toy-dictionary)
               (list "deprive"))
(check-expect (find-rhymes "swapped" toy-dictionary)
               (list "adopt"))
(check-expect (find-rhymes "petite" toy-dictionary)
               (list "concrete"))

    
    
  
    
    
  


    
  

          
  

  
  

  
  
  
  

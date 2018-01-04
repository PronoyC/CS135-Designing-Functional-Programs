;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname suggest) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; OPTIONAL -- spellcheck.rkt provides the following function:
;;
;; (spellcheck? s) determines if s is spelled correctly
;;   (according to a medium-sized wordlist)
;; spellcheck: Str -> Bool
;;
;; You may use this function for your own experiments
;; (and to show off your program to your friends & family)

;; Do NOT leave this require in your file when you submit your code.
;; (require "spellcheck.rkt")
;; [this file will be available after the A07 deadline]
;; NOTE: You do not need to open spellcheck.rkt in DrRacket to use it
;;       (opening the file in DrRacket may slow down your computer).


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; ***************************************************
;;   Pronoy Chaudhuri (20709909)
;;   CS 135 Fall 2017
;;   Assignment 08, Problem 4
;; ***************************************************
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; NOTE: you should complete the documentation & tests (design recipe)
;; for all functions (except remove-at and remove-letters)
;; But remember, because most of your functions will not have a cond
;; or much logic, exhaustive tests may not be required

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A Word is a Str
;; requires: only lowercase letters appear in the word
;;           (no spaces, punctuation, etc.)

(define letters (string->list "abcdefghijklmnopqrstuvwxyz"))

;; (my-word s) converts a Word (denoted by "s") into a (listof Char)
;; myword: Word -> (listof Char)
;; Examples:
(check-expect (my-word "hi") (list #\h #\i))
(check-expect (my-word "racket") (list #\r #\a #\c #\k #\e #\t))

(define (my-word s) (string->list s))

;; 4a ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (remove-dups slst) removes any duplicate elements within (listof Word)
;; (denoted by "slst")
;; remove-dups: (listof Word) -> (listof Word)
;; requires: slst is sorted in non-decreasing order
;; Examples:
(check-expect (remove-dups empty) empty)
(check-expect (remove-dups
               '("apple" "apple" "apples" "banana" "cherry" "cherry"))
              '("apple" "apples" "banana" "cherry"))

(define (remove-dups slst)
  (foldr (lambda (x y) (cond
                         [(empty? y) (cons x empty)]
                         [(equal? x (first y)) (cons (first y) (rest y))]
                         [else (cons x y)])) empty slst))

;; Tests:
(check-expect (remove-dups '("feridun" "feridun" "i" "love" "mrgoose"))
              '("feridun" "i" "love" "mrgoose"))
(check-expect (remove-dups '("cs" "cs" "cs" "is" "the" "zenith"))
              '("cs" "is" "the" "zenith"))

;; 4b ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Constants:
(define starting-value 0)

;; (ifoldr combine base lst) combines the first list item (list denoted by
;; "lst") with the result of reducing the rest of the list (combine function
;; denoted by "combine" and base result denoted by "base)
;; ifoldr: (Nat X Y -> Y) Y (listof X) -> Y
;; Examples:
(check-expect (ifoldr (lambda (i x y) (cons (list i x) y)) empty '(a b c))
              '((0 a) (1 b) (2 c)))
(check-expect (ifoldr + starting-value empty) 0)

(define (ifoldr combine base lst)
  (local
    ;; (my-foldr index lon) increases the value of index everytime my-foldr
    ;; reduces the given list (denoted by "lon")
    ;; my-foldr: Nat (listof Any) -> (listof Any)
    [(define (my-foldr index combine base lon)
       (cond
         [(empty? lon) base]
         [else (combine index (first lon)
                        (my-foldr (add1 index) combine base (rest lon)))]))]
    (my-foldr starting-value combine base lst)))

 
;; Tests: ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (remove-at i lst) removes element with index i from lst
;; remove-at: Nat (listof Any) -> (listof Any)
;; Examples:
(check-expect (remove-at 0 '(a b c d)) '(b c d))
(check-expect (remove-at 3 '(a b c d)) '(a b c))
(check-expect (remove-at 0 '()) '())

(define (remove-at i lst)
  (ifoldr (lambda (k x y)
            (cond [(= i k) y]
                  [else (cons x y)]))
          empty lst))


;; (remove-letters s) produces a list of Words,
;; each with one letter removed from s
;; remove-letters: Word -> (listof Word)
;; Examples:
(check-expect (remove-letters "abc") '("bc" "ac" "ab"))
(check-expect (remove-letters "") '())

(define (remove-letters s)
  (local [(define loc (string->list s))]
    (build-list (length loc) (lambda (i) (list->string (remove-at i loc))))))

;; 4c ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (insert-letters s) inserts the characters of the alphabet
;; before each letter of the chosen Word (denoted by "s") 
;; insert-letters: Word -> (listof Word)
;; Examples:
(check-expect (insert-letters "hi")
              (list "ahi" "bhi" "chi" "dhi" "ehi" "fhi" "ghi" "hhi" "ihi" "jhi"
                    "khi" "lhi" "mhi" "nhi" "ohi" "phi" "qhi" "rhi" "shi" "thi"
                    "uhi" "vhi" "whi" "xhi" "yhi" "zhi" "hai" "hbi" "hci" "hdi"
                    "hei" "hfi" "hgi" "hhi" "hii" "hji" "hki" "hli" "hmi" "hni"
                    "hoi" "hpi" "hqi" "hri" "hsi" "hti" "hui" "hvi" "hwi" "hxi"
                    "hyi" "hzi"))
(check-expect (insert-letters "") empty)

(define (insert-letters s)
  (local
    ;; (insert-letters-compiler index word) inserts the characters of the
    ;; alphabet before a letter of the chosen Word (denoted by "word")
    ;; insert-letters-compiler: Nat (listof Char) -> (listof Str)
    [(define (insert-letters-compiler index word)
       (foldr
        (lambda (x y) (cons (insert-all-char index x word) y)) empty letters)) 
     ;; (insert-all-char index char1 lon) uses index to appropriately place an
     ;; alphabet letter (denoted by "char1") before a letter of the chosen Word
     ;; (denoted by "word")
     ;; insert-all-char: Nat Char (listof Char) -> (listof Str) 
     (define (insert-all-char index char1 word)
       (list->string
        (ifoldr (lambda (i x y)
                  (cond
                    [(equal? index i) (append (cons char1 (list x)) y)]
                    [else (cons x y)])) empty word)))]
    (ifoldr (lambda (i x y) (append (insert-letters-compiler i (my-word s)) y))
            empty (my-word s))))

;; (trailing-letters s) inserts each character of the alphabet
;; behind the chosen Word (denoted by "s"), one character at a time only,
;; in sorted order
;; trailing-letters: Word -> (listof Word)
;; Examples:
(check-expect (trailing-letters "jklm")
              (list "jklma" "jklmb" "jklmc" "jklmd" "jklme" "jklmf" "jklmg"
                    "jklmh" "jklmi" "jklmj" "jklmk" "jklml" "jklmm" "jklmn"
                    "jklmo" "jklmp" "jklmq" "jklmr" "jklms" "jklmt" "jklmu"
                    "jklmv" "jklmw" "jklmx" "jklmy" "jklmz"))
(check-expect (trailing-letters "")
              (list "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o"
                    "p" "q" "r" "s" "t" "u" "v" "w" "x" "y" "z"))

(define (trailing-letters s)
  (foldr (lambda (x y)
           (cons
            (list->string (append (my-word s) (list x))) y)) empty letters))


;; (replace-letters s) substitutes the characters of the alphabet
;; as each letter of a given word (denoted by "s")
;; replace-letters: Word -> (listof Word)
;; Examples:
(check-expect (replace-letters "hi")
              (list "ai" "bi" "ci" "di" "ei" "fi" "gi" "hi" "ii" "ji" "ki" "li"
                    "mi" "ni" "oi" "pi" "qi" "ri" "si" "ti" "ui" "vi" "wi" "xi"
                    "yi" "zi" "ha" "hb" "hc" "hd" "he" "hf" "hg" "hh" "hi" "hj"
                    "hk" "hl" "hm" "hn" "ho" "hp" "hq" "hr" "hs" "ht" "hu" "hv"
                    "hw" "hx" "hy" "hz"))
(check-expect (replace-letters "") empty)

(define (replace-letters s)
  (local
    ;; (replace-letters-compiler index word) substitutes the characters of the
    ;; alphabet as a letter of the chosen Word (denoted by "word")
    ;; replace-letters-compiler: Nat (listof Char) -> (listof Str)
    [(define (replace-letters-compiler index word)
       (foldr
        (lambda (x y) (cons (replace-all-char index x word) y)) empty letters))
     ;; (replace-all-char index char1 lon) uses index to appropriately replace
     ;; an alphabet letter (denoted by "char1") as a letter of the chosen Word
     ;; (denoted by "word")
     ;; replace-all-char: Nat Char (listof Char) -> (listof Str) 
     (define (replace-all-char index char1 word)
       (list->string
        (ifoldr (lambda (i x y)
                  (cond
                    [(equal? index i) (cons char1 y)]
                    [else (cons x y)])) empty word)))]
    (ifoldr (lambda (i x y) (append (replace-letters-compiler i (my-word s)) y))
            empty (my-word s))))

;; (swap-letters s) swaps adjacent pairs of letters in a given Word
;; (denoted by "s")
;; swap-letters: Word -> (listof Word)
;; Examples:
(check-expect (swap-letters "jklm") (list "kjlm" "jlkm" "jkml"))
(check-expect (swap-letters " ") empty)

(define (swap-letters s)
  (local
    ;; (swap-letters-compiler index word) swaps an adjacent pair of letters
    ;; in a given Word (denoted by "word")
    ;; swap-letters-compiler: Nat (listof Char) -> (listof Str)
    [(define (swap-letters-compiler index word)
       (list->string (ifoldr
                      (lambda (i x y)
                        (cond
                          [(equal? index i)
                           (append (cons (swap-all-char (add1 index) word)
                                         (cons (swap-all-char index word) y)))]
                          [(equal? (add1 index) i) y]
                          [else (cons x y)])) empty word)))
     ;; (swap-all-char index char1 lon) uses index to appropriately choose char
     ;; that would mimic a swap on an adjacent pair of letters from the chosen
     ;; Word (denoted by "word")
     ;; swap-all-char: Nat (listof Char) -> Char 
     (define (swap-all-char index word)
       (ifoldr (lambda (i x y)
                 (cond
                   [(equal? index i) x]
                   [else y])) empty word))]
    (build-list (sub1 (length (my-word s)))
                (lambda (x) (swap-letters-compiler x (my-word s))))))
                                                         
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (suggest s valid?) generates all Words with an “Edit Distance”
;; of one (including transpositions/swaps) and uses valid? to
;; determine if it is a valid suggestion
;; suggest: Word (Word -> Bool) -> (listof Word)
;; Examples:
(check-expect (suggest "right" valid?) '("bright" "fight" "rights"))
(check-expect (suggest " " valid?) empty)

(define (suggest s valid?)
  (local [(define words (append (remove-letters s)
                                (insert-letters s)
                                (trailing-letters s)
                                (replace-letters s)
                                (swap-letters s)))

          (define valid-words (filter valid? words))

          (define legal-words (filter (lambda (x) (and (not (string=? s x))
                                                       (not (string=? x ""))))
                                      valid-words))

          (define clean-words (remove-dups (sort legal-words string<=?)))]

    clean-words))

;; Tests:
(check-expect (suggest "light" valid?) '("fight" "right"))
(check-expect (suggest "rights" valid?) '("right"))

;; Functions Created:
(define (valid? s)
  (member? s '("rights" "right" "fight" "aardvark" "fhqwhgads" "bright")))

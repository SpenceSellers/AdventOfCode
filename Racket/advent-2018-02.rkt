#lang racket
(require "lib.rkt")

(define test-input-1
  '( "abcdef"
     "bababc"
     "abbcde"
     "abcccd"
     "aabcdd"
     "abcdee"
     "ababab"))

(define test-input-2
  (map symbol->string
       '(abcde
         fghij
         klmno
         pqrst
         fguij
         axcye
         wvxyz)))

(define (has-count n str)
  (contains? n (hash-values (count-items str))))

(define (problem1 input)
  (define (num-with n)
    (count (curry has-count n) input))

  (* (num-with 2) (num-with 3)))

(define (commonality a b)
  (list->string
   (for/list ([ai a]
              [bi b]
              #:when (equal? ai bi))
     ai)))

(define (problem2 input)
  (define (differs-by-one? a b) (equal? (string-length (commonality a b)) (sub1 (string-length a))))
  (for*/first ([a input]
               [b input]
               #:when (differs-by-one? a b))
    (commonality a b)))

(let ([puzzle (puzzle-input "02")])
  (displayln (problem1 puzzle))
  (displayln (problem2 puzzle)))

  
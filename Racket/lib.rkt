#lang racket

(provide (all-defined-out))

(define (puzzle-input puzzle-num-str)
  (file->lines (string-append "inputs/" puzzle-num-str ".txt")))

;; HASH count functions

(define (hash-add-count hash k)
  (hash-set hash k (add1 (hash-ref hash k 0))))

(define (hash-get-count hash k)
  (hash-ref hash k 0))

(define (count-items seq)
  (for/fold ([counts (hash)])
            ([i seq])
    (hash-add-count counts i)))

;; Other functions

(define (contains? item seq)
  (for/or ([i seq]) (equal? i item)))
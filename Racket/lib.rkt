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

(define (hash-map-assocs grid f)
  (make-hash (hash-map grid f)))

(define (hash-map-values grid f)
  (hash-map-assocs grid (λ (k v) (cons k (f v)))))

;; Grid
(struct point (x y)
  #:transparent)

(define (grid-build width height proc)
  (define grid (make-hash))
  (for* ([x width] [y height])
    (let ([p (point x y)]) (hash-set! grid p (proc p))))
  grid)

;(define (grid-set grid point value)
  




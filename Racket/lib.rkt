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

; Returns `(min-inclusive max-exclusive)
(define (grid-bounding-box grid)
  (define points (hash-keys grid))
  (define xs (map point-x points))
  (define ys (map point-y points))
  (list (point (apply min xs) (apply min ys)) (point (add1 (apply max xs)) (add1 (apply max ys)))))

(define (points-in-bounding-box min max)
  (for*/list ([x (in-range (point-x min) (point-x max))]
         [y (in-range (point-y min) (point-y max))])
    (point x y)))

(define (show-grid-default item)
  (cond
    [(symbol? item) (symbol->string item)]
    [(string? item) item]
    [(eq? #f item) "."]
    [else "?"]))
  

(define (show-grid grid [disp show-grid-default] [empty-value #f])
  (match-define (list (point minx miny) (point maxx maxy)) (grid-bounding-box grid))
  (define (row y) (string-join (for/list ([x (in-range minx maxx)]) (disp (hash-ref grid (point x y) empty-value))) ""))
  (string-join (for/list ([y (in-range miny maxy)]) (row y)) "\n"))



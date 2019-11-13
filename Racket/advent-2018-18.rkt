#lang racket
(require "lib.rkt")


(define (char->cell char)
  (match char
    [#\. 'ground]
    [#\| 'trees]
    [#\# 'lumberyard]))

(define input (puzzle-input "18-18"))
(define board (map (λ (row) (map char->cell row)) (map string->list input)))

(define world (make-hash))
(for* ([y (length board)]
       [x (length (car board))])
  (define value (list-ref (list-ref board y) x))
  (dict-set! world (cons x y) value))


(define (adjacents pos)
  (filter (λ (pos) (and (>= (car pos) 0) (>= (cdr pos) 0)))
  (for*/list ([dx `(-1 0 1)]
             [dy `(-1 0 1)]
             #:when (not (and (= dx 0) (= dy 0))))
    (cons (+ dx (car pos))
          (+ dy (cdr pos))))))

(define (tick-cell cell adjacents)
  (define counts (count-items adjacents))
  (define (near many what) (>= (hash-get-count counts what) many))
  (match cell
    ['ground
     #:when (near 3 'trees)
     'trees]
    ['ground 'ground]
    ['trees
     #:when (near 3 'lumberyard)
     'lumberyard]
    ['trees 'trees]
    ['lumberyard
     #:when (and (near 1 'lumberyard) (near 1 'trees))
     'lumberyard]
    ['lumberyard 'ground]))

(define (adjacent-to board pos)
  (define valid (filter (λ (p) (dict-has-key? board p)) (adjacents pos)))
  (map (curry dict-ref board) valid))

(define (tick board)
  (make-hash (dict-map board (λ (pos val)
                    (cons pos (tick-cell val (adjacent-to board pos)))))))

(define (resource-value board)
  (define counts (count-items (dict-values board)))
  (* (hash-get-count counts 'lumberyard) (hash-get-count counts 'trees)))

(define (composen f n)
  (lambda (initial)
    (for/fold ([res initial])
                ([i n])
      (f res))))

; Part 1
(resource-value ((composen tick 10) world))

; Part 2 (To analyze)
(for/fold ([w world])
          ([i 10000])
  (displayln i)
  (displayln (resource-value w))
  (tick w))
#lang racket
(require racket/match)
(require "lib.rkt")

(define line1 "x=495, y=2..7")
(define line2 "y=7, x=495..501")
(define input-lines (list line1 line2))

(define (parse-line line)
  (read (open-input-string (format "(~a)" (string-replace line #rx"(=|, |\\.)" " ")))))

(define (points line)
  (match line
    [(list 'x x 'y ymin ymax) (for/list ([y (in-range ymin (add1 ymax))]) (point x y))]
    [(list 'y y 'x xmin xmax) (for/list ([x (in-range xmin (add1 xmax))]) (point x y))]))

(for/fold ([grid (hash)])
          ([point (flatten (map (compose points parse-line) input-lines))])
  (hash-set grid point 'x))
           





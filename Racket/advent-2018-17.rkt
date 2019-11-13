#lang racket
(require racket/match)
(require "lib.rkt")

(define input-lines (puzzle-input "18-17-sample"))

(define (parse-line line)
  (read (open-input-string (format "(~a)" (string-replace line #rx"(=|, |\\.)" " ")))))

(define (points line)
  (match line
    [(list 'x x 'y ymin ymax) (for/list ([y (in-range ymin (add1 ymax))]) (point x y))]
    [(list 'y y 'x xmin xmax) (for/list ([x (in-range xmin (add1 xmax))]) (point x y))]))

(define grid (for/fold ([grid (hash)])
          ([point (flatten (map (compose points parse-line) input-lines))])
  (hash-set grid point 'x)))

(set! grid (hash-set grid (point 500 0) 's))

(display (show-grid grid))




           





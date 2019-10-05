#lang racket
(require racket/set)

(define raw-input (file->lines "inputs/1.txt"))
(define input (map string->number raw-input))

; Problem 1
(displayln (apply + input))


; Problem 2
(define (problem2 freqs)
  (define (inner current-freq seen freqs)
    (cond
      [(set-member? seen current-freq) current-freq]
      [else (inner (+ (stream-first freqs) current-freq)
                   (set-add seen current-freq)
                   (stream-rest freqs))]))
  (inner 0 (set) (sequence->stream (in-cycle freqs))))

(problem2 input)
#lang racket
(require "lib.rkt")

; Part 1
(define (module->mass module) (- (quotient module 3) 2))

(apply + (map (compose module->mass string->number) (puzzle-input "19-01")))

; Part 2
(define (mass->fuel mass)
  (define additional-fuel (module->mass mass))
  (cond
    [(<= additional-fuel 0) mass]
    [else (+ mass (mass->fuel additional-fuel))]))

(apply + (map (compose mass->fuel module->mass string->number) (puzzle-input "19-01")))
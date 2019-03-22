#lang racket

(require "lib.rkt")

(provide
 (all-from-out "lib.rkt")
 (contract-out
  [1+ (complex? . -> . complex?)]))

(define (1+ c)
  (define one (complex 1 #f))
  (complex+ c one))

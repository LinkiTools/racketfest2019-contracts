#lang racket

(struct complex (r i)
  #:transparent)

(define/contract (make-complex r i)
  (real? real? . -> . complex?)
  (complex r i))

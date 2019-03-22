#lang racket

(provide
  (contract-out
    [complex (real? real? . -> . complex?)]
    [complex? (any/c . -> . boolean?)]
    [complex+ (complex? ... . -> . complex?)]))

(struct complex (r i)
  #:transparent)

(define (binary-complex+ c1 c2)
  (complex (+ (complex-r c1) (complex-r c2))
   	   (+ (complex-i c1) (complex-i c2))))

(define (complex+ . cs)
  (for/fold ([r (complex 0 0)])
            ([c (in-list cs)])
    (binary-complex+ r c)))

#lang racket

(define/contract (sum-list xs)
  ((listof integer?) . -> . integer?)
  (define total
    (for/fold ([s 0])
              ([x (in-list xs)])
      (+ s x)))
  (if (> total 100)
      'buh
      total))

(contract-exercise sum-list)

;; Can you write a function f that breaks its contract but for which a call
;; to (contract-exercise f) can't break it?

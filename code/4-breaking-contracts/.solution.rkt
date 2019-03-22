#lang racket

(define/contract (sum-booleans . bs)
  (->* () #:rest (listof boolean?)  integer?)
  (define v
    (for/fold ([r 0])
              ([b (in-list bs)])
      (+ r (if b 1 0))))
  ;; contract is broken is bs has even number of members and exactly half
  ;; are true
  (if (and (even? (length bs)) (= (/ (length bs) 2) v))
      'hurray
      v))

(contract-exercise sum-booleans)

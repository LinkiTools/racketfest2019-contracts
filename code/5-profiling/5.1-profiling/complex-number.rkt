#lang racket/base
;; ---------------------------------------------------------------------------------------------------

(require racket/contract
         racket/match)

(provide
 (contract-out
  [unit complex?]
  [zero complex?]
  [complex (real? real? . -> . complex?)]
  [complex+ (complex? complex? ... . -> . complex?)]
  [complex- (complex? complex? ... . -> . complex?)]
  [complex* (complex? complex? ... . -> . complex?)]
  [complex-square (complex? . -> . complex?)]
  [complex-mag (complex? . -> . real?)]))

;; ---------------------------------------------------------------------------------------------------

(struct complex (r i)
  #:transparent)

(define unit (complex 1 0))
(define zero (complex 0 0))

(define (bin-complex+ c1 c2)
  (complex (+ (complex-r c1) (complex-r c2))
           (+ (complex-i c1) (complex-i c2))))

(define (complex+ c . cs)
  (for/fold ([r c])
            ([v (in-list cs)])
    (bin-complex+ r v)))

(define (bin-complex- c1 c2)
  (complex (- (complex-r c1) (complex-r c2))
           (- (complex-i c1) (complex-i c2))))

(define (complex- c . cs)
  (for/fold ([r c])
            ([v (in-list cs)])
    (bin-complex- r v)))

(define (bin-complex* c1 c2)
  (match-define (struct complex (x y)) c1)
  (match-define (struct complex (u v)) c2)

  (complex (- (* x u) (* y v))
           (+ (* x v) (* y u))))

(define (complex* c . cs)
  (for/fold ([r c])
            ([v (in-list cs)])
    (bin-complex* r v)))

(define (complex-square c)
  (complex* c c))

(define (complex-mag c)
  (match-define (struct complex (x y)) c)
  (sqrt (+ (* x x) (* y y))))

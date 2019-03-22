#lang racket/base
;; ---------------------------------------------------------------------------------------------------

(require racket/contract
         racket/list
         racket/match
         racket/math)

;; ---------------------------------------------------------------------------------------------------
; Exercises for RacketFest Contracts Tutorial
; This file presents a few commented functions.
; Please write a contract for them that matches the
; function.

; int: Integer
(provide
 (contract-out
  [int integer?]))
(define int 1)

; 1+ : Real -> Real
(provide
 (contract-out
  [1+ (real? . -> . real?)]))
(define (1+ x) (+ x 1))

; A function like map
; mepe : (fn A -> B) (listof A) -> (listof B)
(provide
 (contract-out
  [mepe (parametric->/c [A B]
          ((A . -> . B) (listof A) . -> . (listof B)))]))
(define (mepe fn l)
  (for/list ([v (in-list l)])
    (fn v)))

; withdraw : Integer Integer -> Integer
; withdraws a certain amount from a balance
; and returns the new balance, which
; has to be large positive.
(provide
 (contract-out
  [withdraw (->i ([b nonnegative-integer?]
                  [a (b)
                     (and/c nonnegative-integer?
                            (<=/c b))])
                 [_ nonnegative-integer?])]))
(define (withdraw balance amount)
  (- balance amount))

; x, y, and z are all real numbers
(provide
 (contract-out
  [struct 3d-point
    ([x real?]
     [y real?]
     [z real?])]))
(struct 3d-point (x y z)
  #:transparent)

; name is a symbol
(provide
 (contract-out
  [struct named-3d-point
    ([x real?]
     [y real?]
     [z real?]
     [name symbol?])]))
(struct named-3d-point 3d-point (name)
  #:transparent)

(provide
 (contract-out
  [give-name (symbol? 3d-point? . -> . named-3d-point?)]))
(define (give-name name 3d-point)
  (named-3d-point
   (3d-point-x 3d-point)
   (3d-point-y 3d-point)
   (3d-point-z 3d-point)
   name))

; Are all the real values in `lst`, in between reals v1 and v2?
(provide
 (contract-out
  [all-in-between? (integer? integer? ... integer? . -> . boolean?)]))
(define all-in-between?
  (match-lambda*
    [(list v1 lst v2)
     (for/and ([v (in-list lst)])
       (<= v1 v v2))]))

; Returns the first element in the non-empty list lst that maximizes the result of proc.
(provide
 (contract-out
  [argmaxx (->i ([f (any/c . -> . real?)]
                 [lov (and/c pair? list?)])
                [r (f lov)
                   (lambda (r)
                     (define fr (f r))
                     (define flov (map f lov))
                     (and (is-first-max? r fr (map list lov flov))
                          (dominates-all fr lov)))])]))

; contract helpers
(define (dominates-all fr flov)
  (for/and ([fv (in-list flov)]) (>= fr fv)))
(define (is-first-max? r fr lov+flov)
  (define fst (first lov+flov))
  (if (= (second fst) fr)
      (eq? (first fst) r)
      (is-first-max? r fr (rest lov+flov))))

(define (argmaxx f xs)
  (define init-max-var (f (car xs)))

  (let loop ([max (car xs)]
             [max-var init-max-var]
             [xs (cdr xs)])
    (cond
      [(null? xs) max]
      [else
       (let ([new-max (f (car xs))])
         (cond
           [(> new-max max-var)
            (loop (car xs) new-max (cdr xs))]
           [else
            (loop max max-var (cdr xs))]))])))

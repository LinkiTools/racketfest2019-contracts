#lang racket/base
;; ---------------------------------------------------------------------------------------------------

(require racket/contract
         racket/match)

;; ---------------------------------------------------------------------------------------------------
; Exercises for RacketFest Contracts Tutorial
; This file presents a few commented functions.
; Please write a contract for them that matches the
; function.

; int: Integer
(define int 1)

; 1+ : Number -> Number
(define (1+ x) (+ x 1))

; A function like map
; mepe : (fn A -> B) (listof A) -> (listof B)
(define (mepe fn l)
  (for/list ([v (in-list l)])
    (fn v)))

; withdraw : Integer Integer -> Integer
; withdraws a certain amount from a balance
; and returns the new balance, which
; has to be large positive.
(define (withdraw balance amount)
  (- balance amount))

; x, y, and z are all real numbers
(struct 3d-point (x y z)
  #:transparent)

; name is a symbol
(struct named-3d-point 3d-point (name)
  #:transparent)

(define (give-name name 3d-point)
  (named-3d-point
   (3d-point-x 3d-point)
   (3d-point-y 3d-point)
   (3d-point-z 3d-point)
   name))

; Are all the real values in `lst`, in between reals v1 and v2?
(define all-in-between?
  (match-lambda*
    [(list v1 lst v2)
     (for/and ([v (in-list lst)])
       (<= v1 v v2))]))

; Returns the first element in the non-empty list lst that maximizes the result of proc.
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

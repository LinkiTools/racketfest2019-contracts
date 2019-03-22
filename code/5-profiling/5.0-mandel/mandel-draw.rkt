#lang racket
;; ---------------------------------------------------------------------------------------------------

(require contract-profile
         "complex-number.rkt")

;; ---------------------------------------------------------------------------------------------------

;; in-mandelbrot-set? complex? positive-integer? -> boolean?
;; returns #true if c remains bounded after n iterations of the mandelbrot equation
(define (in-mandelbrot-set? c n)
  (define v
    (for/fold ([z (complex 0 0)])
              ([i (in-range n)]
               #:break (> (complex-mag z) 1))
;      (printf "~a ~a~n" i z)
      (complex+ (complex-square z) c)))
  (<= (complex-mag v) 1))

;; mandel-draw positive-number? positive-number? -> (hash complex? -> bool?)
(define (mandel-draw w h n)
  (define hash (make-hash `((width . ,w) (height . ,h))))
  (define stepw (exact->inexact (/ 3 w)))
  (define steph (exact->inexact (/ 2 h)))

  (for* ([wv (in-range w)]
         [hv (in-range h)])
    (define v (complex+ (complex (* stepw wv) 0)
                        (complex 0 (- (* steph hv)))
                        (complex -2 1)))
    (define in? (in-mandelbrot-set? v n))
    ;(printf "~a ~a : ~a : ~a~n" wv hv v in?)
    (hash-set! hash (cons wv hv) in?))

  hash)

;; hash->ppm (hash complex -> bool?) -> void?
;; Prints to stdout a plain ppm image corresponding to the hash values.
(define (hash->ppm h filepath [white 255] [black 0])
  (define width (hash-ref h 'width))
  (define height (hash-ref h 'height))
  (with-output-to-file filepath
    (lambda ()
      (printf "P3~n")
      (printf "~a ~a~n" width height)
      (printf "255~n")
      (for* ([hv (in-range height)]
             [wv (in-range width)])
        (if (hash-ref h (cons wv hv))
            (printf "~a ~a ~a " black black black)
            (printf "~a ~a ~a " white white white))
        (when (= wv (sub1 width))
          (printf "~n"))))
    #:exists 'replace))

(module+ main

  (require racket/cmdline)

  (define output-filename (make-parameter #false))
  (define number-of-iterations (make-parameter 100))

  (define-values (width height)
    (command-line
     #:once-each
     [("-o" "--output") f "Output filename"
                        (output-filename f)]
     [("-i" "--iterations") i "Number of iterations per pixel"
                            (number-of-iterations (string->number i))]
     #:args (width height)
     (values (string->number width)
             (string->number height))))

  (hash->ppm (mandel-draw width height (number-of-iterations))
             (output-filename)))

#lang slideshow

(require slideshow/text
         slideshow/code
         racket/draw)

(set-page-numbers-visible! #true)


(current-main-font "Roboto")
(current-code-font "Inconsolata")
(current-font-size 24)
(get-current-code-font-size (thunk (- (current-font-size) 4)))

(define lt-thin-logo
  (scale (bitmap "imgs/lt-thin-logo.png") 0.6))

(define default-slide-assembler
  (current-slide-assembler))
(define default-titlet
  (current-titlet))

(define (url s)
  (colorize (text s (current-code-font) (- (current-font-size) 2))
            "blue"))

(define (linkitools-titlet s)
  (colorize (text s "Bebas Neue" 40)
            (make-object color% 0 85 135)))

(define (linkitools-slide-assembler s v-sep c)
  (parameterize ([current-titlet linkitools-titlet])
    (lb-superimpose
     (ct-superimpose (default-slide-assembler s v-sep c)
                     full-page)
     lt-thin-logo)))

(define-syntax-rule (lt-slide v ...)
  (parameterize ([current-slide-assembler linkitools-slide-assembler])
    (slide v ...)))

(define-syntax-rule (itt v)
  (text v `(italic . (bold . modern))  (current-font-size)))

(slide
 #:layout 'center
 (vc-append
  (big (bt "Introduction to Contracts"))
  (blank-line)
  (hc-append (t "Paulo Matos <")
             (url "pmatos@linki.tools")
             (t ">"))
  (scale (bitmap "imgs/lt-logo.png") 0.4)))

(lt-slide
 #:title "Flat Contracts"
 (item "Predicates are flat contracts: "
       (tt "vector?") ", "
       (tt "list?") ", "
       (tt "symbol?") "...")
 'next
 (code
  (define/contract an-integer
    integer?
    2)
  (define/contract a-list
    list?
    '(1 2 3))))

(lt-slide
 #:title "Contract combinators - 1"
 (item (tt "list?") " checks for lists but how to check for lists of integers?")
 'next
 (subitem (tt "(listof integer?)"))
 'next
 (item "vectors of symbols:")
 'next
 (subitem (tt "(vectorof symbol?)"))
 'next
 (item "pair of symbol and boolean:")
 'next
 (subitem (tt "(cons/c symbol? boolean?)"))
 'next
 (item "set of pairs of integer")
 'next
 (subitem (tt "(set/c (cons/c integer? integer?))")))

(lt-slide
 #:title "Contract combinators - 2"
 (item "Function contracts:")
 (subitem (itt "ctc-domain1") " ... " (itt "ctc-domainN") " . -> . " (itt "range"))
 'next
 (subitem (tt "(integer? #:x 0 . -> . integer?)"))
 'next
 (subitem (tt "((listof integer?) . -> . (values symbol? boolean?))"))
 'next
 (subitem (tt "symbol? ... . -> . void?"))
 'next
 (subitem (tt "real? ... symbol? . -> . symbol?")))

(lt-slide
 #:title "Contract combinators - 3"
 (item "Now with optional arguments and rest arguments")
 (subitem
  (code (->* (_mandatory) (_optional) #:rest (_rest)
             _range)))
 'next
 (subitem
  (code (->* (integer?) (#:id symbol?) #:rest (listof real?)
             (vector/c integer? boolean?))))
 'next
 (subitem
  (code (->* (boolean?) (symbol? symbol?)
             void?))))

(lt-slide
 #:title "Contract combinators - 4"
 (item "Now with relationships")
 (subitem (tt "->i") " same as " (tt "->*") " but each member of the domain or range is named to be referenced elsewhere")
 'next
 (subitem
  (code (->i ([a (listof integer?)]
              [b (a) (and/c (listof boolean?)
                            (= (length a) (length b)))])
             (boolean?)
             [r (a b)
                (and/c (listof symbol?)
                       (= (length r)
                          (abs (- (length a) (length b)))))])))
 'next
 (subitem
  (code (->i ([a integer?]
              [b (a)
                 (and/c integer? (>/c b a))])
             integer?))))

(lt-slide
 #:title "Contract combinators - 5"
 (item "Structure instance")
 (code (struct complex (r i)))
 'next
 (code (struct/c complex zero? zero?))
 'next
 (item "Structural information")
 (code (struct complex
         ([r real?]
          [i real?]))))

(lt-slide
 #:title "Exercise - 1"
 (tt "code/3-function-ds-contracts/exercises.rkt"))

(lt-slide
 #:title "Breaking Contracts - 1"
 'next
 (code
  (define/contract (sum-list xs)
    ((listof integer?) . -> . integer?)
    (define total
      (for/fold ([s 0])
                ([x (in-list xs)])
        (+ s x)))
    (if (> total 100)
        'buh
        total))))

(lt-slide
 #:title "Breaking Contracts - 2"
 (code (contract-exercise sum-list)))

(lt-slide
 #:title "Exercise - 2"
 (item "Can we create a function with a contract, where the function breaks the contract but for which " (code contract-exercise) " does not find any problem with it?"))

(lt-slide
 #:title "Contract Profiling"
 (item "Open " (tt "code/5-profiling/5.0-mandel/"))
 (subitem "take some time to look at the code")
 (item "How much time is taken by contracts?")
 (subitem "10%?")
 'next
 (subitem "20%?")
 'next
 (subitem "30%?")
 'next
 (subitem "40%?")
 'next
 (subitem "50%?")
 'next
 (subitem "60%?")
 'next
 (subitem "70%?")
 'next
 (subitem "80%?"))

(lt-slide
 #:title "Exercise - 3"
 (item "Use " (code contract-profile-thunk) " to profile the contracts"))

(lt-slide
 #:title "Disabling contracts - Exercise 4"
 (item "Take a look at " (tt "code/5-profiling/5.2-disabling-contracts"))
 (subitem "Run with and without contracts by changing the value of the variable " (tt "ENABLE_CONTRACTS")))

(lt-slide
 #:title "Thank you!")

#lang racket/base
;; ---------------------------------------------------------------------------------------------------

(require (for-syntax racket/base
                     syntax/parse)
         racket/sandbox
         scribble/manual
         scribble/example)

(provide codeblock/module
         codeblock/continue)

;; ---------------------------------------------------------------------------------------------------

; Defines code as a module that's output just like codeblock
; but it's also kept with a unique identifier for
; later evaluation under `examples/module`.
(define-syntax (codeblock/module stx)
  (syntax-parse stx
    [(_ (~seq (~seq #:module-name name:id)
              (~or (~optional (~seq #:expand expand-expr:expr)
                              #:defaults ([expand-expr #'#f])
                              #:name "#:expand keyword")
                   (~optional (~seq #:indent indent-expr:expr)
                              #:defaults ([indent-expr #'0])
                              #:name "#:expand keyword")
                   (~optional (~seq #:keep-lang-line? keep-lang-line?-expr:expr)
                              #:defaults ([keep-lang-line?-expr #'#t])
                              #:name "#:keep-lang-line? keyword")
                   (~optional (~seq #:context context-expr:expr)
                              #:name "#:context keyword")
                   (~optional (~seq #:line-numbers line-numbers:expr)
                              #:defaults ([line-numbers #'#f])
                              #:name "#:line-numbers keyword")
                   (~optional (~seq #:line-number-sep line-number-sep:expr)
                              #:defaults ([line-number-sep #'1])
                              #:name "#:line-number-sep keyword")
                   (~optional (~seq #:block? block?)
                              #:defaults ([block? #'#t])
                              #:name "#:block? keyword"))
              ...)
        str ...)
     #`(begin
         (define name
           (parameterize ([sandbox-output 'string]
                          [sandbox-error-output 'string])
             (make-module-evaluator
              (open-input-string
               #,(for/fold ([code ""])
                           ([s (in-list (syntax->datum #'(str ...)))])
                   (string-append code s))))))
         (typeset-code
                #:context #,(if (attribute context-expr)
                                #'context-expr
                                (or
                                 (let ([v #'(str ...)])
                                   (and (pair? (syntax-e v))
                                        #`#'#,(car (syntax-e v))))
                                 #'#f))
                #:expand expand-expr
                #:indent indent-expr
                #:keep-lang-line? keep-lang-line?-expr
                #:line-numbers line-numbers
                #:line-number-sep line-number-sep
                #:block? block?
                str ...))]))

(define-syntax (codeblock/continue stx)
  (syntax-parse stx
    [(_ (~seq (~seq #:module-name name:id)
              (~or (~optional (~seq #:expand expand-expr:expr)
                              #:defaults ([expand-expr #'#f])
                              #:name "#:expand keyword")
                   (~optional (~seq #:indent indent-expr:expr)
                              #:defaults ([indent-expr #'0])
                              #:name "#:expand keyword")
                   (~optional (~seq #:keep-lang-line? keep-lang-line?-expr:expr)
                              #:defaults ([keep-lang-line?-expr #'#t])
                              #:name "#:keep-lang-line? keyword")
                   (~optional (~seq #:context context-expr:expr)
                              #:name "#:context keyword")
                   (~optional (~seq #:line-numbers line-numbers:expr)
                              #:defaults ([line-numbers #'#f])
                              #:name "#:line-numbers keyword")
                   (~optional (~seq #:line-number-sep line-number-sep:expr)
                              #:defaults ([line-number-sep #'1])
                              #:name "#:line-number-sep keyword")
                   (~optional (~seq #:block? block?)
                              #:defaults ([block? #'#t])
                              #:name "#:block? keyword"))
              ...)
        str ...)
     #`(begin
         (name (string-append str ...))
         (typeset-code
          #:context #,(if (attribute context-expr)
                          #'context-expr
                          (or
                           (let ([v #'(str ...)])
                             (and (pair? (syntax-e v))
                                  #`#'#,(car (syntax-e v))))
                           #'#f))
          #:expand expand-expr
          #:indent indent-expr
          #:keep-lang-line? keep-lang-line?-expr
          #:line-numbers line-numbers
          #:line-number-sep line-number-sep
          #:block? block?
          str ...))]))

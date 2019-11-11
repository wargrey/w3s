#lang typed/racket/base

;;; https://drafts.xmlwg.org/xml-syntax/#tokenization

(provide (all-defined-out))

(require "digicore.rkt")
(require "misc.rkt")

(require "tokenizer/port.rkt")

(require racket/fixnum)

(require typed/racket/unsafe)

(require (for-syntax racket/base))

(unsafe-require/typed
 racket/base ; the line is gauranteed to count, hence the explicitly requiring.
 [port-next-location (-> Port (Values Positive-Integer Natural Positive-Integer))])

(struct xml-srcloc
  ([in : Input-Port]
   [source : (U String Symbol)]
   [line : Positive-Integer]
   [column : Natural]
   [position : Positive-Integer])
  #:type-name XML-Srcloc)

(define-syntax (xml-make-token stx)
  (syntax-case stx []
    [(_ src make-xml:token datum ...)
     #'(let-values ([(line column here-position) (port-next-location (xml-srcloc-in src))])
         (make-xml:token (xml-srcloc-source src) (xml-srcloc-line src) (xml-srcloc-column src)
                         (xml-srcloc-position src) here-position datum ...))]))
  
(define-syntax (xml-make-bad-token stx)
  (syntax-case stx []
    [(_ src xml:bad:sub token datum)
     #'(let ([bad (xml-make-token src xml:bad:sub (~s (cons (object-name token) datum)))])
         (xml-log-read-error (xml-token->string bad))
         bad)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml-consume-token : (-> Input-Port Symbol Any)
  (lambda [/dev/xmlin name]
    #false))

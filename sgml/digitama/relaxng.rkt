#lang typed/racket/base

(provide (all-defined-out) SGML-Stdin)
(provide rnc-check-prefix?)

(require "relaxng/rnc.rkt")
(require "relaxng/compact.rkt")

(require "digicore.rkt")
(require "stdin.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct rnc-grammar
  ([tagname : Symbol]
   [location : (U String Symbol)]
   [default-namespace : (Option (U Symbol String))] ; `#false` implies `inherit`
   [namespaces : RNC-Preamble-Namespaces]
   [datatypes : RNC-Preamble-Datatypes]
   [body : (U Pattern (Listof Grammar-Content))])
  #:transparent
  #:type-name RNC-Grammar)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define read-rnc-grammar : (->* (SGML-Stdin) ((U False String Symbol) #:tagname Symbol) RNC-Grammar)
  (lambda [/dev/rawin [port-name #false] #:tagname [name 'grammar]]
    (define /dev/dtdin : Input-Port (rnc-open-input-port /dev/rawin #true port-name))
    (define source : (U Symbol String) (or port-name (sgml-port-name /dev/dtdin)))
    (define tokens : (Listof XML-Token) (read-rnc-tokens* /dev/dtdin source))
    (define-values (preamble body-tokens) (rnc-grammar-parse (<:rnc-preamble:>) tokens))
    (define-values (default-ns ns dts) (rnc-grammar-environment preamble))

    (define-values (body rest)
      (parameterize ([rnc-default-namespaces ns]
                     [rnc-default-datatypes dts])
        (rnc-grammar-parse (<:rnc-body:>) body-tokens)))

    #;(let ([whitespace-count (count xml:whitespace? rest)])
        (when (< whitespace-count (length rest))
          (make+exn:xml:malformed rest)))
    
    (rnc-grammar name source default-ns ns dts
                 (let-values ([(grammars patterns) (partition grammar-content? body)])
                   (cond [(null? patterns) grammars]
                         [else (assert (car patterns) pattern?)])))))

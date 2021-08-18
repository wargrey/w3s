#lang typed/racket/base

(provide (all-defined-out) SGML-StdIn)

(require racket/list)

(require "relaxng/rnc.rkt")
(require "relaxng/grammar.rkt")

(require "digicore.rkt")
(require "stdin.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct rng-grammar
  ([location : (U String Symbol)]
   [default-namespace : (Option (U Symbol String))] ; `#false` implies `inherit`
   [namespaces : (Immutable-HashTable Symbol (Option String))]
   [datatypes : (Immutable-HashTable Symbol String)]
   [body : (Listof (U RNG-Pattern RNG-Grammar-Content))])
  #:transparent
  #:type-name RNG-Grammar)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define read-rnc-grammar : (->* (SGML-StdIn) ((U False String Symbol)) RNG-Grammar)
  (lambda [/dev/rawin [port-name #false]]
    (define /dev/dtdin : Input-Port (rnc-open-input-port /dev/rawin #true port-name))
    (define source : (U Symbol String) (or port-name (sgml-port-name /dev/dtdin)))
    (define tokens : (Listof XML-Token) (read-rnc-tokens* /dev/dtdin source))
    (define-values (preamble body-tokens) (rnc-grammar-parse (<:rnc-preamble:>) tokens))
    (define-values (default-ns ns dts) (rnc-grammar-environment preamble))
    (define-values (body rest) (rnc-grammar-parse (<:rnc-body:>) body-tokens))

    #;(let ([whitespace-count (count xml:whitespace? rest)])
        (when (< whitespace-count (length rest))
          (make+exn:xml:malformed rest)))
    
    (rng-grammar source default-ns ns dts body)))

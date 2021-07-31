#lang typed/racket/base

(provide (all-defined-out) SGML-StdIn)

(require "relaxng/rnc.rkt")
(require "relaxng/grammar.rkt")

(require "stdin.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct rng-grammar
  ([location : (U String Symbol)]
   [tokens : (Listof XML-Token)])
  #:transparent
  #:type-name RNG-Grammar)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define read-rnc-grammar : (->* (SGML-StdIn) ((U False String Symbol)) RNG-Grammar)
  (lambda [/dev/rawin [port-name #false]]
    (define /dev/dtdin : Input-Port (rnc-open-input-port /dev/rawin #true port-name))
    (define source : (U Symbol String) (or port-name (sgml-port-name /dev/dtdin)))
    (define tokens : (Listof XML-Token) (read-rnc-tokens* /dev/dtdin source))
    (define-values (rnc-decls pattern-tokens) (rnc-grammar-parse (<:rnc-decl*:>) tokens))

    (for-each displayln rnc-decls)
    
    (rng-grammar source pattern-tokens)))

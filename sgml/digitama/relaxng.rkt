#lang typed/racket/base

(provide (all-defined-out) SGML-StdIn)

(require "relaxng/rnc.rkt")
(require "relaxng/grammar.rkt")

(require "stdin.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define read-rnc-declaration : (->* (SGML-StdIn) ((U False String Symbol)) RNG-Grammar)
  (lambda [/dev/rawin [port-name #false]]
    (define /dev/dtdin : Input-Port (rnc-open-input-port /dev/rawin #true port-name))
    (define source : (U Symbol String) (or port-name (sgml-port-name /dev/dtdin)))
    (define tokens : (Listof XML-Token) (read-rnc-tokens* /dev/dtdin source))

    ((<:rnc-namespace:>) null tokens)
    
    (rng-grammar source tokens)))

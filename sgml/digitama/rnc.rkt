#lang typed/racket/base

(provide (all-defined-out))

(require "relaxng/rnc.rkt")

(require "digicore.rkt")
(require "stdin.rkt")
(require "grammar.rkt")
(require "schema.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type DTD-Raw-Declaration* (Immutable-Vector XML:Name (Listof XML-Token)))
(define-type DTD-Definition* (U XML-Processing-Instruction* XSch-Entity XSch-Notation XSch-Element XSch-Attribute DTD-Raw-Declaration*))
(define-type DTD-Declaration* (U DTD-Definition* XML:PEReference DTD-Section))

(struct dtd-section
  ([condition : (U XML:Name XML:PEReference)]
   [body : (Listof DTD-Declaration*)])
  #:transparent
  #:type-name DTD-Section)

(struct xml-dtd
  ([location : (U String Symbol)]
   [declarations : (Listof DTD-Declaration*)])
  #:transparent
  #:type-name XML-DTD)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define read-rnc-declaration : (->* (SGML-StdIn) ((U False String Symbol)) (Listof XML-Token))
  (lambda [/dev/rawin [port-name #false]]
    (define /dev/dtdin : Input-Port (rnc-open-input-port /dev/rawin #true port-name))
    (define source : (U Symbol String) (or port-name (sgml-port-name /dev/dtdin)))
    (define tokens : (Listof XML-Token) (read-rnc-tokens* /dev/dtdin source))
    
    tokens))

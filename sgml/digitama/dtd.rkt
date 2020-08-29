#lang typed/racket/base

;;; https://www.w3.org/TR/xml11/#sec-documents

(provide (all-defined-out))

(require "doctype.rkt")
(require "grammar.rkt")
(require "digicore.rkt")
(require "stdin.rkt")

(require "tokenizer/port.rkt")
(require "tokenizer/grammar.rkt")
(require "tokenizer.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct xml-type-definition
  ([location : (U String Symbol)]
   [tokens : (Listof XML-Token)])
  #:transparent
  #:type-name XML-Type-Definition)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define read-xml-type-definition : (-> SGML-StdIn XML-Type-Definition)
  (lambda [/dev/rawin]
    (define /dev/dtdin : Input-Port (dtd-open-input-port /dev/rawin #true))
    (define source : (U Symbol String) (sgml-port-name /dev/dtdin))
    (define tokens : (Listof XML-Token) (read-xml-tokens* /dev/dtdin source))
    
    (xml-type-definition source tokens)))

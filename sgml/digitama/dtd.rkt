#lang typed/racket/base

;;; https://www.w3.org/TR/xml11/#sec-documents

(provide (all-defined-out))

(require "doctype.rkt")
(require "digicore.rkt")
(require "stdin.rkt")
(require "grammar.rkt")
(require "tokenizer.rkt")

(require "tokenizer/port.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct xml-type-definition
  ([location : (U String Symbol)]
   [tokens : (Listof XML-Definition*)])
  #:transparent
  #:type-name XML-Type-Definition)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define read-xml-type-definition : (-> SGML-StdIn XML-Type-Definition)
  (lambda [/dev/rawin]
    (define /dev/dtdin : Input-Port (dtd-open-input-port /dev/rawin #true))
    (define source : (U Symbol String) (sgml-port-name /dev/dtdin))
    (define tokens : (Listof XML-Token) (read-xml-tokens* /dev/dtdin source))
    (define definitions : (Listof XML-Definition*) (xml-syntax->definition* tokens))
    
    (xml-type-definition source definitions)))

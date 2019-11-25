#lang typed/racket/base

;;; https://www.w3.org/TR/xml11/#sec-documents

(provide (all-defined-out))

(require racket/string)
(require racket/path)

#;(require "digicore.rkt")
(require "stdin.rkt")
#;(require "misc.rkt")

(require "tokenizer/port.rkt")
(require "tokenizer.rkt")

(struct xml-document
  ([location : (U String Symbol)]
   [namespaces : (Listof (Pairof Symbol String))]
   [version : (Option Nonnegative-Flonum)]
   [encoding : (Option String)]
   [standalone? : Boolean]
   [tokens : (Listof Any)])
  #:transparent
  #:type-name XML-Document)

(define xml-document-placeholder : XML-Document
  (xml-document '/dev/null null 1.1 "UTF-8" #true null))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define read-xml-document : (-> XML-StdIn XML-Document)
  (lambda [/dev/rawin]
    (define-values (/dev/xmlin version encoding standalone?) (xml-open-input-port /dev/rawin #false))

    (xml-document '/dev/null null version encoding standalone?
                  (read-xml-tokens /dev/xmlin))))

(define read-xml-document* : (-> XML-StdIn XML-Document)
  (lambda [/dev/rawin]
    (define-values (/dev/xmlin version encoding standalone?) (xml-open-input-port /dev/rawin #true))

    (xml-document '/dev/null null version encoding standalone?
                  (read-xml-tokens* /dev/xmlin))))

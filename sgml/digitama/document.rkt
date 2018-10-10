#lang typed/racket/base

;;; https://www.w3.org/TR/xml11/#sec-documents

(provide (all-defined-out))

(require racket/string)
(require racket/path)

(require "digicore.rkt")
(require "stdin.rkt")
(require "misc.rkt")

(struct XML-Document
  ([location : (U String Symbol)]
   [namespaces : (Listof (Pairof Symbol String))]
   [tokens : (Listof Any)])
  #:transparent)

(define xml-document-placeholder : XML-Document
  (XML-Document '/dev/null null null))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-xml-parser-entry read-xml-document #:-> XML-Document
  (lambda [/dev/xmlin]
    (XML-Document '/dev/null null
                  (for/list : (Listof XML-Syntax-Any) ([token (in-port xml-read-syntax /dev/xmlin)])
                    token))))

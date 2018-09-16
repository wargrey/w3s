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
   [namespaces : (Listof (Pairof Symbol String))])
  #:transparent)

(define xml-document-placeholder : XML-Document
  (XML-Document '/dev/null null))

#|
(define /dev/cssin : Input-Port (css-open-input-port /dev/stdin))
         (dynamic-wind (λ [] '(css-open-input-port has already enabled line counting))
                       (λ [] ((λ [[/dev/cssin : Input-Port] [args : T defval ...] ...] : ->T body ...) /dev/cssin args ...))
                       (λ [] (close-input-port /dev/cssin))))])
|#

(define read-xml-document : (-> Input-Port XML-Document)
  (lambda [/dev/xmlin]
    xml-document-placeholder))

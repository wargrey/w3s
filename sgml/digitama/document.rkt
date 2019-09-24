#lang typed/racket/base

;;; https://www.w3.org/TR/xml11/#sec-documents

(provide (all-defined-out))

(require racket/string)
(require racket/path)

#;(require "digicore.rkt")
#;(require "stdin.rkt")
#;(require "misc.rkt")

(require "tokenizer/port.rkt")

(struct xml-document
  ([location : (U String Symbol)]
   [namespaces : (Listof (Pairof Symbol String))]
   [tokens : (Listof Any)])
  #:transparent
  #:type-name XML-Document)

(define xml-document-placeholder : XML-Document
  (xml-document '/dev/null null null))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define read-xml-document : (-> Input-Port XML-Document)
  (lambda [/dev/xmlin]
    (xml-document '/dev/null null
                  (reverse (read-xml/reverse /dev/xmlin)))))

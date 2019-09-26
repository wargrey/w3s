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
   [version : (Option Positive-Flonum)]
   [encoding : (Option String)]
   [standalone? : Boolean]
   [tokens : (Listof Any)])
  #:transparent
  #:type-name XML-Document)

(define xml-document-placeholder : XML-Document
  (xml-document '/dev/null null 1.1 "UTF-8" #true null))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define read-xml-document : (-> Input-Port XML-Document)
  (lambda [/dev/xmlin]
    (define-values (version encoding standalone?)
      (cond [(not (xml-stream-valid? /dev/xmlin)) (values #false #false #false)]
            [else (xml-consume-declaration /dev/xmlin)]))
    
    (xml-document '/dev/null null version encoding standalone?
                  (reverse (read-xml/reverse /dev/xmlin)))))

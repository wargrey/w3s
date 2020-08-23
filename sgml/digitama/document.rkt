#lang typed/racket/base

;;; https://www.w3.org/TR/xml11/#sec-documents

(provide (all-defined-out))

(require racket/string)
(require racket/path)

(require "grammar.rkt")
(require "stdin.rkt")

(require "tokenizer/port.rkt")
(require "tokenizer.rkt")

(struct xml-document
  ([location : (U String Symbol)]
   [namespaces : (Listof (Pairof Symbol String))]
   [version : (Option Nonnegative-Flonum)]
   [encoding : (Option String)]
   [standalone? : Boolean]
   [decltype : Symbol]
   [public : (Option String)]
   [system : (Option String)]
   [subset : (Listof Any)]
   [tokens : (Listof Any)])
  #:transparent
  #:type-name XML-Document)

(define xml-document-placeholder : XML-Document
  (xml-document '/dev/null null 1.1 "UTF-8" #true
                '|| #false #false null
                null))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define read-xml-document : (-> XML-StdIn XML-Document)
  (lambda [/dev/rawin]
    (define-values (/dev/xmlin version encoding standalone?) (xml-open-input-port /dev/rawin #false))

    (xml-document '/dev/null null version encoding standalone?
                  '|| #false #false null
                  (read-xml-tokens /dev/xmlin))))

(define read-xml-document* : (-> XML-StdIn XML-Document)
  (lambda [/dev/rawin]
    (define-values (/dev/xmlin version encoding standalone?) (xml-open-input-port /dev/rawin #true))
    (define tokens : (Listof XML-Token) (read-xml-tokens* /dev/xmlin))

    (xml-document '/dev/null null version encoding standalone?
                  '|| #false #false null
                  (xml-syntax->grammar tokens))))

#lang typed/racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; https://www.w3.org/TR/xml11/                                                                ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide (all-defined-out) SGML-StdIn XML-DTD XML-External-ID)
(provide (struct-out XML-Document) read-xml-document)
(provide (struct-out XML-Document*) read-xml-document*)

(require "digitama/dtd.rkt")
(require "digitama/doctype.rkt")
(require "digitama/document.rkt")
(require "digitama/stdin.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml-doc-location : (-> (U XML-Document XML-Document*) (U String Symbol))
  (lambda [xml]
    (xml-doctype-location
     (cond [(xml-document? xml) (xml-document-doctype xml)]
           [else (xml-document*-doctype xml)]))))

(define xml-doc-version : (-> (U XML-Document XML-Document*) (Option Nonnegative-Flonum))
  (lambda [xml]
    (xml-doctype-version
     (cond [(xml-document? xml) (xml-document-doctype xml)]
           [else (xml-document*-doctype xml)]))))

(define xml-doc-standalone? : (-> (U XML-Document XML-Document*) Boolean)
  (lambda [xml]
    (xml-doctype-standalone?
     (cond [(xml-document? xml) (xml-document-doctype xml)]
           [else (xml-document*-doctype xml)]))))

(define xml-doc-encoding : (-> (U XML-Document XML-Document*) (Option String))
  (lambda [xml]
    (xml-doctype-encoding
     (cond [(xml-document? xml) (xml-document-doctype xml)]
           [else (xml-document*-doctype xml)]))))

(define xml-doc-type : (-> (U XML-Document XML-Document*) Symbol)
  (lambda [xml]
    (xml-doctype-name
     (cond [(xml-document? xml) (xml-document-doctype xml)]
           [else (xml-document*-doctype xml)]))))

(define xml-doc-external : (-> (U XML-Document XML-Document*) XML-External-ID)
  (lambda [xml]
    (xml-doctype-external
     (cond [(xml-document? xml) (xml-document-doctype xml)]
           [else (xml-document*-doctype xml)]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module reader racket/base
  (provide (except-out (all-from-out racket/base) read read-syntax))

  (provide (rename-out [xml-read read]))
  (provide (rename-out [xml-read-syntax read-syntax]))
  (provide (rename-out [xml-info get-info]))
  
  (require sgml/village/sgmlang/reader))

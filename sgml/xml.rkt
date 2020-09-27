#lang typed/racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; https://www.w3.org/TR/xml/                                                                  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide (all-defined-out))
(provide SGML-StdIn XML-DTD XML:Space-Filter XML-Space-Position Open-Input-XML-XXE)
(provide (struct-out XML-Document) read-xml-document)
(provide (struct-out XML-Document*) read-xml-document* xml-document*-normalize)
(provide svg:space-filter xml-load-relative-system-dtd)
(provide default-xml-ipe-topsize default-xml-xxe-topsize default-xml-xxe-timeout)

(require "digitama/dtd.rkt")
(require "digitama/doctype.rkt")
(require "digitama/document.rkt")
(require "digitama/normalize.rkt")
(require "digitama/stdin.rkt")

(require "digitama/digicore.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml-doc-location : (-> (U XML-Document XML-Document*) (U String Symbol))
  (lambda [xml]
    (xml-prolog-location
     (cond [(xml-document? xml) (xml-document-prolog xml)]
           [else (xml-document*-prolog xml)]))))

(define xml-doc-version : (-> (U XML-Document XML-Document*) (Option Nonnegative-Flonum))
  (lambda [xml]
    (xml-prolog-version
     (cond [(xml-document? xml) (xml-document-prolog xml)]
           [else (xml-document*-prolog xml)]))))

(define xml-doc-standalone? : (-> (U XML-Document XML-Document*) Boolean)
  (lambda [xml]
    (xml-prolog-standalone?
     (cond [(xml-document? xml) (xml-document-prolog xml)]
           [else (xml-document*-prolog xml)]))))

(define xml-doc-encoding : (-> (U XML-Document XML-Document*) (Option String))
  (lambda [xml]
    (xml-prolog-encoding
     (cond [(xml-document? xml) (xml-document-prolog xml)]
           [else (xml-document*-prolog xml)]))))

(define xml-doc-type : (-> (U XML-Document XML-Document*) (Option Symbol))
  (lambda [xml]
    (cond [(xml-document? xml) (xml-doctype-name (xml-document-doctype xml))]
          [else (let ([?name (xml-doctype*-name (xml-document*-doctype xml))])
                  (and ?name (xml:name-datum ?name)))])))

(define xml-doc-external : (-> (U XML-Document XML-Document*) (U False String (Pairof String String)))
  (lambda [xml]
    (cond [(xml-document? xml) (xml-doctype-external (xml-document-doctype xml))]
          [else (xml-external-id->datum (xml-doctype*-external (xml-document*-doctype xml)))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module reader racket/base
  (provide (except-out (all-from-out racket/base) read read-syntax))

  (provide (rename-out [xml-read read]))
  (provide (rename-out [xml-read-syntax read-syntax]))
  (provide (rename-out [xml-info get-info]))
  
  (require sgml/village/sgmlang/reader))

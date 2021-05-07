#lang typed/racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; https://www.w3.org/TR/xml/                                                                  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide (all-defined-out) (all-from-out "dtd.rkt"))
(provide (all-from-out "digitama/namespace.rkt" "digitama/whitespace.rkt"))
(provide (all-from-out "digitama/plain/xexpr.rkt"))
(provide XML-DTD XML-External-Doctype-Entity xml-load-relative-system-entity)
(provide (struct-out XML-Document) read-xml-document xml-document-normalize xml-document*->document)
(provide (struct-out XML-Document*) read-xml-document* xml-document*-normalize xml-document*-valid?)
(provide XML-DTD-Guard XML-XXE-Guard make-xml-dtd-guard xml-dtd-guard? xml-xxe-guard?)

(require "dtd.rkt")

(require "digitama/dtd.rkt")
(require "digitama/doctype.rkt")
(require "digitama/document.rkt")
(require "digitama/normalize.rkt")

(require "digitama/digicore.rkt")
(require "digitama/namespace.rkt")
(require "digitama/whitespace.rkt")

(require "digitama/plain/xexpr.rkt")

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
(define xml-blank : (->* () (Symbol) XML-Document)
  (lambda [[root 'blank]]
    (xml-document (xml-prolog 'sgml/xml 1.0 #false #false)
                  (xml-doctype root #false)
                  (list (list root null null)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module reader racket/base
  (provide (except-out (all-from-out racket/base) read read-syntax))

  (provide (rename-out [xml-read read]))
  (provide (rename-out [xml-read-syntax read-syntax]))
  (provide (rename-out [xml-info get-info]))
  
  (require sgml/village/sgmlang/reader))

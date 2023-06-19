#lang typed/racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; https://www.w3.org/TR/xml/                                                                  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide (all-defined-out) (all-from-out "dtd.rkt" "xexpr.rkt"))
(provide (all-from-out "digitama/namespace.rkt" "digitama/whitespace.rkt" "digitama/datatype.rkt"))
(provide XML-DTD xml-load-relative-system-entity)
(provide (struct-out XML-Document*) read-xml-document* xml-document*-normalize xml-document*->document xml-document*-valid?)
(provide XML-DTD-Guard XML-XXE-Guard make-xml-dtd-guard xml-dtd-guard? xml-xxe-guard?)
(provide xml-attributes*-extract xml-attributes*-extract-xmlns)
(provide xml-attributes*-extract-pair xml-attributes*-extract-triplet)
(provide default-xml-error-topic)

(provide XML-Processing-Instruction XML-Content XML-Subdatum)
(provide (rename-out [xml-root-xexpr xml-doc-root]
                     [xexpr-attr-ref xml-attr-ref]
                     [xexpr-children-seek xml-children-seek]
                     [XExpr-Element-Children XML-Element-Children]
                     [XExpr-Attribute-Datum XML-Attribute-Datum]))

(require "dtd.rkt")
(require "xexpr.rkt")

(require "digitama/dtd.rkt")
(require "digitama/doctype.rkt")
(require "digitama/document.rkt")
(require "digitama/normalize.rkt")
(require "digitama/datatype.rkt")
(require "digitama/dialect.rkt")

(require "digitama/digicore.rkt")
(require "digitama/namespace.rkt")
(require "digitama/whitespace.rkt")

(require "digitama/plain/grammar.rkt")

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

(define xml-doc-encoding : (-> (U XML-Document XML-Document*) (Option String))
  (lambda [xml]
    (xml-prolog-encoding
     (cond [(xml-document? xml) (xml-document-prolog xml)]
           [else (xml-document*-prolog xml)]))))

(define xml-doc-standalone? : (-> (U XML-Document XML-Document*) Boolean)
  (lambda [xml]
    (xml-prolog-standalone?
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
          [else (xml-external-id*->datum (xml-doctype*-external (xml-document*-doctype xml)))])))

(define xml-doc-namespaces : (-> (U XML-Document XML-Document*) (Listof (Pairof Symbol String)))
  (lambda [xml]
    (define root-attlist : (Listof XML-Element-Attribute)
      (if (xml-document? xml)
          (let ([?root (xml-document-root-element xml)])
            (if (not ?root) null (cadr ?root)))
          (let ([?root (xml-document*-root-element xml)])
            (if (not ?root) null (map xml-attribute*->datum (cadr ?root))))))
    
    (let filter-namespace ([as : (Listof XML-Element-Attribute) root-attlist]
                           [sn : (Listof (Pairof Symbol String)) null])
      (cond [(null? as) (reverse sn)]
            [else (let-values ([(self rest) (values (car as) (cdr as))])
                    (filter-namespace rest
                                      (if (and (xml-qname-xmlns? (car self)) (string? (cdr self)))
                                          (cons self sn)
                                          sn)))]))))

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

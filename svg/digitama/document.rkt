#lang typed/racket/base

(provide (all-defined-out))

(require sgml/digitama/stdin)
(require sgml/digitama/doctype)
(require sgml/digitama/document)

(require "grammar.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct svg
  ([prolog : XML-Prolog]
   [doctype : XML-DocType]
   [root : SVG:Fragment])
  #:type-name SVG
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define read-svg-document : (-> SGML-StdIn SVG)
  (lambda [/dev/svgin]
    (define xml.svg : XML-Document (xml-document-normalize (read-xml-document /dev/svgin)))
    (define prolog : XML-Prolog (xml-document-prolog xml.svg))
    (define doctype : XML-DocType (xml-document-doctype xml.svg))
    
    (svg prolog doctype
         (svg-resolve-root
          (xml-document-contents xml.svg)
          (xml-prolog-location prolog)
          (xml-doctype-name doctype)))))

(define read-svg-document* : (-> SGML-StdIn SVG)
  (lambda [/dev/svgin]
    (define xml.svg : XML-Document* (xml-document*-normalize (read-xml-document* /dev/svgin)))
    (define prolog : XML-Prolog (xml-document*-prolog xml.svg))
    (define doctype : XML-DocType (xml-doctype*->doctype (xml-document*-doctype xml.svg)))
    
    (svg prolog doctype
         (svg-resolve-root*
          (xml-document*-contents xml.svg)
          (xml-prolog-location prolog)
          (xml-doctype-name doctype)))))

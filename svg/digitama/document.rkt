#lang typed/racket/base

(provide (all-defined-out))

(require sgml/digitama/stdin)
(require sgml/digitama/doctype)
(require sgml/digitama/document)
(require sgml/digitama/digicore)
(require sgml/digitama/convert)

(require "grammar.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct svg
  ([prolog : XML-Prolog]
   [doctype : XML-DocType]
   [root : SVG:SVG])
  #:type-name SVG
  #:transparent
  #:property prop:convertible
  (λ [[self : SVG] [mime : Symbol] [fallback : Any]] : Any
    (case mime
      [(svg-bytes) (xml-element->bytes (svg-root self) svg:svg-flatten-attributes)]
      [else fallback])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define read-svg-document : (-> SGML-Stdin SVG)
  (lambda [/dev/svgin]
    (define xml.svg : XML-Document (xml-document-normalize (read-xml-document /dev/svgin)))
    (define prolog : XML-Prolog (xml-document-prolog xml.svg))
    (define doctype : XML-DocType (xml-document-doctype xml.svg))
    
    (svg prolog doctype
         (svg-resolve-root
          (xml-document-content xml.svg)
          (xml-prolog-location prolog)
          (xml-doctype-name doctype)))))

(define read-svg-document* : (-> SGML-Stdin SVG)
  (lambda [/dev/svgin]
    (parameterize ([default-xml-error-topic 'exn:svg:syntax])
      (define xml.svg : XML-Document* (xml-document*-normalize (read-xml-document* /dev/svgin)))
      (define prolog : XML-Prolog (xml-document*-prolog xml.svg))
      (define doctype : XML-DocType (xml-doctype*->doctype (xml-document*-doctype xml.svg)))
      
      (svg prolog doctype
           (svg-resolve-root*
            (xml-document*-content xml.svg)
            (xml-prolog-location prolog)
            (xml-doctype-name doctype))))))

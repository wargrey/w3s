#lang typed/racket/base

(provide (all-defined-out))

(require sgml/xml)

;(require "../village/hashlang/svg11.dtd")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define svg-load-external-dtd : Open-Input-XML-XXE
  (make-xml-load-http-entity))

(define svg-dtd-unsafe-guard : XML-DTD-Guard (make-xml-dtd-guard #:xxe-timeout #false))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define read-svg-document* : (-> SGML-StdIn XML-Document*)
  (lambda [/dev/rawin]
    (read-xml-document* /dev/rawin svg-load-external-dtd #:dtd-guard svg-dtd-unsafe-guard)))

(define svg-document*-normalize : (-> XML-Document* XML-Document*)
  (lambda [doc.svg]
    (xml-document*-normalize doc.svg svg-load-external-dtd #:dtd-guard svg-dtd-unsafe-guard)))

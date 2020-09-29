#lang typed/racket/base

(provide (all-defined-out))

(require sgml/xml)

;(require "../village/hashlang/svg11.dtd")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define svg-load-external-dtd : Open-Input-XML-XXE
  (make-xml-load-http-entity))

(define read-svg-document* : (-> SGML-StdIn XML-Document*)
  (lambda [/dev/rawin]
    (read-xml-document* /dev/rawin svg-load-external-dtd #:xxe-timeout #false)))

(define svg-document*-normalize : (-> XML-Document* XML-Document*)
  (lambda [doc.svg]
    (xml-document*-normalize doc.svg svg-load-external-dtd #:xxe-timeout #false)))

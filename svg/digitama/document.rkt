#lang typed/racket/base

(provide (all-defined-out))

(require sgml/xml)

(require "../village/hashlang/svg11.dtd")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define read-svg-document* : (-> SGML-StdIn XML-Document*)
  (lambda [/dev/rawin]
    (read-xml-document* /dev/rawin svg11.dtd)))

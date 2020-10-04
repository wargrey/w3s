#lang racket/base

(provide (all-defined-out))

(require racket/path)
(require racket/port)

(require syntax/strip-context)

(require sgml/digitama/document)
(require sgml/village/sgmlang/reader)

(require svg/digitama/document)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define svg-read
  (lambda [[/dev/xmlin (current-input-port)]]
    (sgml-read read-xml-document /dev/xmlin)))

(define svg-read-syntax
  (lambda [[src #false] [/dev/svgin (current-input-port)]]
    (sgml-doc-read-syntax read-svg-document* 'XML-Document*
                          'svg #px"\\.svg$" ".svg" src /dev/svgin
                          svg-document*-normalize)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (svg-info in mod line col pos)
  (lambda [key default]
    (case key
      [(drracket:default-filters) '(["SVG Sources" "*.svg"])]
      [(drracket:default-extension) "svg"]
      [(drracket:indentation) (dynamic-require 'sgml/village/sgmlang/indentation 'xml-indentation)]
      [(color-lexer) (dynamic-require 'sgml/village/sgmlang/lexer 'xml-lexer)]
      [else default])))

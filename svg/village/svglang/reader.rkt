#lang racket/base

(provide (all-defined-out))

(require sgml/digitama/document)
(require sgml/village/sgmlang/reader)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define svg-read
  (lambda [[/dev/xmlin (current-input-port)]]
    (sgml-read read-xml-document /dev/xmlin)))

(define svg-read-syntax
  (lambda [[src #false] [/dev/svgin (current-input-port)]]
    (sgml-doc-read-syntax 'read-svg-document* 'svg
                          #px"\\.t?svg$" ".svg" src /dev/svgin)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (svg-info in mod line col pos)
  (lambda [key default]
    (case key
      [(drracket:default-filters) '(["SVG Sources" "*.tsvg"])]
      [(drracket:default-extension) "tsvg"]
      [(drracket:indentation) (dynamic-require 'sgml/village/sgmlang/indentation 'xml-indentation)]
      [(color-lexer) (dynamic-require 'sgml/village/sgmlang/lexer 'xml-lexer)]
      [else default])))

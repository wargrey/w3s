#lang typed/racket/base

(provide (all-defined-out))
(provide (rename-out [read-xml-document* read-xsd-document]))

(require "digitama/document.rkt")
(require "digitama/stdin.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xsd-document-simplify : (-> XML-Document* XML-Document)
  (lambda [xsd]
    (xml-document*->document (xml-document*-normalize xsd))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module reader racket/base
  (provide (except-out (all-from-out racket/base) read read-syntax))

  (provide (rename-out [xsd-read read]))
  (provide (rename-out [xsd-read-syntax read-syntax]))
  (provide (rename-out [xsd-info get-info]))
  
  (require sgml/village/sgmlang/reader))

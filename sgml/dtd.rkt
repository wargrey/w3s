#lang typed/racket/base

(provide (all-defined-out))
(provide (struct-out XML-DTD) read-xml-type-definition)
(provide (struct-out XML-Type) XML-Type-Entities)
(provide xml-dtd-expand xml-dtd-entity-expand)

(require "digitama/dtd.rkt")
(require "digitama/normalize.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module reader racket/base
  (provide (except-out (all-from-out racket/base) read read-syntax))

  (provide (rename-out [dtd-read read]))
  (provide (rename-out [dtd-read-syntax read-syntax]))
  (provide (rename-out [dtd-info get-info]))
  
  (require sgml/village/sgmlang/reader))

#lang typed/racket/base

(provide (all-defined-out))
(provide (struct-out XML-DTD) read-xml-type-definition)
(provide (struct-out XML-Type))

(require "digitama/dtd.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module reader racket/base
  (provide (except-out (all-from-out racket/base) read read-syntax))

  (provide (rename-out [dtd-read read]))
  (provide (rename-out [dtd-read-syntax read-syntax]))
  (provide (rename-out [dtd-info get-info]))
  
  (require sgml/village/sgmlang/reader))

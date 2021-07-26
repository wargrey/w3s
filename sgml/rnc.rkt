#lang typed/racket/base

(provide (all-defined-out) SGML-StdIn default-rnc-error-literal)
(provide #;(struct-out XML-DTD) read-rnc-declaration)
;(provide XML-Schema xml-schema? struct:xml-schema)
;(provide Open-Input-XML-XXE)

(require racket/path)
(require racket/file)
(require racket/port)

(require "digitama/rnc.rkt")
(require "digitama/stdin.rkt")
(require "digitama/schema.rkt")
(require "digitama/normalize.rkt")
(require "digitama/relaxng/rnc.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module reader racket/base
  (provide (except-out (all-from-out racket/base) read read-syntax))

  (provide (rename-out [rnc-read read]))
  (provide (rename-out [rnc-read-syntax read-syntax]))
  (provide (rename-out [rnc-info get-info]))
  
  (require sgml/village/sgmlang/reader))

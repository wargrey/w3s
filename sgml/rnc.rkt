#lang typed/racket/base

;;; RelaxNG(REgular LAnguage for XML, Next Generation) relies on both
;     strong mathematical theory on regular expressions, and
;     what adapts the hedge automata theory

(provide (all-defined-out) SGML-Stdin default-rnc-error-literal)
(provide RNC-Grammar (struct-out rnc-grammar) rnc-check-prefix? read-rnc-grammar)
(provide RNG-Grammar (struct-out rng-grammar) rnc-grammar-simplify)

(require "digitama/relaxng.rkt")
(require "digitama/relaxng/rnc.rkt")
(require "digitama/relaxng/compact.rkt")
(require "digitama/relaxng/schema.rkt")
(require "digitama/relaxng/simplification.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module reader racket/base
  (provide (except-out (all-from-out racket/base) read read-syntax))

  (provide (rename-out [rnc-read read]))
  (provide (rename-out [rnc-read-syntax read-syntax]))
  (provide (rename-out [rnc-info get-info]))
  
  (require sgml/village/sgmlang/reader))

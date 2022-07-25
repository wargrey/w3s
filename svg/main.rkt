#lang typed/racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; https://www.w3.org/TR/SVG11                                                                 ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide (all-defined-out) SVG-Source)
(provide read-svg-document read-svg-document*)

(require "digitama/document.rkt")
(require "digitama/grammar.rkt")

(module reader racket/base
  (provide (except-out (all-from-out racket/base) read read-syntax))

  (provide (rename-out [svg-read read]))
  (provide (rename-out [svg-read-syntax read-syntax]))
  (provide (rename-out [svg-info get-info]))
  
  (require svg/village/svglang/reader))

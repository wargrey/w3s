#lang typed/racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; https://www.w3.org/TR/SVG11                                                                 ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide (all-defined-out))
(provide (all-from-out sgml/xml))
(provide read-svg-document* svg-document*-normalize)

(require sgml/xml)

(require "digitama/document.rkt")

(module reader racket/base
  (provide (except-out (all-from-out racket/base) read read-syntax))

  (provide (rename-out [svg-read read]))
  (provide (rename-out [svg-read-syntax read-syntax]))
  (provide (rename-out [svg-info get-info]))
  
  (require svg/village/hashlang/reader))

#lang typed/racket/base

(require "dtd.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  (define argv : (Listof Symbol) (for/list ([arg (in-vector (current-command-line-arguments))]) (string->symbol arg)))

  (dtd-prepare-attlist!)
  
  (dtd-info-displayln argv 'ELEMENT dtd-element-list)

  (printf "~n=================== ATTLIST Group ===================~n")
  (dtd-attr-group-displayln argv))

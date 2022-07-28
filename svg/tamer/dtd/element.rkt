#lang typed/racket/base

(require "dtd.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  (define argv : (Listof Symbol) (for/list ([arg (in-vector (current-command-line-arguments))]) (string->symbol arg)))

  (dtd-setup!)

  (dtd-info-displayln argv 'ATTLIST dtd-attribute-list)
  
  (printf "~n=================== Attribute Groups ===================~n")
  (for ([e (in-list argv)])
    (printf "~a: ~n" e)
    (dtd-attr-group-displayln (dtd-attribute-list e) "    "))

  (printf "~n=================== Children ===================~n")
  (for ([e (in-list argv)])
    (printf "~a: ~a~n" e (dtd-children e))))

#lang typed/racket/base

(require "dtd.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  (define elements : (Listof Symbol)
    (let ([argv (current-command-line-arguments)])
      (cond [(= (vector-length argv) 0) (dtd-list-all-elements)]
            [else (for/list ([arg (in-vector argv)])
                    (string->symbol arg))])))

  (dtd-prepare-attlist!)

  (dtd-info-displayln elements 'ATTLIST dtd-attribute-list)
  
  (printf "~n=================== Attribute Groups ===================")
  (for ([e (in-list elements)])
    (printf "~n~a: ~n" e)
    (dtd-attr-group-displayln (dtd-attribute-list e) "    "))

  #;(printf "~n=================== Children ===================")
  #;(for ([e (in-list elements)])
    (printf "~n~a: ~a~n" e (dtd-children e))))

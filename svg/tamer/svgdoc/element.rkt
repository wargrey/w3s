#lang typed/racket/base

(require "svg.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  (define svgdb : SVG-Database (svgdoc-load-database))
  
  (define elements : (Listof Symbol)
    (let ([argv (current-command-line-arguments)])
      (cond [(= (vector-length argv) 0) (svg-database-list-all-elements svgdb)]
            [else (for/list ([arg (in-vector argv)])
                    (string->symbol arg))])))

  (svgdoc-info-displayln svgdb elements 'ATTLIST svg-database-list-attributes)
  (svgdoc-element-attgroup-displayln svgdb elements))

#lang typed/racket/base

(require "svg.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  (define svgdb : SVG-Database (svgdoc-load-database))
  
  (define categories : (Listof (Option Keyword))
    (let ([argv (current-command-line-arguments)])
      (cond [(= (vector-length argv) 0) (append (svg-database-list-all-categories svgdb) (list #false))]
            [else (for/list ([arg (in-vector argv)])
                    (string->keyword arg))])))

  (svgdoc-info-displayln svgdb categories 'ELEMENT svg-database-list-elements-of-category))

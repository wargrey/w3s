#lang typed/racket/base

(require "svg.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  (define svgdb : SVG-Database (svgdoc-load-database))
  
  (define categories : (Listof (Option Keyword))
    (let ([argv (current-command-line-arguments)])
      (cond [(= (vector-length argv) 0) (append (svg-database-list-all-categories svgdb) (list #false))]
            [else (for/list ([arg (in-vector argv)])
                    (let ([cn (string->keyword arg)])
                      (cond [(eq? cn '#:misc) #false]
                            [else cn])))])))

  (define elements : (Listof Symbol)
    (apply append
           (for/list : (Listof (Listof Symbol)) ([es (in-list categories)])
             (svg-database-list-elements-of-category svgdb es))))

  (svgdoc-info-displayln svgdb categories 'ELEMENT svg-database-list-elements-of-category)
  (svgdoc-info-displayln svgdb elements 'ATTLIST svg-database-list-attribute/groups)
  (svgdoc-element-attgroup-displayln svgdb elements))

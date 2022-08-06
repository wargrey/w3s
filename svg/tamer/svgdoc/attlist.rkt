#lang typed/racket/base

(require "svg.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  (define svgdb : SvgDoc-Database (svgdoc-load-database))
  
  (define argv : (Listof Symbol)
    (for/list ([arg (in-vector (current-command-line-arguments))])
      (string->symbol arg)))

  (svgdoc-info-displayln svgdb argv 'ELEMENT svg-database-list-elements-of-attribute)

  (printf "~n=================== Categories ===================~n")
  (svgdoc-category-displayln svgdb argv svg-database-list-elements-of-attribute svg-database-list-attributes)
  
  (printf "~n=================== ATTLIST Group ===================~n")
  (svgdoc-attr-group-displayln svgdb argv))

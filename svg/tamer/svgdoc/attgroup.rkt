#lang typed/racket/base

(require "svg.rkt")

(require racket/list)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  (define svgdb : SvgDoc-Database (svgdoc-load-database))
  
  (define groups : (Listof Keyword)
    (let ([argv (current-command-line-arguments)])
      (cond [(= (vector-length argv) 0) (svg-database-list-all-attgroups svgdb)]
            [else (remove-duplicates (for/list : (Listof Keyword) ([arg (in-vector argv)])
                                       (string->keyword arg)))])))

  (svgdoc-info-displayln svgdb groups 'ELEMENT svg-database-list-elements-of-attgroup)

  (printf "~n=================== Categories ===================~n")
  (svgdoc-category-displayln svgdb groups svg-database-list-elements-of-attgroup svg-database-list-attgroups))

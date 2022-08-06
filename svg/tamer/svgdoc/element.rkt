#lang typed/racket/base

(require "svg.rkt")

(require svg/digitama/grammar)

(require sgml/digitama/stdin)
(require sgml/digitama/tokenizer)
(require sgml/digitama/grammar)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define read-svg-element : (-> Symbol (Option SVG-Element))
  (lambda [tag]
    (define-values (/dev/xmlin version encoding standalone?) (xml-open-input-port (format "<~a />" tag) #true))
    (define tokens : (Listof XML-Token) (read-xml-tokens* /dev/xmlin tag))
    (define-values (doctype definitions grammars) (xml-syntax->content* tokens))

    (and (pair? grammars)
         (let ([elem.svg (car grammars)])
           (and (list? elem.svg)
                (xml-element*->svg-element elem.svg))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  (define svgdb : SvgDoc-Database (svgdoc-load-database))
  
  (define elements : (Listof Symbol)
    (let ([argv (current-command-line-arguments)])
      (if (> (vector-length argv) 0)
          (for/list ([arg (in-vector argv)])
            (string->symbol arg))
          (for/list ([tag (svg-database-list-all-elements svgdb)]
                     #:when (svg-unknown? (read-svg-element tag)))
            tag))))

  (svgdoc-info-displayln svgdb elements 'ATTLIST svg-database-list-attribute/groups)
  (svgdoc-element-attgroup-displayln svgdb elements)

  (displayln (length elements)))

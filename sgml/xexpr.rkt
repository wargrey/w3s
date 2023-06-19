#lang typed/racket/base

(provide (all-defined-out))
(provide (all-from-out "digitama/plain/datatype.rkt"))
(provide XExpr XExpr-AttList XExpr-Element XExpr-Attribute XExpr-Attribute-Value XExpr-Datum XExpr-PI)
(provide (struct-out XML-Document) XML-Element XML-Element-Attribute XML-Element-Attribute-Value)
(provide read-xml-document xml-document-normalize)
(provide xexpr? write-xexpr xexpr->bytes xexpr->string)
(provide xml-attributes-extract xml-attributes-extract-xmlns)
(provide xml-attributes-extract-pair xml-attributes-extract-triplet)

(require "digitama/document.rkt")
(require "digitama/plain/grammar.rkt")
(require "digitama/plain/xexpr.rkt")
(require "digitama/plain/datatype.rkt")
(require "digitama/plain/dialect.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type XExpr-Element-Children (U XML-Element XML-Subdatum))
(define-type XExpr-Attribute-Datum (U String Symbol (Listof Symbol)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml-root-xexpr : (-> (U XML-Document XML-Document*) (Option XML-Element))
  (lambda [xml]
    (define cs : (Listof XML-Content)
      (cond [(xml-document? xml) (xml-document-content xml)]
            [else (map xml-content*->datum (xml-document*-content xml))]))

    (for/or ([e (in-list cs)]
             #:when (list? e))
      e)))

(define xexpr-attr-ref : (-> XML-Element Symbol (Option XExpr-Attribute-Datum))
  (lambda [elem name]
    (define attr : (Option XML-Element-Attribute) (assq name (cadr elem)))
      (and attr
           (let ([v (cdr attr)])
             (cond [(box? v) (unbox v)]
                   [else v])))))

(define xexpr-children-seek : (-> XML-Element Symbol (U String Regexp) (Listof XML-Element))
  (lambda [docs.xml attr-name value]
    (let seek : (Listof XML-Element) ([siblings : (Listof XExpr-Element-Children) (caddr docs.xml)]
                                      [stnemele : (Listof XML-Element) null])
      (cond [(null? siblings) (reverse stnemele)]
            [else (let-values ([(self rest) (values (car siblings) (cdr siblings))])
                    (cond [(not (list? self)) (seek (cdr siblings) stnemele)]
                          [else (let ([:class (xexpr-attr-ref self attr-name)])
                                  (cond [(not (string? :class)) (seek rest (append (seek (caddr self) null) stnemele))]
                                        [(and (regexp? value) (regexp-match? value :class)) (seek rest (cons self stnemele))]
                                        [(and (string? value) (string=? value :class)) (seek rest (cons self stnemele))]
                                        [else (seek rest (append (seek (caddr self) null) stnemele))]))]))]))))

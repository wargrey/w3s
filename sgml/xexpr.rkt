#lang typed/racket/base

(provide (all-defined-out))
(provide (all-from-out "digitama/plain/datatype.rkt"))
(provide XExpr XExpr-AttList XExpr-Element XExpr-Attribute XExpr-Attribute-Value XExpr-Datum XExpr-PI)
(provide (struct-out XML-Document) XML-Element XML-Element-Attribute XML-Element-Attribute-Value)
(provide read-xml-document xml-document-normalize)
(provide xexpr? write-xexpr xexpr->bytes xexpr->string)
(provide xml-attributes-extract xml-attributes-extract-xmlns)
(provide xml-attributes-extract-pair xml-attributes-extract-triplet)
(provide raise-xml-missing-attribute-error raise-xml-missing-element-error)

(require "digitama/document.rkt")
(require "digitama/plain/grammar.rkt")
(require "digitama/plain/xexpr.rkt")
(require "digitama/plain/datatype.rkt")
(require "digitama/plain/dialect.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type XML-Element-Children (U XML-Element XML-Subdatum))
(define-type XML-Attribute-Datum (U String Symbol (Listof Symbol)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml-root-xexpr : (-> (U XML-Document XML-Document*) (Option XML-Element))
  (lambda [xml]
    (define cs : (Listof XML-Content)
      (cond [(xml-document? xml) (xml-document-content xml)]
            [else (map xml-content*->datum (xml-document*-content xml))]))

    (for/or ([e (in-list cs)]
             #:when (list? e))
      e)))

(define xml-attr-ref : (-> XML-Element Symbol (Option XML-Attribute-Datum))
  (lambda [elem name]
    (define attr : (Option XML-Element-Attribute) (assq name (cadr elem)))
      (and attr
           (let ([v (cdr attr)])
             (cond [(box? v) (unbox v)]
                   [else v])))))

(define xml-children-seek : (-> XML-Element Symbol (U String Regexp) (Listof XML-Element))
  (lambda [docs.xml attr-name value]
    (let seek : (Listof XML-Element) ([siblings : (Listof XML-Element-Children) (caddr docs.xml)]
                                      [stnemele : (Listof XML-Element) null])
      (cond [(null? siblings) (reverse stnemele)]
            [else (let-values ([(self rest) (values (car siblings) (cdr siblings))])
                    (cond [(not (list? self)) (seek (cdr siblings) stnemele)]
                          [else (let ([:class (xml-attr-ref self attr-name)])
                                  (cond [(not (string? :class)) (seek rest (append (seek (caddr self) null) stnemele))]
                                        [(and (regexp? value) (regexp-match? value :class)) (seek rest (cons self stnemele))]
                                        [(and (string? value) (string=? value :class)) (seek rest (cons self stnemele))]
                                        [else (seek rest (append (seek (caddr self) null) stnemele))]))]))]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define #:forall (T) xml-empty-children->list : (-> XML-Element (-> (Listof XML-Element-Attribute) Symbol
                                                                    (Values T (Listof XML-Element-Attribute)))
                                                    (Listof T))
  (lambda [parent attlist->datum]
    (reverse
     (for/fold ([seirtne : (Listof T) null])
               ([child (caddr parent)] #:when (list? child))
       (let-values ([(entry _) (attlist->datum (cadr child) (car child))])
         (cons entry seirtne))))))

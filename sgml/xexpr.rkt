#lang typed/racket/base

(provide (all-defined-out))
(provide (all-from-out "digitama/namespace.rkt" "digitama/xexpr/datatype.rkt"))
(provide XExpr XExpr-AttList XExpr-Element XExpr-Attribute XExpr-Attribute-Value XExpr-Datum XExpr-PI)
(provide (struct-out XML-Document) XML-Element XML-Element-Attribute XML-Element-Attribute-Value)
(provide read-xml-document xml-document-normalize)
(provide xexpr? write-xexpr xexpr->bytes xexpr->string)
(provide xml-attributes-extract xml-attributes-extract-xmlns)
(provide xml-attributes-extract-pair xml-attributes-extract-triplet)
(provide raise-xml-missing-attribute-error raise-xml-missing-element-error)

(require "digitama/document.rkt")
(require "digitama/namespace.rkt")

(require "digitama/xexpr/grammar.rkt")
(require "digitama/xexpr/xexpr.rkt")
(require "digitama/xexpr/datatype.rkt")
(require "digitama/xexpr/dialect.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type XML-Element-Children (U XML-Element XML-Subdatum))
(define-type XML-Attribute-Datum (U String Symbol (Listof Symbol)))

(define-type (XML-Children-Fold T) (-> XML-Element T Symbol T))
(define-type (XML-Children-Filter-Fold T) (-> XML-Element T Symbol (Option T)))

(define-type (XML-Children-Map T) (-> XML-Element Symbol T))
(define-type (XML-Children-Filter-Map T) (-> XML-Element Symbol (Option T)))

(define-type (XML-Empty-Children-Map T) (-> (Listof XML-Element-Attribute) Symbol
                                            (Values T (Listof XML-Element-Attribute))))

(define-type (XML-Empty-Children-Filter-Map T) (-> (Listof XML-Element-Attribute) Symbol
                                                   (Values (Option T) (Listof XML-Element-Attribute))))

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
(define #:forall (T) xml-children-fold : (-> XML-Element (XML-Children-Fold T) T T)
  (lambda [root child-fold datum0]
    (define parent-name (car root))
    (for/fold ([self : T datum0])
              ([child (in-list (caddr root))] #:when (list? child))
      (child-fold child self parent-name))))

(define #:forall (T) xml-children-filter-fold : (-> XML-Element (XML-Children-Filter-Fold T) T T)
  (lambda [root child-fold datum0]
    (define parent-name (car root))
    (for/fold ([self : T datum0])
              ([child (in-list (caddr root))] #:when (list? child))
      (or (child-fold child self parent-name) self))))

(define #:forall (T) xml-children-filter-fold* : (-> XML-Element (XML-Children-Filter-Fold T) T
                                                     (Values T (Listof XML-Element)))
  (lambda [root child-fold datum0]
    (define parent-name (car root))
    (define-values (self++ tser)
      (for/fold ([self : T datum0]
                 [tser : (Listof XML-Element) null])
                ([child (in-list (caddr root))] #:when (list? child))
        (let ([self++ (child-fold child self parent-name)])
          (cond [(not self++) (values self (cons child tser))]
                [else (values self++ tser)]))))
    (values self++ (reverse tser))))

(define #:forall (T) xml-children-map : (-> XML-Element (XML-Children-Map T) (Listof T))
  (lambda [root child-map]
    (define parent-name (car root))
    (for/list ([child (in-list (caddr root))] #:when (list? child))
      (child-map child parent-name))))

(define #:forall (T) xml-children-filter-map : (-> XML-Element (XML-Children-Filter-Map T) (Listof T))
  (lambda [root child-map]
    (define parent-name (car root))
    (reverse
     (for/fold ([seirtne : (Listof T) null])
               ([child (in-list (caddr root))] #:when (list? child))
       (let ([entry (child-map child parent-name)])
         (cond [(not entry) seirtne]
               [else (cons entry seirtne)]))))))

(define #:forall (T) xml-children-filter-map* : (-> XML-Element (XML-Children-Filter-Map T)
                                                    (Values (Listof T) (Listof XML-Element)))
  (lambda [root child-map]
    (define parent-name (car root))
    (define-values (seirtne tser)
      (for/fold ([seirtne : (Listof T) null]
                 [tser : (Listof XML-Element) null])
                ([child (in-list (caddr root))] #:when (list? child))
        (let ([entry (child-map child parent-name)])
          (cond [(not entry) (values seirtne (cons child tser))]
                [else (values (cons entry seirtne) tser)]))))
    (values (reverse seirtne) (reverse tser))))

(define #:forall (T) xml-children->map : (-> XML-Element (XML-Children-Map T) (Immutable-HashTable Symbol T))
  (lambda [root child-map]
    (define parent-name (car root))
    (for/fold ([entries : (Immutable-HashTable Symbol T) (make-immutable-hasheq)])
              ([child (in-list (caddr root))] #:when (list? child))
      (hash-set entries (car child) (child-map child parent-name)))))

(define #:forall (T) xml-children->map* : (-> XML-Element (XML-Children-Filter-Map T)
                                              (Values (Immutable-HashTable Symbol T) (Listof XML-Element)))
  (lambda [root child-map]
    (define parent-name (car root))
    (define-values (entries tser)
      (for/fold ([entries : (Immutable-HashTable Symbol T) (make-immutable-hasheq)]
                 [tser : (Listof XML-Element) null])
                ([child (in-list (caddr root))] #:when (list? child))
        (let ([entry (child-map child parent-name)])
          (cond [(not entry) (values entries (cons child tser))]
                [else (values (hash-set entries (car child) entry) tser)]))))
    (values entries (reverse tser))))

(define #:forall (T) xml-empty-children-map : (-> XML-Element (XML-Empty-Children-Map T) (Listof T))
  (lambda [parent attlist->datum]
    (for/list ([child (caddr parent)] #:when (list? child))
      (let-values ([(entry _) (attlist->datum (cadr child) (car child))])
        entry))))

(define #:forall (T) xml-empty-children-filter-map : (-> XML-Element (XML-Empty-Children-Filter-Map T) (Listof T))
  (lambda [parent attlist->datum]
    (reverse
     (for/fold ([seirtne : (Listof T) null])
               ([child (caddr parent)] #:when (list? child))
       (let-values ([(entry _) (attlist->datum (cadr child) (car child))])
         (cond [(not entry) seirtne]
               [else (cons entry seirtne)]))))))

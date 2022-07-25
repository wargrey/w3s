#lang typed/racket/base

(provide (all-defined-out))

(require racket/symbol)

(require sgml/digitama/digicore)
(require sgml/digitama/document)
(require sgml/digitama/datatype)

(require sgml/digitama/plain/grammar)
(require sgml/digitama/grammar)

(require (for-syntax racket/base))
(require (for-syntax racket/syntax))
(require (for-syntax racket/sequence))
(require (for-syntax racket/symbol))

(require (for-syntax syntax/parse))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type SVG-Element-Attribute* (Pairof Symbol XML-Element-Attribute-Value*))

(define-syntax (define-svg-attribute stx)
  (syntax-parse stx #:literals [:]
    [(_ svg-elem : SVG-Elem ([field : FieldType defval ...] ...) options ...)
     (with-syntax* ([([sfield SFieldType [sdefval ...] sfield-ref] ...)
                     #'([source (Option SVG-Source) [#false] svg-element-source]
                        [id (Option Symbol) [#false] svg-element-id]
                        [class (Listof Symbol) [null] svg-element-class]
                        [style (Option String) [#false] svg-element-style])]
                    [make-id (format-id #'svg-elem "make-~a" (syntax-e #'svg-elem))]
                    [remake-id (format-id #'svg-elem "remake-~a" (syntax-e #'svg-elem))]
                    [(field-ref ...)
                     (for/list ([<field> (in-syntax #'(field ...))])
                       (format-id <field> "~a-~a" (syntax-e #'svg-elem) (syntax-e <field>)))]
                    [([kw-args ...] [kw-reargs ...])
                     (let-values ([(args reargs)
                                   (for/fold ([args null] [reargs null])
                                             ([<field> (in-syntax #'(sfield ... field ...))]
                                              [<Argument> (in-syntax #'([sfield : SFieldType sdefval ...] ...
                                                                        [field : FieldType defval ...] ...))]
                                              [<ReArgument> (in-syntax #'([sfield : (U Void SFieldType) (void)] ...
                                                                          [field : (U Void FieldType) (void)] ...))])
                                     (let ([<kw-name> (datum->syntax <field> (string->keyword (symbol->immutable-string (syntax-e <field>))))])
                                       (values (cons <kw-name> (cons <Argument> args))
                                               (cons <kw-name> (cons <ReArgument> reargs)))))])
                       (list args reargs))])
       (syntax/loc stx
         (begin (struct svg-elem svg-element ([field : FieldType] ...)
                  #:type-name SVG-Elem
                  #:transparent
                  options ...)

                (define (make-id kw-args ...) : SVG-Elem
                  (svg-elem sfield ... field ...))

                (define (remake-id [src : SVG-Elem] kw-reargs ...) : SVG-Elem
                  (svg-elem (if (void? sfield) (sfield-ref src) sfield) ...
                      (if (void? field) (field-ref src) field) ...)))))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct svg-attribute () #:type-name SVG-Attribute #:transparent)
(struct svg:attr:uncategorized ([raw : (Listof (Pairof Symbol XML-Element-Attribute-Value))]) #:type-name SVG:Attr:Uncategorized #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml-attributes->svg-attribute : (-> (Listof XML-Element-Attribute*)
                                            (Values (Option Symbol) (Listof Symbol) (Option String)
                                                    (Listof SVG-Element-Attribute*)))
  (lambda [attrs]
    (define-values (?id ?class ?style as) (svg-attributes*-extract-core attrs))

    (values ?id ?class ?style as)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define svg-attributes*-extract-core : (-> (Listof XML-Element-Attribute*)
                                           (Values (Option Symbol) (Listof Symbol) (Option String)
                                                   (Listof SVG-Element-Attribute*)))
  (lambda [attrs]
    (let extract ([xas : (Listof XML-Element-Attribute*) attrs]
                  [?id : (Option Symbol) #false]
                  [?class : (Listof Symbol) null]
                  [?style : (Option String) #false]
                  [sas : (Listof SVG-Element-Attribute*) null])
      (cond [(null? sas) (values ?id ?class ?style sas)]
            [else (let*-values ([(self rest) (values (car xas) (cdr xas))]
                                [(name value) (values (xml:name-datum (car self)) (cdr self))])
                    (cond [(eq? name 'id) (extract rest (xml-attribute-value->symbol value) ?class ?style sas)]
                          [(eq? name 'class) (extract rest ?id (xml-attribute-value->symbols value) ?style sas)]
                          [(eq? name 'style) (extract rest ?id ?class (xml-attribute-value->string value) sas)]
                          [else (extract rest ?id ?class ?style (cons (cons name value) sas))]))]))))

(define svg-attributes*-extract : (-> (Listof SVG-Element-Attribute*) Symbol (Values (Option XML-Element-Attribute-Value*) (Listof SVG-Element-Attribute*)))
  (lambda [attrs name]
    (let extract ([as : (Listof SVG-Element-Attribute*) attrs]
                  [sa : (Listof SVG-Element-Attribute*) null])
      (cond [(null? as) (values #false sa)]
            [else (let-values ([(self rest) (values (car as) (cdr as))])
                    (cond [(eq? (car self) name) (values (cdr self) (append rest sa))]
                          [else (extract rest (cons self sa))]))]))))

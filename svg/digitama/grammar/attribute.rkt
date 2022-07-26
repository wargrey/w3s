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
(define-syntax (define-svg-attribute stx)
  (syntax-parse stx #:literals [:]
    [(_ svg-attr : SVG-Attr ([field : FieldType #:=> xml-attribute-value->datum] ...) options ...)
     (with-syntax* ([make-id (format-id #'svg-attr "make-~a" (syntax-e #'svg-attr))]
                    [remake-id (format-id #'svg-attr "remake-~a" (syntax-e #'svg-attr))]
                    [extract-id (format-id #'svg-attr "extract-~a" (syntax-e #'svg-attr))]
                    [cascade-id (format-id #'svg-attr "cascade-~a" (syntax-e #'svg-attr))]
                    [(field-ref ...)
                     (for/list ([<field> (in-syntax #'(field ...))])
                       (format-id <field> "~a-~a" (syntax-e #'svg-attr) (syntax-e <field>)))]
                    [([kw-args ...] [kw-reargs ...])
                     (let-values ([(args reargs)
                                   (for/fold ([args null] [reargs null])
                                             ([<field> (in-syntax #'(field ...))]
                                              [<Argument> (in-syntax #'([field : (Option FieldType) #false] ...))]
                                              [<ReArgument> (in-syntax #'([field : (U Void FieldType) (void)] ...))])
                                     (let ([<kw-name> (datum->syntax <field> (string->keyword (symbol->immutable-string (syntax-e <field>))))])
                                       (values (cons <kw-name> (cons <Argument> args))
                                               (cons <kw-name> (cons <ReArgument> reargs)))))])
                       (list args reargs))])
       (syntax/loc stx
         (begin (struct svg-attr svg-attribute ([field : (Option FieldType)] ...)
                  #:type-name SVG-Attr
                  #:transparent
                  options ...)

                (define (make-id kw-args ...) : SVG-Attr
                  (svg-attr field ...))

                (define (remake-id [src : SVG-Attr] kw-reargs ...) : SVG-Attr
                  (svg-attr (if (void? field) (field-ref src) field) ...))

                (define (extract-id [attrs : (Listof XML-Element-Attribute*)] [_src : (Option SVG-Attr) #false]) : (Values SVG-Attr (Listof XML-Element-Attribute*))
                  (let extract ([as : (Listof XML-Element-Attribute*) attrs]
                                [sa : (Listof XML-Element-Attribute*) null]
                                [field : (Option XML-Element-Attribute-Value*) #false] ...)
                    (if (pair? as)
                        (let*-values ([(self rest) (values (car as) (cdr as))]
                                      [(name value) (values (xml:name-datum (car self)) (cdr self))])
                          (case name
                            [(field) (extract rest sa (if (eq? 'field name) value field) ...)] ...
                            [else (extract rest (cons self sa) field ...)]))
                        (values (if (not _src)
                                    (svg-attr (and field (xml-attribute-value->datum field)) ...)
                                    (svg-attr (if (or field) (xml-attribute-value->datum field) (field-ref _src)) ...))
                                sa))))

                (define (cascade-id [parent : SVG-Attr] [child : SVG-Attr]) : SVG-Attr
                  (svg-attr (or (field-ref child) (field-ref parent)) ... )))))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct svg-attribute () #:type-name SVG-Attribute #:transparent)

(define-svg-attribute svg:attr:core : SVG:Attr:Core
  ([xml:base : String  #:=> xml-attribute-value->string]
   [xml:lang : String  #:=> xml-attribute-value->string]
   [xml:space : Symbol #:=> xml-attribute-value->symbol]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml-attributes->svg-attribute : (-> (Listof XML-Element-Attribute*)
                                            (Values (Option Symbol) (Listof Symbol) (Option String)
                                                    (Listof XML-Element-Attribute*)))
  (lambda [attrs]
    (define-values (?id ?class ?style as) (svg-attributes*-extract-core attrs))

    (values ?id ?class ?style as)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define svg-attributes*-extract-core : (-> (Listof XML-Element-Attribute*)
                                           (Values (Option Symbol) (Listof Symbol) (Option String)
                                                   (Listof XML-Element-Attribute*)))
  (lambda [attrs]
    (let extract ([xas : (Listof XML-Element-Attribute*) attrs]
                  [?id : (Option Symbol) #false]
                  [?class : (Listof Symbol) null]
                  [?style : (Option String) #false]
                  [sas : (Listof XML-Element-Attribute*) null])
      (cond [(null? sas) (values ?id ?class ?style sas)]
            [else (let*-values ([(self rest) (values (car xas) (cdr xas))]
                                [(name value) (values (xml:name-datum (car self)) (cdr self))])
                    (cond [(eq? name 'id) (extract rest (xml-attribute-value->symbol value) ?class ?style sas)]
                          [(eq? name 'class) (extract rest ?id (xml-attribute-value->symbols value) ?style sas)]
                          [(eq? name 'style) (extract rest ?id ?class (xml-attribute-value->string value) sas)]
                          [else (extract rest ?id ?class ?style (cons self sas))]))]))))

(define svg-attributes*-extract : (-> (Listof XML-Element-Attribute*) Symbol (Values (Option XML-Element-Attribute-Value*) (Listof XML-Element-Attribute*)))
  (lambda [attrs name]
    (let extract ([as : (Listof XML-Element-Attribute*) attrs]
                  [sa : (Listof XML-Element-Attribute*) null])
      (cond [(null? as) (values #false sa)]
            [else (let-values ([(self rest) (values (car as) (cdr as))])
                    (cond [(eq? (car self) name) (values (cdr self) (append rest sa))]
                          [else (extract rest (cons self sa))]))]))))

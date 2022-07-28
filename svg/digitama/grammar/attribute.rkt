#lang typed/racket/base

(provide (all-defined-out))

(require digimon/syntax)

(require sgml/digitama/digicore)
(require sgml/digitama/document)
(require sgml/digitama/datatype)

(require sgml/digitama/plain/grammar)
(require sgml/digitama/grammar)

(require (for-syntax syntax/parse))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax (define-svg-attribute stx)
  (syntax-parse stx #:literals [:]
    [(_ svg-attr : SVG-Attr ([field : FieldType #:=> xml-attribute-value->datum defval ...] ...) options ...)
     (with-syntax* ([make-attr (format-id #'svg-attr "make-~a" (syntax-e #'svg-attr))]
                    [remake-attr (format-id #'svg-attr "remake-~a" (syntax-e #'svg-attr))]
                    [extract-attr (format-id #'svg-attr "extract-~a" (syntax-e #'svg-attr))]
                    [cascade-attr (format-id #'svg-attr "cascade-~a" (syntax-e #'svg-attr))]
                    [attr-values (format-id #'svg-attr "~a-values" (syntax-e #'svg-attr))]
                    [(field-ref ...) (make-identifiers #'svg-attr #'(field ...))]
                    [([kw-args ...] [kw-reargs ...]) (make-keyword-arguments #'(field ...) #'(FieldType ...) #'([defval ...] ...))])
       (syntax/loc stx
         (begin (struct svg-attr svg-attribute ([field : FieldType] ...)
                  #:type-name SVG-Attr
                  #:transparent
                  options ...)

                (define (make-attr kw-args ...) : SVG-Attr
                  (svg-attr field ...))

                (define (remake-attr [src : SVG-Attr] kw-reargs ...) : SVG-Attr
                  (svg-attr (if (void? field) (field-ref src) field) ...))

                (define (extract-attr [attrs : (Listof XML-Element-Attribute*)] [_src : (Option SVG-Attr) #false])
                  : (Values (Option SVG-Attr) (Listof XML-Element-Attribute*))
                  (let extract ([_attrs : (Listof XML-Element-Attribute*) attrs]
                                [_srtta : (Listof XML-Element-Attribute*) null]
                                [field : (Option XML-Element-Attribute-Value*) #false] ...)
                    (cond [(pair? _attrs)
                           (let*-values ([(self rest) (values (car _attrs) (cdr _attrs))]
                                         [(name value) (values (xml:name-datum (car self)) (cdr self))])
                             (case name
                               [(field) (extract rest _srtta (if (eq? 'field name) value field) ...)] ...
                               [else (extract rest (cons self _srtta) field ...)]))]
                          [(or field ...)
                           (values (if (or _src)
                                       (svg-attr (if (or field) (xml-attribute-value->datum field) (field-ref _src)) ...)
                                       (svg-attr (or (and field (xml-attribute-value->datum field)) defval ...) ...))
                                   _srtta)]
                          [else (values #false _srtta)])))

                (define (attr-values [self : SVG-Attr]) : (Values FieldType ...)
                  (values (field-ref self) ...))

                (define (cascade-attr [parent : SVG-Attr] [child : SVG-Attr]) : SVG-Attr
                  (svg-attr (or (field-ref child) (field-ref parent)) ... )))))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct svg-attribute () #:type-name SVG-Attribute #:transparent)

(define-svg-attribute svg:attr:core : SVG:Attr:Core
  ([xml:base : (Option String)  #:=> xml-attribute-value->string #false]
   [xml:lang : (Option String)  #:=> xml-attribute-value->string #false]
   [xml:space : (Option Symbol) #:=> xml-attribute-value->symbol #false]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define svg-attributes*-extract-core : (-> (Listof XML-Element-Attribute*) (Values (Option Symbol) (Option SVG:Attr:Core) (Listof XML-Element-Attribute*)))
  (lambda [attrs]
    (let*-values ([(?id rest) (svg-attributes*-extract attrs 'id)]
                  [(?core rest) (extract-svg:attr:core rest)])
      (values (and ?id (xml-attribute-value->symbol ?id)) ?core rest))))

(define svg-attributes*-extract : (-> (Listof XML-Element-Attribute*) Symbol (Values (Option XML-Element-Attribute-Value*) (Listof XML-Element-Attribute*)))
  (lambda [attrs name]
    (let extract ([as : (Listof XML-Element-Attribute*) attrs]
                  [sa : (Listof XML-Element-Attribute*) null])
      (cond [(null? as) (values #false sa)]
            [else (let-values ([(self rest) (values (car as) (cdr as))])
                    (cond [(xml:name=:=? (car self) name) (values (cdr self) (append rest sa))]
                          [else (extract rest (cons self sa))]))]))))

(define svg-report-unrecognized-attributes : (-> XML:Name (Listof XML-Element-Attribute*) Void)
  (lambda [elem srtta]
    (for ([attr (in-list (reverse srtta))])
      (define v (cdr attr))
      (make+exn:svg:unrecognized
       (cond [(list? v) (cons elem (cons (car attr) v))]
             [else (list elem (car attr) v)])))))

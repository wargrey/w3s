#lang typed/racket/base

(provide (all-defined-out))

(require racket/math)

(require digimon/syntax)
(require digimon/dimension)

(require sgml/digitama/grammar)
(require sgml/digitama/digicore)
(require sgml/digitama/datatype)

(require css/digitama/syntax/dimension)

(require (for-syntax syntax/parse))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax (define-svg-dimension stx)
  (syntax-parse stx #:literals [:]
    [(_ dim [canonical-unit unit ...]
        (~optional [suffix ...] #:defaults ([(suffix 1) null]))
        (~optional (~seq #:with [env ...]) #:defaults ([(env 1) null]))
        (~optional (~seq #:alias [alias:id ...]) #:defaults ([(alias 1) null])))
     (with-syntax* ([(svg:attr-value*+>dim svg:attr-value*+>alias ...) (map-identifiers #'(dim alias ...) "svg:attr-value*+>~a")]
                    [(svg:attr-value*->dim svg:attr-value*->alias ...) (map-identifiers #'(dim alias ...) "svg:attr-value*->~a")]
                    [(svg:attr-dim->value  svg:attr-alias->value ...)  (map-identifiers #'(dim alias ...) "svg:attr-~a->value")]
                    [svg:dim-check (make-identifier #'dim "svg:~a-check")])
       (syntax/loc stx
         (begin (provide (rename-out [svg:attr-value*+>dim svg:attr-value*+>alias]
                                     [svg:attr-value*->dim svg:attr-value*->alias]
                                     [svg:attr-dim->value  svg:attr-alias->value]))
                ...

                (define dim-units : (Listof Symbol) '(canonical-unit unit ...))

                ;;; NOTE
                ; SVG's units are subsets of CSS's, but client-code shouldn't be bothered by that,
                ; so here we only report errors, leaving errors to the renderer, which might deal
                ; with them in a more intelligent way.    
                (define svg:dim-check : (All (a) (-> XML-Element-Attribute-Value* (Pairof (∩ a Flonum) Symbol) Boolean))
                  (lambda [value datum]
                    (let-values ([(n u) (values (car datum) (cdr datum))])
                      (when (nan? n) (make+exn:svg:range value))
                      (case u [(suffix) (void)] ... [else (unless (memq u dim-units) (make+exn:svg:unit value))]))
                    #true))

                (define svg:attr-value*+>dim : (-> XML-Element-Attribute-Value* (Pairof Nonnegative-Flonum Symbol))
                  (lambda [value]
                    (define dim (xml-attribute-value*+>dimension value 'canonical-unit))
                    (svg:dim-check value dim)
                    dim))

                (define svg:attr-value*->dim : (-> XML-Element-Attribute-Value* (Pairof Flonum Symbol))
                  (lambda [value]
                    (define dim (xml-attribute-value*->dimension value 'canonical-unit))
                    (svg:dim-check value dim)
                    dim))

                (define svg:attr-dim->value : (-> (Pairof Real Symbol) String)
                  (lambda [datum]
                    (define-values (n u) (values (real->double-flonum (car datum)) (cdr datum)))
                    (xml-attribute-dimension->value
                     (cond [(eq? u 'suffix) (cons (car datum) u)] ...
                           [(memq u dim-units) (cons n u)]
                           [else (cons (dim n u env ...) 'canonical-unit)])))))))]))

(define-syntax (define-svg-lists stx)
  (syntax-case stx [:]
    [(_ [name : Type attr->datum] ...)
     (with-syntax* ([(name->value ...) (map-identifiers #'(name ...) "svg:attr-value*->~a-list")])
       (syntax/loc stx
         (begin (define nmame->value : (-> XML-Element-Attribute-Value* (Listof Type))
                  (lambda [v]
                    (xml-attribute-value*->type-list attr->datum #px"\\s*,\\s*")))
                ...)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-svg-dimension dim:angle     [deg rad grad])
(define-svg-dimension dim:frequency [Hz kHz])
(define-svg-dimension dim:length    [px em ex in cm mm pt pc] [%] #:with [css-dimenv] #:alias [coordinate])
(define-svg-dimension dim:time      [s ms])

(define svg:attr-value*->integer : (-> XML-Element-Attribute-Value* (Option Integer))
  (lambda [v]
    (define i (xml-attribute-value*->integer v))

    (when (not i) (make+exn:svg:range v))
    i))

(define svg:attr-value*->number : (-> XML-Element-Attribute-Value* (Option Real))
  (lambda [v]
    (define i (xml-attribute-value*->number v))

    (when (not i) (make+exn:svg:range v))
    i))

(define svg:attr-value*->name : (-> XML-Element-Attribute-Value* (Option Symbol))
  (lambda [v]
    (define s (xml-attribute-value*->string v))

    (cond [(regexp-match? #px"[,()]|\\s" s) (make+exn:svg:range v) #false]
          [else (string->symbol s)])))

(define svg:attr-value*->number-pair : (-> XML-Element-Attribute-Value* (U (Pairof Real Real) Real False))
  (lambda [v]
    (define ns (xml-attribute-value*->type-list v svg:attr-value*->number))

    (cond [(null? ns) (make+exn:svg:range v) #false]
          [(null? (cdr ns)) (car ns)]
          [(pair? (cddr ns)) (make+exn:svg:range v) (cons (car ns) (cadr ns))]
          [else (cons (car ns) (cadr ns))])))

(define svg:attr-value*->integer-pair : (-> XML-Element-Attribute-Value* (U (Pairof Integer Integer) Integer False))
  (lambda [v]
    (define ns (xml-attribute-value*->type-list v svg:attr-value*->integer))

    (cond [(null? ns) (make+exn:svg:range v) #false]
          [(null? (cdr ns)) (car ns)]
          [(pair? (cddr ns)) (make+exn:svg:range v) (cons (car ns) (cadr ns))]
          [else (cons (car ns) (cadr ns))])))

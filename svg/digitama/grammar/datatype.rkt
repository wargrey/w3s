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
        (~optional (~seq #:env env ...) #:defaults ([(env 1) null])))
     (with-syntax* ([svg-attribute-value*+>id (make-identifier #'dim "svg-attribute-value*+>~a")]
                    [svg-attribute-value*->id (make-identifier #'dim "svg-attribute-value*->~a")]
                    [svg-attribute-id->value (make-identifier #'dim "svg-attribute-~a->value")]
                    [svg:dim-check (make-identifier #'dim "svg:~a-check")])
       (syntax/loc stx
         (begin (define dim-units : (Listof Symbol) '(canonical-unit unit ...))

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

                (define svg-attribute-value*+>id : (-> XML-Element-Attribute-Value* (Pairof Nonnegative-Flonum Symbol))
                  (lambda [value]
                    (define dim (xml-attribute-value*->nonnegative-dimension value 'canonical-unit))
                    (svg:dim-check value dim)
                    dim))

                (define svg-attribute-value*->id : (-> XML-Element-Attribute-Value* (Pairof Flonum Symbol))
                  (lambda [value]
                    (define dim (xml-attribute-value*->dimension value 'canonical-unit))
                    (svg:dim-check value dim)
                    dim))

                (define svg-attribute-id->value : (-> (Pairof Real Symbol) String)
                  (lambda [datum]
                    (define-values (n u) (values (real->double-flonum (car datum)) (cdr datum)))
                    (xml-attribute-dimension->value
                     (cond [(eq? u 'suffix) (cons (car datum) u)] ...
                           [(memq u dim-units) (cons n u)]
                           [else (cons (dim n u env ...) 'canonical-unit)])))))))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-svg-dimension dim:angle     [deg rad grad])
(define-svg-dimension dim:frequency [Hz kHz])
(define-svg-dimension dim:length    [px em ex in cm mm pt pc] [%] #:env css-dimenv)
(define-svg-dimension dim:time      [s ms])

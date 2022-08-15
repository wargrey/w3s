#lang typed/racket/base

(provide (all-defined-out))

(require bitmap/color)
(require bitmap/digitama/color)

(require racket/math)
(require racket/symbol)
(require racket/string)

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
                (define svg:dim-check : (All (a) (-> XML-Element-Attribute-Value* (Option (Pairof a Symbol)) Boolean))
                  (lambda [value datum]
                    (cond [(not datum) (make+exn:svg:range value)]
                          [else (let ([u (cdr datum)])
                                  (case (cdr datum)
                                    [(suffix) (void)] ...
                                    [else (unless (memq u dim-units)
                                            (make+exn:svg:unit value))]))])
                    #true))

                (define svg:attr-value*+>dim : (-> XML-Element-Attribute-Value* (Option (Pairof Nonnegative-Flonum Symbol)))
                  (lambda [value]
                    (define dim (xml:attr-value*+>dimension value 'canonical-unit))
                    (svg:dim-check value dim)
                    dim))

                (define svg:attr-value*->dim : (-> XML-Element-Attribute-Value* (Option (Pairof Flonum Symbol)))
                  (lambda [value]
                    (define dim (xml:attr-value*->dimension value 'canonical-unit))
                    (svg:dim-check value dim)
                    dim))

                (define svg:attr-dim->value : (-> (Pairof Real Symbol) String)
                  (lambda [datum]
                    (define-values (n u) (values (real->double-flonum (car datum)) (cdr datum)))
                    (xml:attr-dimension->value
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
                    (xml:attr-value*->type-list attr->datum #px"\\s*,\\s*")))
                ...)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-svg-dimension dim:angle     [deg rad grad])
(define-svg-dimension dim:frequency [Hz kHz])
(define-svg-dimension dim:length    [px em ex in cm mm pt pc] [%] #:with [css-dimenv] #:alias [coordinate])
(define-svg-dimension dim:time      [s ms])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; NOTE
; Normalizing the attributes' values require DTD or other schema,
; These APIs are designed to manually normalizing,
; Thus, the input tokens are usually XML:String instances.

(define svg:attr-value*->color : (-> XML-Element-Attribute-Value* (Option FlColor))
  (lambda [v]
    (cond [(xml:string? v) (svg-parse-color v (string-trim (xml:string-datum v)))]
          [(xml:name? v) (svg-parse-color v (symbol->immutable-string (xml:name-datum v)))]
          [(list? v) (svg-parse-color v (string-join (for/list : (Listof String) ([deadcode (in-list v)])
                                                       (symbol->immutable-string (xml:name-datum deadcode)))))]
          [else #false])))

(define svg:attr-value*->integer : (-> XML-Element-Attribute-Value* (Option Integer))
  (lambda [v]
    (define i (xml:attr-value*->integer v))

    (when (not i) (make+exn:svg:range v))
    i))

(define svg:attr-value*->number : (-> XML-Element-Attribute-Value* (Option Real))
  (lambda [v]
    (define i (xml:attr-value*->number v))

    (when (not i) (make+exn:svg:range v))
    i))

(define svg:attr-value*->name : (-> XML-Element-Attribute-Value* (Option Symbol))
  (lambda [v]
    (define s (xml:attr-value*->string v))

    (cond [(regexp-match? #px"[,()]|\\s" s) (make+exn:svg:range v) #false]
          [else (string->symbol s)])))

(define svg:attr-value*->number-pair : (-> XML-Element-Attribute-Value* (U (Pairof Real Real) Real False))
  (lambda [v]
    (define ns (xml:attr-value*->type-list v svg:attr-value*->number))

    (cond [(null? ns) (make+exn:svg:malformed v) #false]
          [(null? (cdr ns)) (car ns)]
          [(pair? (cddr ns)) (make+exn:svg:malformed v) (cons (car ns) (cadr ns))]
          [else (cons (car ns) (cadr ns))])))

(define svg:attr-value*->integer-pair : (-> XML-Element-Attribute-Value* (U (Pairof Integer Integer) Integer False))
  (lambda [v]
    (define ns (xml:attr-value*->type-list v svg:attr-value*->integer))

    (cond [(null? ns) (make+exn:svg:malformed v) #false]
          [(null? (cdr ns)) (car ns)]
          [(pair? (cddr ns)) (make+exn:svg:malformed v) (cons (car ns) (cadr ns))]
          [else (cons (car ns) (cadr ns))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define svg-function-take-off : (-> XML-Element-Attribute-Value* String String (Option String))
  (lambda [token v func-head]
    (define fh-size (string-length func-head))
    (define maxsize (- (string-length v) 1))
    (define minsize (+ fh-size 1))

    (cond [(< (string-length v) minsize) #false]
          [(and (string-prefix? v func-head) (eq? (string-ref v maxsize) #\))) (substring v fh-size maxsize)]
          [(regexp-match? #px"\\w+[(][^)]*[)]" v) (make+exn:svg:function token) #false]
          [else #false])))

(define svg-parse-color : (-> XML-Element-Attribute-Value* String (Option FlColor))
  (lambda [token v]
    (cond [(string=? v "") (make+exn:xml:missing-value token) #false]
          [(eq? (string-ref v 0) #\#)
           (let ([maybe-rgb (css-#hex-color->rgb (substring v 1))])
             (cond [(symbol? maybe-rgb) (make+exn:svg:digit token) #false]
                   [else (and maybe-rgb (hexa maybe-rgb 1.0))]))]
          [else (let ([func-body (svg-function-take-off token v "rgb(")])
                  (if (string? func-body)
                      (let ([cs (string-split (string-trim func-body) #px"\\s*,\\s*")])
                        (cond [(and (pair? cs) (pair? (cdr cs)) (pair? (cddr cs)) (null? (cdddr cs)))
                               (let*-values ([(_r _g _b) (values (car cs) (cadr cs) (caddr cs))]
                                             [(r u) (string->integer-dimension _r)])
                                 (cond [(and r (or (eq? u '||) (eq? u '%)))
                                        (let-values ([(g gu) (string->integer-dimension _g)]
                                                     [(b bu) (string->integer-dimension _b)])
                                          (cond [(not (and g b)) (make+exn:svg:range token) #false]
                                                [(not (and (eq? u gu) (eq? u bu))) (make+exn:svg:malformed token) #false]
                                                [else (let ([denominator (if (eq? u '%) 100.0 255.0)])
                                                        (rgb (/ (inexact->exact r) denominator)
                                                             (/ (inexact->exact g) denominator)
                                                             (/ (inexact->exact b) denominator)))]))]
                                       [else (make+exn:svg:range token) #false]))]
                              [else (make+exn:svg:malformed token) #false]))
                      (or (named-rgba (string->symbol v) 1.0 rgb*)
                          (named-rgba (string->symbol (string-downcase v)) 1.0 rgb*))))])))

#lang typed/racket/base

(provide (all-defined-out))

(require digimon/token)

(require sgml/digitama/digicore)
(require sgml/digitama/document)

(require sgml/digitama/plain/grammar)
(require sgml/digitama/grammar)

(require "grammar/attribute.rkt")

(require (for-syntax racket/base))
(require (for-syntax racket/syntax))
(require (for-syntax racket/sequence))
(require (for-syntax racket/symbol))

(require (for-syntax syntax/parse))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type SVG-Source (U String Symbol (Pairof (U String Symbol) (Pairof Positive-Integer Natural))))

(define-syntax (define-svg-element stx)
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
                    [([kw-sargs ...] [kw-sreargs ...])
                     (let-values ([(args reargs)
                                   (for/fold ([args null] [reargs null])
                                             ([<field> (in-syntax #'(sfield ...))]
                                              [<Argument> (in-syntax #'([sfield : SFieldType sdefval ...] ...))]
                                              [<ReArgument> (in-syntax #'([sfield : (U Void SFieldType) (void)] ...))])
                                     (let ([<kw-name> (datum->syntax <field> (string->keyword (symbol->immutable-string (syntax-e <field>))))])
                                       (values (cons <kw-name> (cons <Argument> args))
                                               (cons <kw-name> (cons <ReArgument> reargs)))))])
                       (list args reargs))]
                    [([kw-args ...] [kw-reargs ...])
                     (let-values ([(args reargs)
                                   (for/fold ([args null] [reargs null])
                                             ([<field> (in-syntax #'(field ...))]
                                              [<Argument> (in-syntax #'([field : FieldType defval ...] ...))]
                                              [<ReArgument> (in-syntax #'([field : (U Void FieldType) (void)] ...))])
                                     (let ([<kw-name> (datum->syntax <field> (string->keyword (symbol->immutable-string (syntax-e <field>))))])
                                       (values (cons <kw-name> (cons <Argument> args))
                                               (cons <kw-name> (cons <ReArgument> reargs)))))])
                       (list args reargs))])
       (syntax/loc stx
         (begin (struct svg-elem svg-element ([field : FieldType] ...)
                  #:type-name SVG-Elem
                  #:transparent
                  options ...)

                (define (make-id kw-sargs ... kw-args ...) : SVG-Elem
                  (svg-elem sfield ... field ...))

                (define (remake-id [src : SVG-Elem] kw-sreargs ... kw-reargs ...) : SVG-Elem
                  (svg-elem (if (void? sfield) (sfield-ref src) sfield) ...
                            (if (void? field) (field-ref src) field) ...)))))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct svg-element
  ([source : (Option SVG-Source)]
   [id : (Option Symbol)]
   [class : (Listof Symbol)]
   [style : (Option String)])
  #:type-name SVG-Element
  #:transparent)

(define-svg-element svg:fragment : SVG:Fragment
  ([children : (Listof SVG-Element) null]))

(define-svg-element svg:unknown : SVG:Unknown
  ([raw : XML-Element]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define svg-resolve-root : (-> (Listof XML-Content) (U String Symbol) (Option Symbol) SVG:Fragment)
  (lambda [xml.svg location svg-name]
    (let search-root ([cs : (Listof XML-Content) xml.svg])
      (cond [(null? cs) (make-svg:fragment #:source location)]
            [else (let-values ([(self rest) (values (car cs) (cdr cs))])
                    (cond [(not (list? self)) (search-root rest)]
                          [else (make-svg:fragment #:source location
                                                   #:children (xml-contents->svg-elements (caddr self)))]))]))))

(define svg-resolve-root* : (-> (Listof XML-Content*) (U String Symbol) (Option Symbol) SVG:Fragment)
  (lambda [xml.svg source svg-name]
    (let search-root ([cs : (Listof XML-Content*) xml.svg])
      (cond [(null? cs) (make-svg:fragment #:source source)]
            [else (let-values ([(self rest) (values (car cs) (cdr cs))])
                    (cond [(not (list? self)) (search-root rest)]
                          [else (let ([<svg> (car self)])
                                  (make-svg:fragment #:source (xml-token->svg-source <svg>)
                                                     #:children (xml-contents*->svg-elements (caddr self))))]))]))))

(define xml-contents->svg-elements : (-> (Listof (U XML-Content XML-Subdatum)) (Listof SVG-Element))
  (lambda [contents]
    (let filter : (Listof SVG-Element) ([xcs : (Listof (U XML-Content XML-Subdatum)) contents]
                                        [ses : (Listof SVG-Element) null])
      (cond [(null? xcs) (reverse ses)]
            [else (let-values ([(self rest) (values (car xcs) (cdr xcs))])
                    (cond [(not (list? self)) (filter rest ses)]
                          [else (let ([maybe-se (xml-element->svg-element self)])
                                  (filter rest (if (not maybe-se) ses (cons maybe-se ses))))]))]))))

(define xml-contents*->svg-elements : (-> (Listof (U XML-Content* XML-Subdatum*)) (Listof SVG-Element))
  (lambda [contents]
    (let filter : (Listof SVG-Element) ([xcs : (Listof (U XML-Content* XML-Subdatum*)) contents]
                                        [ses : (Listof SVG-Element) null])
      (cond [(null? xcs) (reverse ses)]
            [else (let-values ([(self rest) (values (car xcs) (cdr xcs))])
                    (cond [(not (list? self)) (filter rest ses)]
                          [else (let ([maybe-se (xml-element*->svg-element self)])
                                  (filter rest (if (not maybe-se) ses (cons maybe-se ses))))]))]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml-element->svg-element : (-> XML-Element (Option SVG-Element))
  (lambda [e]
    (make-svg:unknown #:raw e)))

(define xml-element*->svg-element : (-> XML-Element* (Option SVG-Element))
  (lambda [e]
    (define-values (?id ?class ?style sas) (svg-attributes*-extract-core (cadr e)))
    (make-svg:unknown #:raw (xml-element->datum e))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml-token->svg-source : (-> XML-Token (Option SVG-Source))
  (lambda [t]
    (cons (syn-token-source t)
          (cons (syn-token-line t)
                (syn-token-column t)))))

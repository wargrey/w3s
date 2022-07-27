#lang typed/racket/base

(provide (all-defined-out))

(require digimon/token)
(require digimon/syntax)

(require sgml/digitama/digicore)
(require sgml/digitama/document)

(require sgml/digitama/plain/grammar)
(require sgml/digitama/grammar)

(require "grammar/attribute.rkt")

(require (for-syntax syntax/parse))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type SVG-Source (U String Symbol (Pairof (U String Symbol) (Pairof Positive-Integer Natural))))

(define-syntax (define-svg-element stx)
  (syntax-parse stx #:literals [:]
    [(_ svg-elem : SVG-Elem
        #:attribute-categories [attr ...]
        #:attributes ([safield : SAFieldType #:=> xml-attribute-value->datum] ...)
        ([field : FieldType defval ...] ...)
        options ...)
     (with-syntax* ([([sfield SFieldType sfield-ref] ...)
                     #'([source (Option SVG-Source) svg-element-source]
                        [id (Option Symbol) svg-element-id]
                        [attr:core (Option SVG:Attr:Core) svg-element-attr:core])]
                    [make-svg (make-identifier #'svg-elem "make-~a")]
                    [remake-svg (make-identifier #'svg-elem "remake-~a")]
                    [refine-svg (make-identifier #'svg-elem "refine-~a")]
                    [(svg:attr ...) (map-identifiers #'(attr ...) "svg:attr:~a")]
                    [(extract-svg:attr ...) (map-identifiers #'(svg:attr ...) "extract-~a")]
                    [(SVG:Attr ...) (for/list ([sa (in-syntax #'(attr ...))])
                                      (format-id sa "SVG:Attr:~a" (string-titlecase (symbol->immutable-string (syntax-e sa)))))]
                    [(field-ref ...) (make-identifiers #'svg-elem #'(field ...))]
                    [(afield-ref ...) (make-identifiers #'svg-elem #'(attr ...))]
                    [(safield-ref ...) (make-identifiers #'svg-attr #'(safield ...))]
                    [([kw-sargs ...] [kw-sreargs ...]) (make-keyword-optional-arguments #'(sfield ...) #'(SFieldType ...))]
                    [([kw-aargs ...] [kw-areargs ...]) (make-keyword-optional-arguments #'(attr ...) #'(SVG:Attr ...))]
                    [([kw-saargs ...] [kw-sareargs ...]) (make-keyword-optional-arguments #'(safield ...) #'[SAFieldType ...])]
                    [([kw-args ...] [kw-reargs ...]) (make-keyword-arguments #'(field ...) #'(FieldType ...) #'([defval ...] ...))])
       (syntax/loc stx
         (begin (struct svg-elem svg-element ([attr : (Option SVG:Attr)] ... [safield : (Option SAFieldType)] ... [field : FieldType] ...)
                  #:type-name SVG-Elem
                  #:transparent
                  options ...)

                (define (make-svg kw-sargs ... kw-aargs ... kw-saargs ... kw-args ...) : SVG-Elem
                  (svg-elem sfield ... attr ... safield ... field ...))

                (define (remake-svg [src : SVG-Elem] kw-sreargs ... kw-areargs ... kw-saargs ... kw-reargs ...) : SVG-Elem
                  (svg-elem (if (void? sfield) (sfield-ref src) sfield) ...
                            (if (void? attr) (afield-ref src) attr) ...
                            (if (void? safield) (safield-ref src) safield) ...
                            (if (void? field) (field-ref src) field) ...))

                (define (refine-svg [xml.svg : XML-Element*] kw-args ...) : SVG-Elem
                  (let*-values ([(?id ?core rest) (svg-attributes*-extract-core (cadr xml.svg))]
                                [(attr rest) (extract-svg:attr rest)] ...)
                    (let extract ([_attrs : (Listof XML-Element-Attribute*) rest]
                                  [_srtta : (Listof XML-Element-Attribute*) null]
                                  [safield : (Option XML-Element-Attribute-Value*) #false] ...)
                      (if (pair? _attrs)
                          (let*-values ([(self rest) (values (car _attrs) (cdr _attrs))]
                                        [(name value) (values (xml:name-datum (car self)) (cdr self))])
                            (case name
                              [(safield) (extract rest sa (if (eq? 'safield name) value safield) ...)] ...
                              [else (extract rest (cons self _srtta) safield ...)]))
                          (svg-elem (xml-token->svg-source (car xml.svg)) ?id ?core
                                    attr ... (and safield (xml-attribute-value->datum safield)) ...
                                    field ...))))))))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct svg-element
  ([source : (Option SVG-Source)]
   [id : (Option Symbol)]
   [attr:core : (Option SVG:Attr:Core)])
  #:type-name SVG-Element
  #:transparent)

(define-svg-element svg:svg : SVG:SVG
  #:attribute-categories [core]
  #:attributes ()
  ([children : (Listof SVG-Element) null]))

(define-svg-element svg:unknown : SVG:Unknown
  #:attribute-categories []
  #:attributes ()
  ([raw : XML-Element]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define svg-resolve-root : (-> (Listof XML-Content) (U String Symbol) (Option Symbol) SVG:SVG)
  (lambda [xml.svg location svg-name]
    (let search-root ([cs : (Listof XML-Content) xml.svg])
      (cond [(null? cs) (make-svg:svg #:source location)]
            [else (let-values ([(self rest) (values (car cs) (cdr cs))])
                    (cond [(not (list? self)) (search-root rest)]
                          [else (make-svg:svg #:source location
                                              #:children (xml-contents->svg-elements (caddr self)))]))]))))

(define svg-resolve-root* : (-> (Listof XML-Content*) (U String Symbol) (Option Symbol) SVG:SVG)
  (lambda [xml.svg source svg-name]
    (let search-root ([cs : (Listof XML-Content*) xml.svg])
      (cond [(null? cs) (make-svg:svg #:source source)]
            [else (let-values ([(self rest) (values (car cs) (cdr cs))])
                    (cond [(not (list? self)) (search-root rest)]
                          [else (let ([<svg> (car self)])
                                  (make-svg:svg #:source (xml-token->svg-source <svg>)
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
    (refine-svg:unknown e #:raw (xml-element->datum e))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml-token->svg-source : (-> XML-Token (Option SVG-Source))
  (lambda [t]
    (cons (syn-token-source t)
          (cons (syn-token-line t)
                (syn-token-column t)))))

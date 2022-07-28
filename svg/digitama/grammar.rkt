#lang typed/racket/base

(provide (all-defined-out))

(require digimon/token)
(require digimon/syntax)

(require sgml/digitama/digicore)
(require sgml/digitama/document)
(require sgml/digitama/namespace)
(require sgml/digitama/datatype)

(require sgml/digitama/plain/grammar)
(require sgml/digitama/grammar)

(require "grammar/attribute.rkt")

(require (for-syntax racket/list))
(require (for-syntax syntax/parse))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type SVG-Source (U String Symbol (Pairof (U String Symbol) (Pairof Positive-Integer Natural))))

(define-syntax (define-svg-element stx)
  (syntax-parse stx #:literals [:]
    [(_ svg-elem : SVG-Elem
        #:refinement refine-svg
        #:header super ([sfield : SFieldType [sdefval ...] sfield-ref] ...)
        #:sub-header ([subfield : SubFieldType [subdefval ...] subfield-ref xml-attribute-value->subdatum] ...)
        #:attribute-categories [attr ...]
        #:attributes ([safield : SAFieldType #:=> xml-attribute-value->datum sadefval ...] ...)
        ([field : FieldType defval ...] ...)
        options ...)
     (with-syntax* ([make-svg (make-identifier #'svg-elem "make-~a")]
                    [remake-svg (make-identifier #'svg-elem "remake-~a")]
                    [(svg:attr ...) (map-identifiers #'(attr ...) "svg:attr:~a")]
                    [(extract-svg:attr ...) (map-identifiers #'(svg:attr ...) "extract-~a")]
                    [(SVG:Attr ...) (for/list ([sa (in-syntax #'(attr ...))])
                                      (format-id sa "SVG:Attr:~a" (string-titlecase (symbol->immutable-string (syntax-e sa)))))]
                    [(content-ref field-ref ...) (make-identifiers #'svg-elem #'(content field ...))]
                    [(afield-ref ...) (make-identifiers #'svg-elem #'(attr ...))]
                    [(safield-ref ...) (make-identifiers #'svg-attr #'(safield ...))]
                    [([kw-sargs ...] [kw-sreargs ...]) (make-keyword-arguments #'(sfield ...) #'(SFieldType ...) #'([sdefval ...] ...))]
                    [([kw-subargs ...] [kw-subreargs ...]) (make-keyword-arguments #'(subfield ...) #'(SubFieldType ...) #'([subdefval ...] ...))]
                    [([kw-aargs ...] [kw-areargs ...]) (make-keyword-optional-arguments #'(attr ...) #'(SVG:Attr ...))]
                    [([kw-saargs ...] [kw-sareargs ...]) (make-keyword-arguments #'(safield ...) #'(SAFieldType ...) #'([sadefval ...] ...))]
                    [([kw-args ...] [kw-reargs ...]) (make-keyword-arguments #'(field ...) #'(FieldType ...) #'([defval ...] ...))])
       (syntax/loc stx
         (begin (struct svg-elem super
                  ([attr : (Option SVG:Attr)] ...
                   [safield : (Option SAFieldType)] ...
                   [content : (Listof SVG-Element)] #| only 5 empty elements (font-face-format, font-face-name, glyphRef, hkern, vkern) |#
                   [field : FieldType] ...)
                  #:type-name SVG-Elem
                  #:transparent
                  options ...)

                (define (make-svg #:content [content : (Listof SVG-Element) null]
                                  kw-sargs ... kw-subargs ... kw-aargs ... kw-saargs ... kw-args ...) : SVG-Elem
                  (svg-elem sfield ... subfield ... attr ... safield ... content field ...))

                (define (remake-svg #:content [content : (U Void (Listof SVG-Element)) (void)]
                                    [src : SVG-Elem] kw-sreargs ... kw-subreargs ... kw-areargs ... kw-saargs ... kw-reargs ...) : SVG-Elem
                  (svg-elem (if (void? sfield) (sfield-ref src) sfield) ...
                            (if (void? subfield) (subfield-ref src) subfield) ...
                            (if (void? attr) (afield-ref src) attr) ...
                            (if (void? safield) (safield-ref src) safield) ...
                            (if (void? content) (content-ref src) content)
                            (if (void? field) (field-ref src) field) ...))

                (define (refine-svg [xml.svg : XML-Element*] kw-args ...) : SVG-Elem
                  (let*-values ([(?id ?core rest) (svg-attributes*-extract-core (cadr xml.svg))]
                                [(attr rest) (extract-svg:attr rest)] ...)
                    (let extract ([_attrs : (Listof XML-Element-Attribute*) rest]
                                  [_srtta : (Listof XML-Element-Attribute*) null]
                                  [subfield : (Option XML-Element-Attribute-Value*) #false] ...
                                  [safield : (Option XML-Element-Attribute-Value*) #false] ...)
                      (if (pair? _attrs)
                          (let*-values ([(self rest) (values (car _attrs) (cdr _attrs))]
                                        [(name value) (values (xml:name-datum (car self)) (cdr self))])
                            (case name
                              [(subfield) (extract rest _srtta (if (eq? 'subfield name) value subfield) ... safield ...)] ...
                              [(safield) (extract rest _srtta subfield ... (if (eq? 'safield name) value safield) ...)] ...
                              [else (extract rest (cons self _srtta) subfield ... safield ...)]))
                          (let ()
                            (when (pair? _srtta) (svg-report-unrecognized-attributes (car xml.svg) _srtta))
                            (svg-elem (xml-token->svg-source (car xml.svg))
                                      ?id ?core (or (and subfield (xml-attribute-value->subdatum subfield)) subdefval ...) ...
                                      attr ... (or (and safield (xml-attribute-value->datum safield)) sadefval ...) ...
                                      (xml-contents*->svg-elements (caddr xml.svg))
                                      field ...)))))))))]))

(define-syntax (define-svg-subdom stx)
  (syntax-parse stx #:literals [:]
    [(_ subsvg : SubSVG
        #:refinement refine
        #:header super ([sfield : SFieldType [sdefval ...] sfield-ref] ...)
        #:sub-header ([subfield : SubFieldType [subdefval ...] subfield-ref xml-attribute-value->subdatum] ...)
        ([tfield : TFieldType #:=> xml-attribute-value->datum tdefval ...] ...)
        #:subtree [(deftree nested-svg : Nested-SVG nested-rest ...) ...]
        (defsvg svg-elem : SVG-Elem rest ...) ...)
     (with-syntax* ([subsvg-refine (make-identifier #'subsvg "~a-refine")]
                    [(tfield-ref ...) (make-identifiers #'subsvg #'(tfield ...))]
                    [(subsvg-refine ...) (map-identifiers #'(nested-svg ...) "~a-refine")]
                    [(refine-elem ...) (map-identifiers #'(svg-elem ...) "refine-~a")]
                    [(svg-tagname ...) (for/list ([<e> (in-list (syntax->list #'(svg-elem ...)))])
                                         (datum->syntax <e> (string->symbol (substring (symbol->immutable-string (syntax-e <e>)) 4))))])
       (syntax/loc stx
         (begin (struct subsvg super ([tfield : TFieldType] ...) #:type-name SubSVG #:transparent)

                (define refine : (->* (XML-Element*) ((Listof Symbol)) (Option SubSVG))
                  (lambda [e [valid-tagnames null]]
                    (or (subsvg-refine e valid-tagnames) ...
                        (let-values ([(ns e-tagname) (xml-qname-split (xml:name-datum (car e)))])
                          (and (or (null? valid-tagnames) (memq e-tagname valid-tagnames))
                               (case e-tagname
                                 [(svg-tagname) (refine-elem e)] ...
                                 [else #false]))))))

                (deftree nested-svg : Nested-SVG
                  #:refinement subsvg-refine
                  #:header subsvg ([sfield : SFieldType [sdefval ...] sfield-ref] ...)
                  #:sub-header ([subfield : SubFieldType [subdefval ...] subfield-ref xml-attribute-value->subdatum] ...
                                [tfield : TFieldType [tdefval ...] tfield-ref xml-attribute-value->datum] ...)
                  nested-rest ...) ...

                (defsvg svg-elem : SVG-Elem
                  #:refinement refine-elem
                  #:header subsvg ([sfield : SFieldType [sdefval ...] sfield-ref] ...)
                  #:sub-header ([subfield : SubFieldType [subdefval ...] subfield-ref xml-attribute-value->subdatum] ...
                                [tfield : TFieldType [tdefval ...] tfield-ref xml-attribute-value->datum] ...)
                  rest ...) ...)))]
    [(_ subsvg : SubSVG #:refinement refine #:header super field-defs #:sub-header sub-field-defs self-field-defs elem-defs ...)
     (syntax/loc stx
       (define-svg-subdom subsvg : SubSVG
         #:refinement refine #:header super field-defs #:sub-header sub-field-defs
         self-field-defs
         #:subtree []
         elem-defs ...))]))

(define-syntax (define-svg-dom stx)
  (syntax-parse stx #:literals [:]
    [(_ svg : SVG #:refinement refine ([sfield : SFieldType sdefval ...] ...)
        #:subtree [(deftree subsvg : SubSVG subrest ...) ...]
        (defsvg svg-elem : SVG-Elem rest ...) ...)
     (with-syntax* ([(sfield-ref ...) (make-identifiers #'svg #'(sfield ...))]
                    [(subsvg-refine ...) (map-identifiers #'(subsvg ...) "~a-refine")]
                    [(refine-elem ...) (map-identifiers #'(svg-elem ...) "refine-~a")]
                    [([svg-tagname xml->svg] ...) (for/list ([<e> (in-list (drop-right (syntax->list #'(svg-elem ...)) 1 #| for svg:unknown |#))]
                                                             [<refine> (in-syntax #'(refine-elem ...))])
                                                    (list (datum->syntax <e> (string->symbol (substring (symbol->immutable-string (syntax-e <e>)) 4)))
                                                          <refine>))])
       (syntax/loc stx
         (begin (struct svg ([sfield : SFieldType] ...) #:type-name SVG #:transparent)

                (define refine : (->* (XML-Element*) ((Listof Symbol)) SVG)
                  (lambda [e [valid-tagnames null]]
                    (or (subsvg-refine e valid-tagnames) ...
                        (let-values ([(ns e-tagname) (xml-qname-split (xml:name-datum (car e)))])
                          (and (or (null? valid-tagnames) (memq e-tagname valid-tagnames))
                               (case e-tagname
                                 [(svg-tagname) (xml->svg e)] ...
                                 [else #false])))
                        (and (svg-report-unrecognized-element e)
                             (refine-svg:unknown e #:raw (xml-element->datum e))))))

                (deftree subsvg : SubSVG
                  #:refinement subsvg-refine
                  #:header svg ([sfield : SFieldType [sdefval ...] sfield-ref] ...) #:sub-header []
                  subrest ...) ...
                
                (defsvg svg-elem : SVG-Elem
                  #:refinement refine-elem
                  #:header svg ([sfield : SFieldType [sdefval ...] sfield-ref] ...) #:sub-header []
                  rest ...) ...)))]
    [(_ svg : SVG #:refinement refine field-defs elem-defs ...)
     (syntax/loc stx
       (define-svg-dom svg : SVG #:refinement refine field-defs #:subtree [] elem-defs ...))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-svg-dom svg-element : SVG-Element
  #:refinement xml-element*->svg-element
  ([source : (Option SVG-Source) #false]
   [id : (Option Symbol) #false]
   [attr:core : (Option SVG:Attr:Core) #false])

  #:subtree
  [(define-svg-subdom svg-visual-element : SVG-Visual-Element
     ([width : (Option String)  #:=> xml-attribute-value->string #false]
      [height : (Option String) #:=> xml-attribute-value->string #false])
     
     (define-svg-element svg:svg : SVG:SVG
       #:attribute-categories []
       #:attributes ()
       ()))]
  
  (define-svg-element svg:unknown : SVG:Unknown
    #:attribute-categories []
    #:attributes ()
    ([raw : XML-Element])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define svg-resolve-root : (-> (Listof XML-Content) (U String Symbol) (Option Symbol) SVG:SVG)
  (lambda [xml.svg location svg-name]
    (let search-root ([cs : (Listof XML-Content) xml.svg])
      (cond [(null? cs) (make-svg:svg #:source location)]
            [else (let-values ([(self rest) (values (car cs) (cdr cs))])
                    (cond [(not (list? self)) (search-root rest)]
                          [else (make-svg:svg #:source location
                                              #:content (xml-contents->svg-elements (caddr self)))]))]))))

(define svg-resolve-root* : (-> (Listof XML-Content*) (U String Symbol) (Option Symbol) SVG:SVG)
  (lambda [xml.svg source svg-name]
    (let search-root ([cs : (Listof XML-Content*) xml.svg])
      (cond [(null? cs) (make-svg:svg #:source source)]
            [else (let-values ([(self rest) (values (car cs) (cdr cs))])
                    (cond [(not (list? self)) (search-root rest)]
                          [else (assert (xml-element*->svg-element self) svg:svg?)]))]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

#;(define xml-element*->svg-element : (-> XML-Element* (Option SVG-Element))
  (lambda [e]
    (refine-svg:unknown e #:raw (xml-element->datum e))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml-token->svg-source : (-> XML-Token (Option SVG-Source))
  (lambda [t]
    (cons (syn-token-source t)
          (cons (syn-token-line t)
                (syn-token-column t)))))

(define svg-report-unrecognized-element : (-> XML-Element* Void)
  (lambda [e]
    (make+exn:svg:unrecognized (car e))
    (void)))

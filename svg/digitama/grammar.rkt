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

(define-for-syntax (racket->svg-name <e>)
  (datum->syntax <e> (string->symbol (substring (symbol->immutable-string (syntax-e <e>)) 4))))

(define-syntax (define-svg-element stx)
  (syntax-parse stx #:literals [:]
    [(_ svg-elem : SVG-Elem
        #:xml->svg refine-svg
        #:header super ([supfield : SupFieldType [supdefval ...] supfield-ref] ...)
        #:sub-header ([subfield : SubFieldType [subdefval ...] subfield-ref xml-attribute-value->subdatum] ...)
        #:attribute-categories [attrib ...]
        #:attributes ([field : FieldType #:=> xml-attribute-value->datum defval ...] ...)
        ([extfield : ExtFieldType extdefval ...] ...)
        options ...)
     (with-syntax* ([make-svg (make-identifier #'svg-elem "make-~a")]
                    [remake-svg (make-identifier #'svg-elem "remake-~a")]
                    [(svg:attr ...) (map-identifiers #'(attrib ...) "svg:attr:~a")]
                    [(extract-svg:attr ...) (map-identifiers #'(svg:attr ...) "extract-~a")]
                    [(SVG:Attr ...) (for/list ([sa (in-syntax #'(attrib ...))])
                                      (format-id sa "SVG:Attr:~a" (string-titlecase (symbol->immutable-string (syntax-e sa)))))]
                    [(content-ref field-ref ...) (make-identifiers #'svg-elem #'(content extfield ...))]
                    [(attfield-ref ...) (make-identifiers #'svg-elem #'(attrib ...))]
                    [(safield-ref ...) (make-identifiers #'svg-attr #'(field ...))]
                    [([kw-supargs ...] [kw-supreargs ...]) (make-keyword-arguments #'(supfield ...) #'(SupFieldType ...) #'([supdefval ...] ...))]
                    [([kw-subargs ...] [kw-subreargs ...]) (make-keyword-arguments #'(subfield ...) #'(SubFieldType ...) #'([subdefval ...] ...))]
                    [([kw-attargs ...] [kw-attreargs ...]) (make-keyword-optional-arguments #'(attrib ...) #'(SVG:Attr ...))]
                    [([kw-slfargs ...] [kw-slfreargs ...]) (make-keyword-arguments #'(field ...) #'(FieldType ...) #'([defval ...] ...))]
                    [([kw-extargs ...] [kw-extreargs ...]) (make-keyword-arguments #'(extfield ...) #'(ExtFieldType ...) #'([extdefval ...] ...))])
       (syntax/loc stx
         (begin (struct svg-elem super
                  ([attrib : (Option SVG:Attr)] ... [field : FieldType] ...
                   [content : (Listof SVG-Element)] #| only 5 empty elements (font-face-format, font-face-name, glyphRef, hkern, vkern) |#
                   [extfield : ExtFieldType] ...)
                  #:type-name SVG-Elem
                  #:transparent
                  options ...)

                (define (make-svg #:content [content : (Listof SVG-Element) null]
                                  kw-supargs ... kw-subargs ... kw-attargs ... kw-slfargs ... kw-extargs ...) : SVG-Elem
                  (svg-elem supfield ... subfield ... attrib ... field ... content extfield ...))

                (define (remake-svg #:content [content : (U Void (Listof SVG-Element)) (void)]
                                    [src : SVG-Elem] kw-supreargs ... kw-subreargs ... kw-attreargs ... kw-slfreargs ... kw-extreargs ...) : SVG-Elem
                  (svg-elem (if (void? supfield) (supfield-ref src) supfield) ...
                            (if (void? subfield) (subfield-ref src) subfield) ...
                            (if (void? attrib) (attfield-ref src) attrib) ...
                            (if (void? field) (safield-ref src) field) ...
                            (if (void? content) (content-ref src) content)
                            (if (void? extfield) (field-ref src) extfield) ...))

                (define (refine-svg [xml.svg : XML-Element*] kw-extargs ...) : SVG-Elem
                  (let*-values ([(?id ?core rest) (svg-attributes*-extract-core (cadr xml.svg))]
                                [(attrib rest) (extract-svg:attr rest)] ...)
                    (let extract ([_attrs : (Listof XML-Element-Attribute*) rest]
                                  [_srtta : (Listof XML-Element-Attribute*) null]
                                  [subfield : (Option XML-Element-Attribute-Value*) #false] ...
                                  [field : (Option XML-Element-Attribute-Value*) #false] ...)
                      (if (pair? _attrs)
                          (let*-values ([(self rest) (values (car _attrs) (cdr _attrs))]
                                        [(name value) (values (xml:name-datum (car self)) (cdr self))])
                            (case name
                              [(subfield) (extract rest _srtta (if (eq? 'subfield name) value subfield) ... field ...)] ...
                              [(field) (extract rest _srtta subfield ... (if (eq? 'field name) value field) ...)] ...
                              [else (extract rest (cons self _srtta) subfield ... field ...)]))
                          (let ()
                            (when (pair? _srtta) (svg-report-unrecognized-attributes (car xml.svg) _srtta))
                            (svg-elem (xml-token->svg-source (car xml.svg))
                                      ?id ?core (or (and subfield (xml-attribute-value->subdatum subfield)) subdefval ...) ...
                                      attrib ... (or (and field (xml-attribute-value->datum field)) defval ...) ...
                                      (xml-contents*->svg-elements (caddr xml.svg))
                                      extfield ...)))))))))]))

(define-syntax (define-svg-subdom stx)
  (syntax-parse stx #:literals [:]
    [(_ subsvg : SubSVG
        #:xml->svg refine
        #:header super ([sfield : SFieldType [sdefval ...] sfield-ref] ...)
        #:sub-header ([subfield : SubFieldType [subdefval ...] subfield-ref xml-attribute-value->subdatum] ...)
        ([tfield : TFieldType #:=> xml-attribute-value->datum tdefval ...] ...)
        #:subtree [(deftree nested-svg : Nested-SVG nested-rest ...) ...]
        (defsvg svg-elem : SVG-Elem rest ...) ...)
     (with-syntax* ([subsvg-refine (make-identifier #'subsvg "~a-refine")]
                    [(tfield-ref ...) (make-identifiers #'subsvg #'(tfield ...))]
                    [(subsvg-refine ...) (map-identifiers #'(nested-svg ...) "~a-refine")]
                    [(refine-elem ...) (map-identifiers #'(svg-elem ...) "refine-~a")]
                    [(svg-tagname ...) (map racket->svg-name (syntax->list #'(svg-elem ...)))])
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
                  #:xml->svg subsvg-refine
                  #:header subsvg ([sfield : SFieldType [sdefval ...] sfield-ref] ...)
                  #:sub-header ([subfield : SubFieldType [subdefval ...] subfield-ref xml-attribute-value->subdatum] ...
                                [tfield : TFieldType [tdefval ...] tfield-ref xml-attribute-value->datum] ...)
                  nested-rest ...) ...

                (defsvg svg-elem : SVG-Elem
                  #:xml->svg refine-elem
                  #:header subsvg ([sfield : SFieldType [sdefval ...] sfield-ref] ...)
                  #:sub-header ([subfield : SubFieldType [subdefval ...] subfield-ref xml-attribute-value->subdatum] ...
                                [tfield : TFieldType [tdefval ...] tfield-ref xml-attribute-value->datum] ...)
                  rest ...) ...)))]
    [(_ subsvg : SubSVG #:xml->svg refine #:header super field-defs #:sub-header sub-field-defs self-field-defs elem-defs ...)
     (syntax/loc stx
       (define-svg-subdom subsvg : SubSVG
         #:xml->svg refine #:header super field-defs #:sub-header sub-field-defs
         self-field-defs
         #:subtree []
         elem-defs ...))]))

(define-syntax (define-svg-dom stx)
  (syntax-parse stx #:literals [:]
    [(_ svg : SVG #:xml->svg refine ([sfield : SFieldType sdefval ...] ...)
        #:subtree [(deftree subsvg : SubSVG subrest ...) ...]
        (defsvg svg-elem : SVG-Elem rest ...) ...)
     (with-syntax* ([(sfield-ref ...) (make-identifiers #'svg #'(sfield ...))]
                    [(subsvg-refine ...) (map-identifiers #'(subsvg ...) "~a-refine")]
                    [(refine-elem ...) (map-identifiers #'(svg-elem ...) "refine-~a")]
                    [(svg-tagname ...) (map racket->svg-name (syntax->list #'(svg-elem ...)))])
       (syntax/loc stx
         (begin (struct svg ([sfield : SFieldType] ...) #:type-name SVG #:transparent)

                (define refine : (->* (XML-Element*) ((Listof Symbol)) SVG)
                  (lambda [e [valid-tagnames null]]
                    (or (subsvg-refine e valid-tagnames) ...
                        (let-values ([(ns e-tagname) (xml-qname-split (xml:name-datum (car e)))])
                          (and (or (null? valid-tagnames) (memq e-tagname valid-tagnames))
                               (case e-tagname
                                 [(svg-tagname) (refine-elem e)] ...
                                 [else #false])))
                        (and (svg-report-unrecognized-element e)
                             (svg-refine-unknown e)))))

                (deftree subsvg : SubSVG
                  #:xml->svg subsvg-refine
                  #:header svg ([sfield : SFieldType [sdefval ...] sfield-ref] ...) #:sub-header []
                  subrest ...) ...
                
                (defsvg svg-elem : SVG-Elem
                  #:xml->svg refine-elem
                  #:header svg ([sfield : SFieldType [sdefval ...] sfield-ref] ...) #:sub-header []
                  rest ...) ...)))]
    [(_ svg : SVG #:xml->svg refine field-defs elem-defs ...)
     (syntax/loc stx
       (define-svg-dom svg : SVG #:xml->svg refine field-defs #:subtree [] elem-defs ...))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-svg-dom svg-element : SVG-Element
  #:xml->svg xml-element*->svg-element
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
       ()))])

(struct svg-unknown svg-element
  ([tag : Symbol]
   [attributes : (Listof XML-Element-Attribute)]
   [content : (Listof (U XML-Content XML-Subdatum))])
  #:type-name SVG-Unknown
  #:transparent)

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

(define svg-refine-unknown : (-> XML-Element* SVG-Unknown)
  (lambda [e]
    (define-values (?id ?core rest) (svg-attributes*-extract-core (cadr e)))
    (svg-unknown (xml-token->svg-source (car e))
                 ?id ?core
                 (xml:name-datum (car e))
                 (map xml-attribute*->datum rest)
                 (map xml-mixed-content*->datum (caddr e)))))

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
    (svg-unknown #false #false #false (car e) (cadr e) (caddr e))))

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

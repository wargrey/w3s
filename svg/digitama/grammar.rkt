#lang typed/racket/base

(provide (all-defined-out))

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
  (define defname (syntax-e <e>))

  (cond [(symbol? defname) (list <e> (datum->syntax <e> (string->symbol (substring (symbol->immutable-string defname) 4))))]
        [(and (list? defname) (= (length defname) 2)) (list (car defname) (cadr defname))]
        [else (raise-syntax-error 'define-svg-element "invalid element name" <e>)]))

(define-for-syntax (racket->attr-names <a>)
  (define attrib (symbol->immutable-string (syntax-e <a>)))
  (cons (format-id <a> "attr:~a" attrib)
        (cdr (racket->svg:attr-names <a>))))

(define-syntax (extract-svg-values stx)
  (syntax-case stx [:]
    [(_ func #:pre-args [pre-argl ...] #:post-args [post-argl ...] #:with attrs omits elem #:fields [])
     (syntax/loc stx (func pre-argl ... attrs post-argl ...))]
    [(_ func #:pre-args [pre-argl ...] #:post-args [post-argl ...] #:with attrs omits elem #:fields [[field xml-attribute-value->datum defval ...] ...])
     (syntax/loc stx
       (let extract ([_attrs : (Listof XML-Element-Attribute*) attrs]
                     [_srtta : (Listof XML-Element-Attribute*) null]
                     [field : (Option XML-Element-Attribute*) #false] ...)
         (if (pair? _attrs)
             (let*-values ([(self tail) (values (car _attrs) (cdr _attrs))])
               (case (xml:name-datum (car self))
                 [(field) (with-a-field-replaced (extract tail _srtta #:fields (field ...)) #:for field #:set self)] ...
                 [else (extract tail (cons self _srtta) field ...)]))
             (func pre-argl ...
                   (svg-attribute->datum/safe field xml-attribute-value->datum (or defval ...) omits elem) ... _srtta
                   post-argl ...))))]))

(define-syntax (extract-svg-datum stx)
  (syntax-case stx [:]
    [(_ func : SVG-Elem #:pre-args [pre-argl ...] #:post-args [post-argl ...] #:with attrs #:fields [])
     (syntax/loc stx (values (func pre-argl ... post-argl ...) attrs))]
    [(_ func : SVG-Elem #:pre-args [pre-argl ...] #:post-args [post-argl ...] #:with attrs #:fields [[field xml-attribute-value->datum defval ...] ...])
     (syntax/loc stx
       (let extract : (Values SVG-Elem (Listof XML-Element-Attribute*)) ([_attrs : (Listof XML-Element-Attribute*) attrs]
                                                                         [_srtta : (Listof XML-Element-Attribute*) null]
                                                                         [field : (Option XML-Element-Attribute*) #false] ...)
         (if (pair? _attrs)
             (let*-values ([(self tail) (values (car _attrs) (cdr _attrs))])
               (case (xml:name-datum (car self))
                 [(field) (with-a-field-replaced (extract tail _srtta #:fields (field ...)) #:for field #:set self)] ...
                 [else (extract tail (cons self _srtta) field ...)]))
             (values (func pre-argl ...
                           (if field (xml-attribute-value->datum (cdr field)) (or defval ...)) ...
                           post-argl ...)
                     _srtta))))]))

(define-syntax (define-svg-element stx)
  (syntax-parse stx #:literals [:]
    [(_ [svg-elem svg-name] : SVG-Elem
        #:xml->svg refine-svg
        #:header super ([supfield : SupFieldType [supdefval ...] supfield-ref] ...)
        #:sub-header subheader-values ([subfield : SubFieldType [subdefval ...] subfield-ref] ...)
        (~optional (~seq #:attribute-categories [attrib ...]) #:defaults ([(attrib 1) null]))
        (~optional (~seq #:omit-header-fields [excfield-name ...]) #:defaults ([(excfield-name 1) null]))
        ([field : FieldType #:=> xml-attribute-value->datum defval ...] ...)
        (~optional (~seq #:extra ([extfield : ExtFieldType extdefval ...] ...))
                   #:defaults ([(extfield 1) null] [(ExtFieldType 1) null] [(extdefval 2) null]))
        options ...)
     (with-syntax* ([make-svg (make-identifier #'svg-elem "make-~a")]
                    [remake-svg (make-identifier #'svg-elem "remake-~a")]
                    [([svg:attr SVG:Attr extract-svg:attr] ...) (map racket->attr-names (syntax->list #'(attrib ...)))]
                    [([(incfield IncFieldType [incdefval ...]) ...] [(excfield ExcFieldType [excdefval ...]) ...])
                     (let ([omitted-fields (syntax->datum #'(excfield-name ...))]
                           [<subfield>s #'([subfield SubFieldType [subdefval ...]] ...)])
                       (cond [(null? omitted-fields) (list <subfield>s null)]
                             [else (let-values ([(scni scxe) (for/fold ([scni null] [scxe null])
                                                                        ([<subfield-def> (in-syntax <subfield>s)])
                                                                (if (memq (syntax-e (car (syntax->list <subfield-def>))) omitted-fields)
                                                                    (values scni (cons <subfield-def> scxe))
                                                                    (values (cons <subfield-def> scni) scxe)))])
                                     (list (reverse scni) (reverse scxe)))]))]
                    [(content-ref field-ref ...) (make-identifiers #'svg-elem #'(content extfield ...))]
                    [(attfield-ref ...) (make-identifiers #'svg-elem #'(svg:attr ...))]
                    [(safield-ref ...) (make-identifiers #'svg-elem #'(field ...))]
                    [([kw-supargs ...] [kw-supreargs ...]) (make-keyword-arguments #'(supfield ...) #'(SupFieldType ...) #'([supdefval ...] ...))]
                    [([kw-subargs ...] [kw-subreargs ...]) (make-keyword-arguments #'(incfield ...) #'(IncFieldType ...) #'([incdefval ...] ...))]
                    [([kw-attargs ...] [kw-attreargs ...]) (make-keyword-optional-arguments #'(svg:attr ...) #'(SVG:Attr ...))]
                    [([kw-slfargs ...] [kw-slfreargs ...]) (make-keyword-arguments #'(field ...) #'(FieldType ...) #'([defval ...] ...))]
                    [([kw-extargs ...] [kw-extreargs ...]) (make-keyword-arguments #'(extfield ...) #'(ExtFieldType ...) #'([extdefval ...] ...))])
       (syntax/loc stx
         (begin (struct svg-elem super
                  ([svg:attr : (Option SVG:Attr)] ... [field : FieldType] ...
                   [content : (Listof SVG-Element)] #| only 5 empty elements (font-face-format, font-face-name, glyphRef, hkern, vkern) |#
                   [extfield : ExtFieldType] ...)
                  #:type-name SVG-Elem
                  #:transparent
                  options ...)

                (define (make-svg #:content [content : (Listof SVG-Element) null]
                                  kw-supargs ... kw-subargs ... kw-attargs ... kw-slfargs ... kw-extargs ...) : SVG-Elem
                  (let ([excfield : ExcFieldType excdefval ...] ...)
                    (svg-elem supfield ... subfield ... svg:attr ... field ... content extfield ...)))

                (define (remake-svg #:content [content : (U Void (Listof SVG-Element)) (void)]
                                    [src : SVG-Elem] kw-supreargs ... kw-subreargs ... kw-attreargs ... kw-slfreargs ... kw-extreargs ...) : SVG-Elem
                  (let ([excfield : Void (void)] ...)
                    (svg-elem (if (void? supfield) (supfield-ref src) supfield) ...
                              (if (void? subfield) (subfield-ref src) subfield) ...
                              (if (void? svg:attr) (attfield-ref src) svg:attr) ...
                              (if (void? field) (safield-ref src) field) ...
                              (if (void? content) (content-ref src) content)
                              (if (void? extfield) (field-ref src) extfield) ...)))

                (define (refine-svg [xml.svg : XML-Element*] kw-extargs ...) : SVG-Elem
                  (let*-values ([(?id ?core rest) (svg-attributes*-extract-core (cadr xml.svg))]
                                [(subfield ... rest) (subheader-values rest '(excfield ...) (car xml.svg))]
                                [(svg:attr rest) (extract-svg:attr rest) #| only subfields could be omitted |#] ...)
                    (define-values (self unknowns)
                      (extract-svg-datum svg-elem : SVG-Elem
                                         #:pre-args [(xml-token->svg-source (car xml.svg)) ?id ?core subfield ... svg:attr ...]
                                         #:post-args [(xml-contents*->svg-elements (caddr xml.svg)) extfield ...]
                                         #:with rest #:fields [[field xml-attribute-value->datum defval ...] ...]))
                    (when (pair? unknowns) (svg-report-unrecognized-attributes (car xml.svg) unknowns))
                    self)))))]))

(define-syntax (define-svg-subdom stx)
  (syntax-parse stx #:literals [:]
    [(_ subsvg : SubSVG
        #:xml->svg refine
        #:header super ([sfield : SFieldType [sdefval ...] sfield-ref] ...)
        #:sub-header subheader-values ([subfield : SubFieldType [subdefval ...] subfield-ref] ...)
        (~optional (~seq #:attribute-categories [attrib ...]) #:defaults ([(attrib 1) null]))
        ([tfield : TFieldType #:=> xml-attribute-value->datum tdefval ...] ...)
        (defsvg svg-elem+name : SVG-Elem svg-elem-rest ...) ...
        (~optional (~seq #:subdom [(deftree nested-svg : Nested-SVG nested-rest ...) ...])
                   #:defaults ([(deftree 1) null] [(nested-svg 1) null] [(Nested-SVG 1) null] [(nested-rest 2) null])))
     (with-syntax* ([subsvg-refine (make-identifier #'subsvg "~a-refine")]
                    [([svg:attr SVG:Attr extract-svg:attr] ...) (map racket->attr-names (syntax->list #'(attrib ...)))]
                    [(afield-ref ...) (make-identifiers #'subsvg #'(svg:attr ...))]
                    [(tfield-ref ...) (make-identifiers #'subsvg #'(tfield ...))]
                    [(subsvg-refine ...) (map-identifiers #'(nested-svg ...) "~a-refine")]
                    [([svg-elem svg-tagname] ...) (map racket->svg-name (syntax->list #'(svg-elem+name ...)))]
                    [(refine-elem ...) (map-identifiers #'(svg-elem ...) "refine-~a")])
       (syntax/loc stx
         (begin (struct subsvg super ([svg:attr : (Option SVG:Attr)] ... [tfield : TFieldType] ...) #:type-name SubSVG #:transparent)

                (define refine : (->* (XML-Element*) ((Listof Symbol)) (Option SubSVG))
                  (lambda [e [valid-tagnames null]]
                    (or (subsvg-refine e valid-tagnames) ...
                        (let-values ([(ns e-tagname) (xml-qname-split (xml:name-datum (car e)))])
                          (case e-tagname
                            [(svg-tagname) (and (or (null? valid-tagnames) (memq e-tagname valid-tagnames)) (refine-elem e))] ...
                            [else #false])))))

                (define (extract-subheader [attrs : (Listof XML-Element-Attribute*)] [omits : (Listof Symbol) null] [elem : (Option XML:Name) #false])
                  : (Values SubFieldType ... (Option SVG:Attr) ... TFieldType ... (Listof XML-Element-Attribute*))
                  (let*-values ([(subfield ... tail) (subheader-values attrs omits elem)]
                                [(svg:attr tail) (extract-svg:attr tail omits elem)] ...)
                    (extract-svg-values values #:pre-args [subfield ... svg:attr ...] #:post-args [] #:with tail omits elem
                                        #:fields [[tfield xml-attribute-value->datum tdefval ...] ...])))

                (deftree nested-svg : Nested-SVG
                  #:xml->svg subsvg-refine
                  #:header subsvg ([sfield : SFieldType [sdefval ...] sfield-ref] ...)
                  #:sub-header extract-subheader ([subfield : SubFieldType [subdefval ...] subfield-ref] ...
                                                  [svg:attr : (Option SVG:Attr) [#false] afield-ref] ...
                                                  [tfield : TFieldType [tdefval ...] tfield-ref] ...)
                  nested-rest ...) ...

                (defsvg [svg-elem svg-tagname] : SVG-Elem
                  #:xml->svg refine-elem
                  #:header subsvg ([sfield : SFieldType [sdefval ...] sfield-ref] ...)
                  #:sub-header extract-subheader ([subfield : SubFieldType [subdefval ...] subfield-ref] ...
                                                  [svg:attr : (Option SVG:Attr) [#false] afield-ref] ...
                                                  [tfield : TFieldType [tdefval ...] tfield-ref] ...)
                  svg-elem-rest ...) ...)))]))

(define-syntax (define-svg-dom stx)
  (syntax-parse stx #:literals [:]
    [(_ svg : SVG #:xml->svg refine ([sfield : SFieldType sdefval ...] ...)
        (defsvg svg-elem+name : SVG-Elem rest ...) ...
        (~optional (~seq #:subdom [(deftree subsvg : SubSVG subrest ...) ...])
                   #:defaults ([(deftree 1) null] [(subsvg 1) null] [(SubSVG 1) null] [(subrest 2) null])))
     (with-syntax* ([(sfield-ref ...) (make-identifiers #'svg #'(sfield ...))]
                    [(subsvg-refine ...) (map-identifiers #'(subsvg ...) "~a-refine")]
                    [([svg-elem svg-tagname] ...) (map racket->svg-name (syntax->list #'(svg-elem+name ...)))]
                    [(refine-elem ...) (map-identifiers #'(svg-elem ...) "refine-~a")])
       (syntax/loc stx
         (begin (struct svg ([sfield : SFieldType] ...) #:type-name SVG #:transparent)

                (define refine : (->* (XML-Element*) ((Listof Symbol)) SVG)
                  (lambda [e [valid-tagnames null]]
                    (or (subsvg-refine e valid-tagnames) ...
                        (let-values ([(ns e-tagname) (xml-qname-split (xml:name-datum (car e)))])
                          (case e-tagname
                            [(svg-tagname) (and (or (null? valid-tagnames) (memq e-tagname valid-tagnames)) (refine-elem e))] ...
                            [else #false]))
                        (and (svg-report-unrecognized-element e)
                             (svg-refine-unknown e)))))

                (define (extract-subheader [attrs : (Listof XML-Element-Attribute*)] [omits : (Listof Symbol) null] [elem : (Option XML:Name) #false])
                  : (Values (Listof XML-Element-Attribute*))
                  attrs)

                (deftree subsvg : SubSVG
                  #:xml->svg subsvg-refine
                  #:header svg ([sfield : SFieldType [sdefval ...] sfield-ref] ...)
                  #:sub-header extract-subheader []
                  subrest ...) ...
                
                (defsvg [svg-elem svg-tagname] : SVG-Elem
                  #:xml->svg refine-elem
                  #:header svg ([sfield : SFieldType [sdefval ...] sfield-ref] ...)
                  #:sub-header extract-subheader []
                  rest ...) ...)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-svg-dom svg-element : SVG-Element
  #:xml->svg xml-element*->svg-element
  ([source : (Option SVG-Source) #false]
   [id : (Option Symbol) #false]
   [attr:core : (Option SVG:Attr:Core) #false])

  #:subdom
  [(define-svg-subdom svg-stylish-element : SVG-Stylish-Element
     #:attribute-categories [style] ()

     #:subdom
     [(define-svg-subdom svg-descriptive-element : SVG-Descriptive-Element ()
        (define-svg-element svg:desc : SVG:Desc ())
        (define-svg-element svg:title : SVG:Title ())
        (define-svg-element svg:metadata : SVG:Metadata #:omit-header-fields [class style] ()))

      (define-svg-subdom svg-visual-element : SVG-Visual-Element
        ([x : (Option String)      #:=> xml-attribute-value->string #false]
         [y : (Option String)      #:=> xml-attribute-value->string #false]
         [width : (Option String)  #:=> xml-attribute-value->string #false]
         [height : (Option String) #:=> xml-attribute-value->string #false])
     
        (define-svg-element svg:svg : SVG:SVG
          #:attribute-categories []
          ([baseProfile : (Option String) #:=> xml-attribute-value->string #false]
           [contentScriptType : (Option String) #:=> xml-attribute-value->string #false]
           [contentStyleType : (Option String) #:=> xml-attribute-value->string #false]
           [preserveAspectRatio : (Option String) #:=> xml-attribute-value->string #false]
           [version : (Option String) #:=> xml-attribute-value->string #false]
           [viewBox : (Option String) #:=> xml-attribute-value->string #false]
           [zoomAndPan : (Option String) #:=> xml-attribute-value->string #false])))])

   (define-svg-subdom svg-light-source-element : SVG-Light-Source-Element ()
     (define-svg-element [svg:distant-light feDistantLight] : SVG:DistantLight
       ([azimuth : (Option String)   #:=> xml-attribute-value->string #false]
        [elevation : (Option String) #:=> xml-attribute-value->string #false]))

     (define-svg-element [svg:point-light fePointLight] : SVG:PointLight
       ([x : (Option String) #:=> xml-attribute-value->string #false]
        [y : (Option String) #:=> xml-attribute-value->string #false]
        [z : (Option String) #:=> xml-attribute-value->string #false]))
     
     (define-svg-element [svg:spot-light feSpotLight] : SVG:SpotLight
       ([x : (Option String) #:=> xml-attribute-value->string #false]
        [y : (Option String) #:=> xml-attribute-value->string #false]
        [z : (Option String) #:=> xml-attribute-value->string #false]
        [pointsAtX : (Option String) #:=> xml-attribute-value->string #false]
        [pointsAtY : (Option String) #:=> xml-attribute-value->string #false]
        [pointsAtZ : (Option String) #:=> xml-attribute-value->string #false]
        [specularExponent : (Option String) #:=> xml-attribute-value->string #false]
        [limitingConeAngle : (Option String) #:=> xml-attribute-value->string #false])))

   (define-svg-subdom svg-animation-element : SVG-Animation-Element
     #:attribute-categories [condition external xlink animation-event animation-timing] ()

     (define-svg-element svg:animate : SVG:Animate
       #:attribute-categories [animation-target animation-value animation-addition presentation] ())

     (define-svg-element svg:set : SVG:Set
       #:attribute-categories [animation-target]
       ([to : (Option String) #:=> xml-attribute-value->string #false]))
     
     (define-svg-element [svg:animate-color animateColor] : SVG:AnimateColor
       #:attribute-categories [animation-target animation-value animation-addition presentation] ())

     (define-svg-element [svg:animate-motion animateMotion] : SVG:AnimateMotion
       #:attribute-categories [animation-value animation-addition]
       ([path : (Option String) #:=> xml-attribute-value->string #false]
        [keyPoints : (Option String) #:=> xml-attribute-value->string #false]
        [rotate : (Option String) #:=> xml-attribute-value->string #false]
        [origin : (Option String) #:=> xml-attribute-value->string #false]))
     
     (define-svg-element [svg:animate-transform animateTransform] : SVG:AnimateTransform
       #:attribute-categories [animation-target animation-value animation-addition presentation]
       ([type : (Option String) #:=> xml-attribute-value->string #false])))])

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

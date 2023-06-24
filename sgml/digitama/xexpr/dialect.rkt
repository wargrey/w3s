#lang typed/racket/base

(provide (all-defined-out))
(provide (all-from-out "../shared/dialect.rkt"))

(require digimon/syntax)
(require digimon/function)

(require "datatype.rkt")

(require "../shared/dialect.rkt")
(require "../namespace.rkt")
(require "grammar.rkt")

(require (for-syntax racket/list))
(require (for-syntax syntax/parse))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type (XML-Attribute-Extract XML-Attr)
  (->* ((Listof XML-Element-Attribute)) ((Option Symbol) (Listof Symbol))
       (Values XML-Attr (Listof XML-Element-Attribute))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax (extract-dom-values stx)
  (syntax-case stx [:]
    [(_ func #:pre-args [pre-argl ...] #:post-args [post-argl ...] #:with attrs omits elem report-unknown report-range-exn #:fields [])
     (syntax/loc stx (func pre-argl ... attrs post-argl ...))]
    [(_ func #:pre-args [pre-argl ...] #:post-args [post-argl ...]
        #:with attrs omits elem report-unknown report-range-exn
        #:fields [[field FieldType xml-value->datum defval ...] ...])
     (syntax/loc stx
       (let extract ([_attrs : (Listof XML-Element-Attribute) attrs]
                     [_srtta : (Listof XML-Element-Attribute) null]
                     [field : (Option XML-Element-Attribute) #false] ...)
         (if (pair? _attrs)
             (let*-values ([(self tail) (values (car _attrs) (cdr _attrs))])
               (case (car self)
                 [(field) (with-a-field-replaced (extract tail _srtta #:fields (field ...)) #:for field #:set self)] ...
                 [else (extract tail (cons self _srtta) field ...)]))
             (func pre-argl ...
                   ((inst xml-attribute->datum/safe FieldType) 'func 'field field xml-value->datum report-unknown elem omits defval ...) ...
                   _srtta
                   post-argl ...))))]))

(define-syntax (extract-dom-datum stx)
  (syntax-case stx [:]
    [(_ func : DOM-Elem #:pre-args [pre-argl ...] #:post-args [post-argl ...] #:with attrs report-range-exn #:fields [])
     (syntax/loc stx (values (func pre-argl ... post-argl ...) attrs))]
    [(_ func : DOM-Elem #:pre-args [pre-argl ...] #:post-args [post-argl ...] #:with attrs report-range-exn #:fields [[field FieldType xml->datum defval ...] ...])
     (syntax/loc stx
       (let extract : (Values DOM-Elem (Listof XML-Element-Attribute)) ([_attrs : (Listof XML-Element-Attribute) attrs]
                                                                        [_srtta : (Listof XML-Element-Attribute) null]
                                                                        [field : (Option XML-Element-Attribute) #false] ...)
         (if (pair? _attrs)
             (let*-values ([(self tail) (values (car _attrs) (cdr _attrs))])
               (case (car self)
                 [(field) (with-a-field-replaced (extract tail _srtta #:fields (field ...)) #:for field #:set self)] ...
                 [else (extract tail (cons self _srtta) field ...)]))
             (values (func pre-argl ...
                           ((inst xml-attribute->datum/safe FieldType) 'func 'field field xml->datum #false #false null defval ...) ...
                           post-argl ...)
                     _srtta))))]))

(define-syntax (dom-attribute-list stx)
  (syntax-case stx [:]
    [(_ [field value #:~> datum->value] ...)
     (syntax/loc stx
       (for/list : (Listof (Pairof Symbol String))
         ([fname (in-list ((inst list Symbol) 'field ...))]
          [datum (in-list ((inst list (Option String)) (and value (datum->value value)) ...))]
          #:when datum)
         (cons fname datum)))]
    [(_ header [attr #:=> attr->xml] ...)
     (syntax/loc stx
       (for/fold ([attrs : (Listof (Pairof Symbol String)) header])
                 ([alist (in-list ((inst list (Option (Listof (Pairof Symbol String)))) (and attr (attr->xml attr)) ...))]
                  #:when alist)
         (append attrs alist)))]))

(define-syntax (define-dom-element stx)
  (syntax-parse stx #:literals [:]
    [(_ dom-elem : DOM-Elem
        #:element-interface [refine-element dom-flatten-attributes #:report report-unknown report-range-exn]
        #:head [super ([hfield : HFieldType [hdefval ...] hfield-ref] ...)
                      #:with header-values header->xexpr #:-> DOM-Element]
        #:body [([bfield : BFieldType [bdefval ...]] ...) #:values body-values]
        (~optional (~seq #:attribute-categories [[attrib : Attrib extract-attrib attrib->xexpr] ...])
                   #:defaults ([(attrib 1) null] [(Attrib 1) null] [(extract-attrib 1) null] [(attrib->xexpr 1) null]))
        (~optional (~seq #:omit-header-fields [excfield-name ...])
                   #:defaults ([(excfield-name 1) null]))
        ([field : FieldType
                (~optional (~seq #:= defval ...) #:defaults ([(defval 1) null]))
                (~seq #:<-> xml->datum (~optional datum->xml #:defaults ([datum->xml #'xml:attr-datum->value])))] ...)
        (~optional (~seq #:extra ([efield : EFieldType edefval ...] ...))
                   #:defaults ([(efield 1) null] [(EFieldType 1) null] [(edefval 2) null]))
        options ...)
     (with-syntax* ([make-dom (make-identifier #'dom-elem "make-~a")]
                    [remake-dom (make-identifier #'dom-elem "remake-~a")]
                    [([(ifield IFieldType [idefval  ...] ifield-ref) ...] [(excfield ExcFieldType [excdefval ...] excfield-ref) ...])
                     (let ([omitted-fields (syntax->datum #'(excfield-name ...))]
                           [<supfield>s (syntax->list #'([hfield HFieldType [hdefval ...] hfield-ref] ...))])
                       (cond [(null? omitted-fields) (list <supfield>s null)]
                             [(null? <supfield>s) (list <supfield>s null)]
                             [else (let-values ([(scni scxe) (for/fold ([scni (car <supfield>s)] [scxe null])
                                                                       ([<supfield> (in-list (cdr <supfield>s))])
                                                               (if (memq (syntax-e (car (syntax->list <supfield>))) omitted-fields)
                                                                   (values scni (cons <supfield> scxe))
                                                                   (values (cons <supfield> scni) scxe)))])
                                     (list (reverse scni) (reverse scxe)))]))]
                    [(attfield-ref ...) (make-identifiers #'dom-elem #'(attrib ...))]
                    [(selfield-ref ...) (make-identifiers #'dom-elem #'(field ...))]
                    [(bdyfield-ref ...) (make-identifiers #'dom-elem #'(bfield ...))]
                    [(extfield-ref ...) (make-identifiers #'dom-elem #'(efield ...))]
                    [([kw-hdrargs ...] [kw-hdrreargs ...]) (make-keyword-arguments #'(ifield ...) #'(IFieldType ...) #'([idefval  ...] ...))]
                    [([kw-attargs ...] [kw-attreargs ...]) (make-keyword-optional-arguments #'(attrib ...) #'(Attrib ...))]
                    [([kw-bdyargs ...] [kw-bdyreargs ...]) (make-keyword-arguments #'(bfield ...) #'(BFieldType ...) #'([bdefval ...] ...))]
                    [([kw-extargs ...] [kw-extreargs ...]) (make-keyword-arguments #'(efield ...) #'(EFieldType ...) #'([edefval ...] ...))]
                    [([kw-slfargs ...] [kw-slfreargs ...]) (make-keyword-arguments #'(field ...) #'(FieldType ...) #'([defval ...] ...))])
       (syntax/loc stx
         (begin (struct dom-elem super
                  ([attrib : (Option Attrib)] ...
                   [field : FieldType] ...
                   [bfield : BFieldType] ...
                   [efield : EFieldType] ...)
                  #:type-name DOM-Elem
                  #:transparent
                  #:property prop:custom-write
                  (λ [[self : DOM-Elem] [/dev/stdout : Output-Port] [mode : (U Zero One Boolean)]]
                    (xml-element-custom-write 'dom-elem /dev/stdout mode
                                              (list 'hfield ... 'attrib ... 'field ...)
                                              (list (hfield-ref self) ... (attfield-ref self) ... (selfield-ref self) ...)
                                              (list (bdyfield-ref self) ... (extfield-ref self) ...)))
                  options ...)

                (define (make-dom kw-hdrargs ... kw-attargs ... kw-slfargs ... kw-bdyargs ... kw-extargs ...) : DOM-Elem
                  (let ([excfield : ExcFieldType excdefval ...] ...)
                    (dom-elem hfield ... attrib ... field ... bfield ... efield ...)))

                (define (remake-dom [self : DOM-Elem] kw-hdrreargs ... kw-attreargs ... kw-slfreargs ... kw-bdyreargs ... kw-extreargs ...) : DOM-Elem
                  (let ([excfield (excfield-ref self)] ...)
                    (dom-elem (if (void? hfield) (hfield-ref self) hfield) ...
                              (if (void? attrib) (attfield-ref self) attrib) ...
                              (if (void? field) (selfield-ref self) field) ...
                              (if (void? bfield) (bdyfield-ref self) bfield) ...
                              (if (void? efield) (extfield-ref self) efield) ...)))

                (define (refine-element [xml.dom : XML-Element*] kw-extargs ...) : DOM-Elem
                  (let*-values ([(hfield ... rest) (header-values (cadr xml.dom) (car xml.dom) '(excfield ...))]
                                [(bfield ...) (body-values (caddr xml.dom) (car xml.dom))] #| TODO: deal with invalid children |#
                                [(attrib rest) (extract-attrib rest) #| only subfields could be omitted |#] ...)
                    (define-values (self unknowns)
                      (extract-dom-datum dom-elem : DOM-Elem
                                         #:pre-args [hfield ... attrib ...]
                                         #:post-args [bfield ... efield ...]
                                         #:with rest report-range-exn
                                         #:fields [[field FieldType xml->datum defval ...] ...]))
                    (when (pair? unknowns) (report-unknown (car xml.dom) unknowns))
                    self))

                (define (dom-flatten-attributes [self : DOM-Elem]) : (Listof (Pairof Symbol String))
                  (append (dom-attribute-list (header->xexpr self) [(attfield-ref self) #:=> attrib->xexpr] ...)
                          (dom-attribute-list [field (selfield-ref self) #:~> datum->xml] ...))))))]))

(define-syntax (define-xml-subdom stx)
  (syntax-parse stx #:literals [:]
    [(_ subdom : SubDOM
        #:subdom-interface [refine flattern-attributes #:report report-unknown report-range-exn]
        #:head [super ([hfield : HFieldType [hdefval ...] hfield-ref] ...) #:with header-values header->xexpr #:-> DOM-Element]
        #:body [([bfield : BFieldType [bdefval ...]] ...) #:values extract-body]
        (~optional (~seq #:attribute-categories [[attrib : Attrib extract-attrib attrib->xexpr] ...])
                   #:defaults ([(attrib 1) null] [(Attrib 1) null] [(extract-attrib 1) null] [(attrib->xexpr 1) null]))
        ([mfield : MFieldType
                 (~optional (~seq #:= mdefval ...) #:defaults ([(mdefval 1) null]))
                 (~seq #:<-> xml->mdatum (~optional mdatum->xml #:defaults ([mdatum->xml #'xml:attr-datum->value])))] ...)
        (~optional (~seq #:subdom [(deftree subsubdom : SubsubDOM subsubrest ...) ...])
                   #:defaults ([(deftree 1) null] [(subsubdom 1) null] [(SubsubDOM 1) null] [(subsubrest 2) null]))
        (defdom [dom-elem dom-tagname] : DOM-Elem #:with dom-flatten-attributes dom-elem-rest ...) ...)
     (with-syntax* ([subdom-refine (make-identifier #'subdom "~a-refine")]
                    [(afield-ref ...) (make-identifiers #'subdom #'(attrib ...))]
                    [(mfield-ref ...) (make-identifiers #'subdom #'(mfield ...))]
                    [(subdom-refine ...) (map-identifiers #'(subsubdom ...) "~a-refine")]
                    [(subdom-flatten ...) (map-identifiers #'(subsubdom ...) "~a-flatten-attributes")]
                    [(refine-elem ...) (map-identifiers #'(dom-elem ...) "refine-~a")]
                    [(subdom? ...) (map-identifiers #'(subsubdom ...) "~a?")]
                    [(dom-elem? ...) (map-identifiers #'(dom-elem ...) "~a?")])
       (syntax/loc stx
         (begin (struct subdom super ([attrib : (Option Attrib)] ... [mfield : MFieldType] ...) #:type-name SubDOM #:transparent)

                (define refine : (->* (XML-Element*) ((Listof Symbol)) (Option SubDOM))
                  (lambda [e [valid-tagnames null]]
                    (or (subdom-refine e valid-tagnames) ...
                        (let-values ([(ns e-tagname) (xml-qname-split (car e))])
                          (case e-tagname
                            [(dom-tagname) (and (or (null? valid-tagnames) (memq e-tagname valid-tagnames)) (refine-elem e))] ...
                            [else #false])))))

                (define flattern-attributes : (-> DOM-Element (Listof (Pairof Symbol String)))
                  (lambda [self]
                    (cond [(subdom? self) (subdom-flatten self)] ...
                          [(dom-elem? self) (dom-flatten-attributes self)] ...
                          [else null])))

                (define (extract-header [attrs : (Listof XML-Element-Attribute)] [elem : (Option Symbol) #false] [omits : (Listof Symbol) null])
                  : (Values HFieldType ... (Option Attrib) ... MFieldType ... (Listof XML-Element-Attribute))
                  (let*-values ([(hfield ... tail) (header-values attrs elem omits)]
                                [(attrib tail) (extract-attrib tail elem omits)] ...)
                    (extract-dom-values values #:pre-args [hfield ... attrib ...] #:post-args [] #:with tail omits elem report-unknown report-range-exn
                                        #:fields [[mfield MFieldType xml->mdatum mdefval ...] ...])))

                (define (header->xml-attributes [self : SubDOM]) : (Listof (Pairof Symbol String))
                  (append (dom-attribute-list (header->xexpr self) [(afield-ref self) #:=> attrib->xexpr] ...)
                          (dom-attribute-list [mfield (mfield-ref self) #:~> mdatum->xml] ...)))

                (deftree subsubdom : SubsubDOM
                  #:subdom-interface [subdom-refine subdom-flatten #:report report-unknown report-range-exn]
                  #:head [subdom ([hfield : HFieldType [hdefval ...] hfield-ref] ...
                                  [attrib : (Option Attrib) [#false] afield-ref] ...
                                  [mfield : MFieldType [mdefval ...] mfield-ref] ...)
                                 #:with extract-header header->xml-attributes #:-> DOM-Element]
                  #:body [([bfield : BFieldType [bdefval ...]] ...) #:values extract-body]
                  subsubrest ...) ...

                (defdom dom-elem : DOM-Elem
                  #:element-interface [refine-elem dom-flatten-attributes #:report report-unknown report-range-exn]
                  #:head [subdom ([hfield : HFieldType [hdefval ...] hfield-ref] ...
                                  [attrib : (Option Attrib) [#false] afield-ref] ...
                                  [mfield : MFieldType [mdefval ...] mfield-ref] ...)
                                 #:with extract-header header->xml-attributes #:-> DOM-Element]
                  #:body [([bfield : BFieldType [bdefval ...]] ...) #:values extract-body]
                  dom-elem-rest ...) ...)))]))

(define-syntax (define-xml-dom stx)
  (syntax-parse stx #:literals [:]
    [(_ dom-element : DOM-Element
        #:dom-interface [[header-extract head->xexpr] refine flatten-attributes report-unknown report-range-exn body-extract]
        ([hfield : HFieldType hdefval ...] ...) ([bfield : BFieldType bdefval ...] ...)
        #:unknown [dom-unknown : DOM-Unknown #:refine refine-unknown]
        (~optional (~seq #:subdom [(deftree subdom : SubDOM subrest ...) ...])
                   #:defaults ([(deftree 1) null] [(subdom 1) null] [(SubDOM 1) null] [(subrest 2) null]))
        (defelem [dom-elem dom-tagname] : DOM-Elem #:with dom-flatten-attributes dom-rest-definition ...) ...)
     (with-syntax* ([(hfield-ref ...) (make-identifiers #'dom-element #'(hfield ...))]
                    [(subdom-refine ...) (map-identifiers #'(subdom ...) "~a-refine")]
                    [(subdom-flatten ...) (map-identifiers #'(subdom ...) "~a-flatten-attributes")]
                    [(refine-elem ...) (map-identifiers #'(dom-elem ...) "refine-~a")]
                    [(subdom? ...) (map-identifiers #'(subdom ...) "~a?")]
                    [(dom-elem? ...) (map-identifiers #'(dom-elem ...) "~a?")])
       (syntax/loc stx
         (begin (struct dom-element ([hfield : HFieldType] ...) #:type-name DOM-Element #:transparent)

                (struct dom-unknown dom-element
                  ([tag : Symbol]
                   [attributes : (Listof XML-Element-Attribute)]
                   [content : (Listof (U XML-Content XML-Subdatum))])
                  #:type-name DOM-Unknown
                  #:transparent)

                (define refine : (->* (XML-Element*) ((Listof Symbol)) DOM-Element)
                  (lambda [e [valid-tagnames null]]
                    (or (subdom-refine e valid-tagnames) ...
                        (let-values ([(ns e-tagname) (xml-qname-split (car e))])
                          (case e-tagname
                            [(dom-tagname) (and (or (null? valid-tagnames) (memq e-tagname valid-tagnames)) (refine-elem e))] ...
                            [else #false]))
                        (and (report-unknown e)
                             (refine-unknown e)))))

                (define flatten-attributes : (-> DOM-Element (Listof (Pairof Symbol String)))
                  (lambda [self]
                    (cond [(subdom? self) (subdom-flatten self)] ...
                          [(dom-elem? self) (dom-flatten-attributes self)] ...
                          [else null])))

                (define refine-unknown : (-> XML-Element* DOM-Unknown)
                  (lambda [e]
                    (let-values ([(?src ?id ?core other-attrs) (extract-header (cadr e) (car e) null)])
                      (dom-unknown ?src ?id ?core
                                   (car e)
                                   (map xml-attribute*->datum other-attrs)
                                   (map xml-mixed-content*->datum (caddr e))))))

                (define extract-header
                  : (->* ((Listof XML-Element-Attribute)) ((Option Symbol) (Listof Symbol))
                         (Values HFieldType ... (Listof XML-Element-Attribute)))
                  (lambda [attrs [elem #false] [omits null]]
                    (let-values ([(?src) (and elem (xml-token->source elem))]
                                 [(?id ?core other-attrs) (header-extract attrs)])
                      (values ?src ?id ?core other-attrs))))

                (define header->xml-attributes : (-> DOM-Element (Listof (Pairof Symbol String)))
                  (lambda [self]
                    (head->xexpr (hfield-ref self) ...)))

                (deftree subdom : SubDOM
                  #:subdom-interface [subdom-refine subdom-flatten #:report report-unknown report-range-exn]
                  #:head [dom-element ([hfield : HFieldType [hdefval ...] hfield-ref] ...)
                                      #:with extract-header header->xml-attributes #:-> DOM-Element]
                  #:body [([bfield : BFieldType [bdefval ...]] ...) #:values body-extract]
                  subrest ...) ...
                
                (defelem dom-elem : DOM-Elem
                  #:element-interface [refine-elem dom-flatten-attributes #:report report-unknown report-range-exn]
                  #:head [dom-element ([hfield : HFieldType [hdefval ...] hfield-ref] ...)
                                      #:with extract-header header->xml-attributes #:-> DOM-Element]
                  #:body [([bfield : BFieldType [bdefval ...]] ...) #:values body-extract]
                  dom-rest-definition ...) ...)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax (define-xml-attribute-extract stx)
  (syntax-parse stx #:literals [:]
    [(_ extract-attr : XML-Attr #:inline #:with report-unknown report-range-exn (xml-attr) mandatory-fields)
     (syntax/loc stx
       (define extract-attr : (->* ((Listof XML-Element-Attribute)) ((Option Symbol) (Listof Symbol)) (Values XML-Attr (Listof XML-Element-Attribute)))
         (let ([singleton (xml-attr)])
           (lambda [attrs [elem #false] [omits null]]
             (values singleton attrs)))))]
    [(_ extract-attr : XML-Attr #:inline #:with report-unknown report-range-exn
        (xml-attr [field FieldType xml-attr-value->datum defval ...] ...)
        mandatory-fields AltReturnType)
     (syntax/loc stx
       (define extract-attr : (->* ((Listof XML-Element-Attribute))
                                   ((Option Symbol) (Listof Symbol))
                                   (Values (U XML-Attr AltReturnType) (Listof XML-Element-Attribute)))
         (lambda [attrs [elem #false] [omits null]]
           (let extract ([_attrs : (Listof XML-Element-Attribute) attrs]
                         [_srtta : (Listof XML-Element-Attribute) null]
                         [collected? : Any #false]
                         [field : (Option XML-Element-Attribute) #false] ...)
             (cond [(pair? _attrs)
                    (let*-values ([(self rest) (values (car _attrs) (cdr _attrs))])
                      (case (car self)
                        [(field) (with-a-field-replaced (extract rest _srtta #true #:fields (field ...)) #:for field #:set self)] ...
                        [else (extract rest (cons self _srtta) collected? field ...)]))]
                   [(or collected?)
                    (values (xml-attr ((inst xml-attribute->datum/safe FieldType) 'xml-attr 'field field xml-attr-value->datum report-unknown elem omits defval ...) ...)
                            _srtta)]
                   [else (values (and (pair? mandatory-fields) (raise-xml-missing-attribute-error elem 'xml-attr mandatory-fields) #false) attrs)])))))]
    [(_ extract-attr : XML-Attr #:vector #:with report-unknown report-range-exn
        (xml-attr [field FieldType xml-attr-value->datum defval ...] ...)
        mandatory-fields AltReturnType)
     (with-syntax* ([(total field-idx ...) (make-identifier-indices #'(field ...))]
                    [([XML-Type false] ...) (make-list (syntax-e #'total) (list #'XML-Element-Attribute #'#false))])
       (syntax/loc stx
         (define extract-attr : (->* ((Listof XML-Element-Attribute))
                                     ((Option Symbol) (Listof Symbol))
                                     (Values (U XML-Attr AltReturnType) (Listof XML-Element-Attribute)))
           (lambda [attrs [elem #false] [omits null]]
             (let ([avec : (Vector (Option XML-Type) ...) (vector false ...)])
               (let extract ([_attrs : (Listof XML-Element-Attribute) attrs]
                             [_srtta : (Listof XML-Element-Attribute) null]
                             [collected? : Any #false])
                 (cond [(pair? _attrs)
                        (let*-values ([(self rest) (values (car _attrs) (cdr _attrs))])
                          (case (car self)
                            [(field) (vector-set! avec field-idx self) (extract rest _srtta #true)] ...
                            [else (extract rest (cons self _srtta) collected?)]))]
                       [(or collected?)
                        (values (xml-attr ((inst xml-attribute->datum/safe FieldType)
                                           'xml-attr 'field
                                           (vector-ref avec field-idx) xml-attr-value->datum
                                           report-unknown elem omits defval ...) ...)
                                _srtta)]
                       [else (values (and (pair? mandatory-fields) (raise-xml-missing-attribute-error elem 'xml-attr mandatory-fields) #false) attrs)])))))))]
    [(_ extract-attr : XML-Attr #:hash #:with report-unknown report-range-exn
        (xml-attr [field FieldType xml-attr-value->datum defval ...] ...)
        mandatory-fields AltReturnType)
     (syntax/loc stx
       (define extract-attr : (->* ((Listof XML-Element-Attribute))
                                   ((Option Symbol) (Listof Symbol))
                                   (Values (U XML-Attr AltReturnType) (Listof XML-Element-Attribute)))
         (lambda [attrs [elem #false] [omits null]]
           (let-values ([(adict _srtta) (xml-attributes-extract attrs '(field ...) report-unknown elem omits)])
             (cond [(hash-empty? adict) (values (and (pair? mandatory-fields) (raise-xml-missing-attribute-error elem 'xml-attr mandatory-fields)) attrs)]
                   [else (values (xml-attr ((inst xml-attribute->datum/safe FieldType)
                                            'xml-attr 'field
                                            (hash-ref adict 'field λfalse) xml-attr-value->datum
                                            report-unknown elem omits defval ...) ...)
                                 _srtta)])))))]))

(define-syntax (define-xml-attribute stx)
  (syntax-parse stx #:literals [:]
    [(_ attr : Attr #:-> super #:with extract-attr attr->xexpr rest ...)
     (syntax/loc stx
       (define-xml-attribute/extract attr : Attr #:-> super
         #:with define-xml-attribute-extract extract-attr attr->xexpr rest ...))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml-attributes-extract
  : (All (T) (case-> [(Listof XML-Element-Attribute) (U Symbol (-> Symbol Boolean)) -> (Values (Option XML-Element-Attribute-Value) (Listof XML-Element-Attribute))]
                     [(Listof XML-Element-Attribute) (-> Symbol Boolean) (-> XML-Element-Attribute T) -> (Values (Listof T) (Listof XML-Element-Attribute))]
                     [(Listof XML-Element-Attribute) (Listof Symbol) (-> (Option Symbol) (Listof XML-Element-Attribute) Void) (Option Symbol) (Listof Symbol)
                                                     -> (Values (Immutable-HashTable Symbol XML-Element-Attribute) (Listof XML-Element-Attribute))]))
  (case-lambda
    [(attrs name)
     (let extract ([_attrs : (Listof XML-Element-Attribute) attrs]
                   [_srtta : (Listof XML-Element-Attribute) null])
       (cond [(null? _attrs) (values #false _srtta)]
             [else (let-values ([(self rest) (values (car _attrs) (cdr _attrs))])
                     (cond [(eq? (car self) name) (values (cdr self) (append rest _srtta))]
                           [else (extract rest (cons self _srtta))]))]))]
    [(attrs name? attr->datum)
     (let extract ([_attrs : (Listof XML-Element-Attribute) attrs]
                   [_srtta : (Listof XML-Element-Attribute) null]
                   [attrs : (Listof T) null])
       (cond [(null? _attrs) (values attrs _srtta)]
             [else (let-values ([(self rest) (values (car _attrs) (cdr _attrs))])
                     (cond [(name? (car self)) (extract rest _srtta (cons (attr->datum self) attrs))]
                           [else (extract rest (cons self _srtta) attrs)]))]))]
    [(attrs names report-unknown elem omits)
     (let extract ([_attrs : (Listof XML-Element-Attribute) attrs]
                   [_srtta : (Listof XML-Element-Attribute) null]
                   [adict : (Immutable-HashTable Symbol XML-Element-Attribute) (hasheq)])
       (cond [(null? _attrs) (values adict _srtta)]
             [else (let*-values ([(self rest) (values (car _attrs) (cdr _attrs))]
                                 [(attr) (car self)])
                     (cond [(not (memq attr names)) (extract rest (cons self _srtta) adict)]
                           [(not (memq attr omits)) (extract rest _srtta (hash-set adict attr self))]
                           [else (report-unknown elem (list self)) (extract rest _srtta adict)]))]))]))

(define xml-attributes-extract-xmlns : (-> (Listof XML-Element-Attribute) (Values XML-Namespaces (Listof XML-Element-Attribute)))
  (let ([xml:attr->namespace (λ [[attr : XML-Element-Attribute]] (cons (xml-qname-local-part (car attr)) (xml:attr-value->string (cdr attr))))])
    (lambda [attrs]
      (xml-attributes-extract attrs xml-qname-xmlns? xml:attr->namespace))))

(define xml-attributes-extract-pair : (All (X Y) (-> (Listof XML-Element-Attribute) Symbol Symbol
                                                     (XML-Attribute-Value->Datum (Option X)) (XML-Attribute-Value->Datum (Option Y))
                                                     (Values (Option (Pairof X Y)) (Listof XML-Element-Attribute))))
  (lambda [attrs car-name cdr-name val->car val->cdr]
    (let*-values ([(1st rest) (xml-attributes-extract attrs car-name)]
                  [(2nd rest) (xml-attributes-extract rest cdr-name)])
      (let ([x (and 1st (val->car 1st))]
            [y (and 2nd (val->cdr 2nd))])
        (cond [(and x y) (values (cons x y) rest)]
              [else (values #false attrs)])))))

(define xml-attributes-extract-triplet : (All (R G B) (-> (Listof XML-Element-Attribute) Symbol Symbol Symbol
                                                          (XML-Attribute-Value->Datum (Option R))
                                                          (XML-Attribute-Value->Datum (Option G))
                                                          (XML-Attribute-Value->Datum (Option B))
                                                          (Values (Option (List R G B)) (Listof XML-Element-Attribute))))
  (lambda [attrs 1st-name 2nd-name 3rd-name val->1st val->2nd val->3rd]
    (let*-values ([(1st rest) (xml-attributes-extract attrs 1st-name)]
                  [(2nd rest) (xml-attributes-extract rest 2nd-name)]
                  [(3rd rest) (xml-attributes-extract rest 3rd-name)])
      (let ([r (and 1st (val->1st 1st))]
            [g (and 2nd (val->2nd 2nd))]
            [b (and 3rd (val->3rd 3rd))])
        (cond [(and r g b) (values (list r g b) rest)]
              [else (values #false attrs)])))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type XML-Report-Unknown (-> (Option Symbol) (Listof XML-Element-Attribute) Void))

(define raise-xml-missing-attribute-error : (-> (Option Symbol) Symbol (U Symbol (Pairof Symbol (Listof Symbol))) Nothing)
  (lambda [elem λattr fields]
    (raise-syntax-error (or elem λattr)
                        (cond [(symbol? fields) (format "missing mandatory attribute: ~a" fields)]
                              [(null? (cdr fields)) (format "missing mandatory attribute: ~a" (car fields))]
                              [else (format "missing mandatory attributes: ~a" fields)]))))

(define raise-xml-missing-element-error : (-> (Option Symbol) (U Symbol (Pairof Symbol (Listof Symbol))) Nothing)
  (lambda [elem children]
    (raise-syntax-error elem
                        (cond [(symbol? children) (format "missing mandatory element: ~a" children)]
                              [(null? (cdr children)) (format "missing mandatory element: ~a" (car children))]
                              [else (format "missing mandatory elements: ~a" children)]))))

(define #:forall (a) xml-attribute->datum/safe
  : (case-> [Symbol Symbol (Option XML-Element-Attribute) (XML-Attribute-Value->Datum (Option a)) (Option XML-Report-Unknown) (Option Symbol) (Listof Symbol) -> a]
            [Symbol Symbol (Option XML-Element-Attribute) (XML-Attribute-Value->Datum (Option a)) (Option XML-Report-Unknown) (Option Symbol) (Listof Symbol) a -> a])
  (case-lambda
    [(λattr field attr value->datum report-unknown elem omits)
     (or (cond [(not attr) #false]
               [(and report-unknown (pair? omits) (memq (car attr) omits)) (report-unknown elem (list attr)) #false]
               [else (value->datum (cdr attr))])
         (raise-xml-missing-attribute-error elem λattr field))]
    [(λattr field attr value->datum report-unknown elem omits defval)
     (cond [(not attr) defval]
           [(and report-unknown (pair? omits) (memq (car attr) omits)) (report-unknown elem (list attr)) defval]
           [else (or (value->datum (cdr attr)) defval)])]))

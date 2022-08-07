#lang typed/racket/base

(provide (all-defined-out))

(require digimon/syntax)
(require digimon/digitama/stdio)

(require "digicore.rkt")
(require "document.rkt")
(require "namespace.rkt")

(require "plain/grammar.rkt")
(require "grammar.rkt")

(require (for-syntax syntax/parse))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type XML-Source (U String Symbol (Pairof (U String Symbol) (Pairof Positive-Integer Natural))))

(define-syntax (extract-dom-values stx)
  (syntax-case stx [:]
    [(_ func #:pre-args [pre-argl ...] #:post-args [post-argl ...] #:with attrs omits elem report-unknown #:fields [])
     (syntax/loc stx (func pre-argl ... attrs post-argl ...))]
    [(_ func #:pre-args [pre-argl ...] #:post-args [post-argl ...] #:with attrs omits elem report-unknown #:fields [[field xml-value->datum defval ...] ...])
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
                   (xml-attribute->datum/safe field xml-value->datum (or defval ...) report-unknown omits elem) ... _srtta
                   post-argl ...))))]))

(define-syntax (extract-dom-datum stx)
  (syntax-case stx [:]
    [(_ func : DOM-Elem #:pre-args [pre-argl ...] #:post-args [post-argl ...] #:with attrs #:fields [])
     (syntax/loc stx (values (func pre-argl ... post-argl ...) attrs))]
    [(_ func : DOM-Elem #:pre-args [pre-argl ...] #:post-args [post-argl ...] #:with attrs #:fields [[field xml->datum defval ...] ...])
     (syntax/loc stx
       (let extract : (Values DOM-Elem (Listof XML-Element-Attribute*)) ([_attrs : (Listof XML-Element-Attribute*) attrs]
                                                                         [_srtta : (Listof XML-Element-Attribute*) null]
                                                                         [field : (Option XML-Element-Attribute*) #false] ...)
         (if (pair? _attrs)
             (let*-values ([(self tail) (values (car _attrs) (cdr _attrs))])
               (case (xml:name-datum (car self))
                 [(field) (with-a-field-replaced (extract tail _srtta #:fields (field ...)) #:for field #:set self)] ...
                 [else (extract tail (cons self _srtta) field ...)]))
             (values (func pre-argl ...
                           (if field (xml->datum (cdr field)) (or defval ...)) ...
                           post-argl ...)
                     _srtta))))]))

(define-syntax (define-dom-element stx)
  (syntax-parse stx #:literals [:]
    [(_ dom-elem : DOM-Elem
        #:refine-element [refine-element #:report-unknown report-unknown]
        #:head [super ([hfield : HFieldType [hdefval ...] hfield-ref] ...)
                        #:values header-values #:-> DOM-Element]
        #:body [([bfield : BFieldType [bdefval ...]] ...) #:values body-values]
        (~optional (~seq #:attribute-categories [[attrib : Attrib extract-attrib] ...])
                   #:defaults ([(attrib 1) null] [(Attrib 1) null] [(extract-attrib 1) null]))
        (~optional (~seq #:omit-header-fields [excfield-name ...])
                   #:defaults ([(excfield-name 1) null]))
        ([field : FieldType #:-> xml->datum defval ...] ...)
        (~optional (~seq #:extra ([extfield : ExtFieldType extdefval ...] ...))
                   #:defaults ([(extfield 1) null] [(ExtFieldType 1) null] [(extdefval 2) null]))
        options ...)
     (with-syntax* ([make-dom (make-identifier #'dom-elem "make-~a")]
                    [remake-dom (make-identifier #'dom-elem "remake-~a")]
                    [([(incfield IncFieldType [incdefval ...]) ...] [(excfield ExcFieldType [excdefval ...]) ...])
                     (let ([omitted-fields (syntax->datum #'(excfield-name ...))]
                           [<supfield>s (syntax->list #'([hfield HFieldType [hdefval ...]] ...))])
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
                    [(extfield-ref ...) (make-identifiers #'dom-elem #'(extfield ...))]
                    [([kw-hdrargs ...] [kw-hdrreargs ...]) (make-keyword-arguments #'(incfield ...) #'(IncFieldType ...) #'([incdefval ...] ...))]
                    [([kw-attargs ...] [kw-attreargs ...]) (make-keyword-optional-arguments #'(attrib ...) #'(Attrib ...))]
                    [([kw-slfargs ...] [kw-slfreargs ...]) (make-keyword-arguments #'(field ...) #'(FieldType ...) #'([defval ...] ...))]
                    [([kw-bdyargs ...] [kw-bdyreargs ...]) (make-keyword-arguments #'(bfield ...) #'(BFieldType ...) #'([bdefval ...] ...))]
                    [([kw-extargs ...] [kw-extreargs ...]) (make-keyword-arguments #'(extfield ...) #'(ExtFieldType ...) #'([extdefval ...] ...))])
       (syntax/loc stx
         (begin (struct dom-elem super
                  ([attrib : (Option Attrib)] ...
                   [field : FieldType] ...
                   [bfield : BFieldType] ...
                   [extfield : ExtFieldType] ...)
                  #:type-name DOM-Elem
                  #:transparent
                  #:property prop:custom-write
                  (λ [[self : DOM-Elem] [/dev/stdout : Output-Port] [mode : (U Zero One Boolean)]]
                    (xml-element-write 'dom-elem /dev/stdout mode
                                       (list 'hfield ... 'attrib ... 'field ...)
                                       (list (hfield-ref self) ... (attfield-ref self) ... (selfield-ref self) ...)
                                       (list (bdyfield-ref self) ... (extfield-ref self) ...)))
                  options ...)

                (define (make-dom kw-hdrargs ... kw-attargs ... kw-slfargs ... kw-bdyargs ... kw-extargs ...) : DOM-Elem
                  (let ([excfield : ExcFieldType excdefval ...] ...)
                    (dom-elem hfield ... attrib ... field ... bfield ... extfield ...)))

                (define (remake-dom [src : DOM-Elem] kw-hdrreargs ... kw-attreargs ... kw-slfreargs ... kw-bdyreargs ... kw-extreargs ...) : DOM-Elem
                  (let ([excfield : Void (void)] ...)
                    (dom-elem (if (void? hfield) (hfield-ref src) hfield) ...
                              (if (void? attrib) (attfield-ref src) attrib) ...
                              (if (void? field) (selfield-ref src) field) ...
                              (if (void? bfield) (bdyfield-ref src) bfield) ...
                              (if (void? extfield) (extfield-ref src) extfield) ...)))

                (define (refine-element [xml.dom : XML-Element*] kw-extargs ...) : DOM-Elem
                  (let*-values ([(hfield ... rest) (header-values (cadr xml.dom) '(excfield ...) (car xml.dom))]
                                [(bfield ...) (body-values (caddr xml.dom) (car xml.dom))] #| TODO: deal with invalid children |#
                                [(attrib rest) (extract-attrib rest) #| only subfields could be omitted |#] ...)
                    (define-values (self unknowns)
                      (extract-dom-datum dom-elem : DOM-Elem
                                         #:pre-args [hfield ... attrib ...]
                                         #:post-args [bfield ... extfield ...]
                                         #:with rest #:fields [[field xml->datum defval ...] ...]))
                    (when (pair? unknowns) (report-unknown (car xml.dom) unknowns))
                    self)))))]))

(define-syntax (define-xml-subdom stx)
  (syntax-parse stx #:literals [:]
    [(_ subdom : SubDOM
        #:subdom-refine [refine #:report-unknown report-unknown]
        #:head [super ([hfield : HFieldType [hdefval ...] hfield-ref] ...) #:values header-values #:-> DOM-Element]
        #:body [([bfield : BFieldType [bdefval ...]] ...) #:values extract-body]
        (~optional (~seq #:attribute-categories [[attrib : Attrib extract-attrib] ...])
                   #:defaults ([(attrib 1) null] [(Attrib 1) null] [(extract-attrib 1) null]))
        ([mfield : MFieldType #:-> xml->datum mdefval ...] ...)
        (~optional (~seq #:subdom [(deftree subsubdom : SubsubDOM subsubrest ...) ...])
                   #:defaults ([(deftree 1) null] [(subsubdom 1) null] [(SubsubDOM 1) null] [(subsubrest 2) null]))
        (defdom [dom-elem dom-tagname] : DOM-Elem dom-elem-rest ...) ...)
     (with-syntax* ([subdom-refine (make-identifier #'subdom "~a-refine")]
                    [(afield-ref ...) (make-identifiers #'subdom #'(attrib ...))]
                    [(mfield-ref ...) (make-identifiers #'subdom #'(mfield ...))]
                    [(subdom-refine ...) (map-identifiers #'(subsubdom ...) "~a-refine")]
                    [(refine-elem ...) (map-identifiers #'(dom-elem ...) "refine-~a")])
       (syntax/loc stx
         (begin (struct subdom super ([attrib : (Option Attrib)] ... [mfield : MFieldType] ...) #:type-name SubDOM #:transparent)

                (define refine : (->* (XML-Element*) ((Listof Symbol)) (Option SubDOM))
                  (lambda [e [valid-tagnames null]]
                    (or (subdom-refine e valid-tagnames) ...
                        (let-values ([(ns e-tagname) (xml-qname-split (xml:name-datum (car e)))])
                          (case e-tagname
                            [(dom-tagname) (and (or (null? valid-tagnames) (memq e-tagname valid-tagnames)) (refine-elem e))] ...
                            [else #false])))))

                (define (extract-header [attrs : (Listof XML-Element-Attribute*)] [omits : (Listof Symbol) null] [elem : (Option XML:Name) #false])
                  : (Values HFieldType ... (Option Attrib) ... MFieldType ... (Listof XML-Element-Attribute*))
                  (let*-values ([(hfield ... tail) (header-values attrs omits elem)]
                                [(attrib tail) (extract-attrib tail omits elem)] ...)
                    (extract-dom-values values #:pre-args [hfield ... attrib ...] #:post-args [] #:with tail omits elem report-unknown
                                        #:fields [[mfield xml->datum mdefval ...] ...])))

                (deftree subsubdom : SubsubDOM
                  #:subdom-refine [subdom-refine #:report-unknown report-unknown]
                  #:head [subdom ([hfield : HFieldType [hdefval ...] hfield-ref] ...
                                  [attrib : (Option Attrib) [#false] afield-ref] ...
                                  [mfield : MFieldType [mdefval ...] mfield-ref] ...)
                                 #:values extract-header #:-> DOM-Element]
                  #:body [([bfield : BFieldType [bdefval ...]] ...) #:values extract-body]
                  subsubrest ...) ...

                (defdom dom-elem : DOM-Elem
                  #:refine-element [refine-elem #:report-unknown report-unknown]
                  #:head [subdom ([hfield : HFieldType [hdefval ...] hfield-ref] ...
                                  [attrib : (Option Attrib) [#false] afield-ref] ...
                                  [mfield : MFieldType [mdefval ...] mfield-ref] ...)
                                 #:values extract-header #:-> DOM-Element]
                  #:body [([bfield : BFieldType [bdefval ...]] ...) #:values extract-body]
                  dom-elem-rest ...) ...)))]))

(define-syntax (define-xml-dom stx)
  (syntax-parse stx #:literals [:]
    [(_ dom-element : DOM-Element
        #:dom-refine [header-extract refine report-unknown body-extract]
        ([hfield : HFieldType hdefval ...] ...) ([bfield : BFieldType bdefval ...] ...)
        #:unknown [dom-unknown : DOM-Unknown #:refine refine-unknown]
        (~optional (~seq #:subdom [(deftree subdom : SubDOM subrest ...) ...])
                   #:defaults ([(deftree 1) null] [(subdom 1) null] [(SubDOM 1) null] [(subrest 2) null]))
        (defelem [dom-elem dom-tagname] : DOM-Elem dom-rest-definition ...) ...)
     (with-syntax* ([(hfield-ref ...) (make-identifiers #'dom-element #'(hfield ...))]
                    [(subdom-refine ...) (map-identifiers #'(subdom ...) "~a-refine")]
                    [(refine-elem ...) (map-identifiers #'(dom-elem ...) "refine-~a")])
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
                        (let-values ([(ns e-tagname) (xml-qname-split (xml:name-datum (car e)))])
                          (case e-tagname
                            [(dom-tagname) (and (or (null? valid-tagnames) (memq e-tagname valid-tagnames)) (refine-elem e))] ...
                            [else #false]))
                        (and (report-unknown e)
                             (refine-unknown e)))))

                (define refine-unknown : (-> XML-Element* DOM-Unknown)
                  (lambda [e]
                    (let-values ([(?src ?id ?core other-attrs) (extract-header (cadr e) null (car e))])
                      (dom-unknown ?src ?id ?core
                                   (xml:name-datum (car e))
                                   (map xml-attribute*->datum other-attrs)
                                   (map xml-mixed-content*->datum (caddr e))))))

                (define extract-header
                  : (->* ((Listof XML-Element-Attribute*) (Listof Symbol)) ((Option XML:Name))
                         (Values HFieldType ... (Listof XML-Element-Attribute*)))
                  (lambda [attrs [omits null] [elem #false]]
                    (let-values ([(?src) (and elem (xml-token->source elem))]
                                 [(?id ?core other-attrs) (header-extract attrs)])
                      (values ?src ?id ?core other-attrs))))

                (deftree subdom : SubDOM
                  #:subdom-refine [subdom-refine #:report-unknown report-unknown]
                  #:head [dom-element ([hfield : HFieldType [hdefval ...] hfield-ref] ...)
                                      #:values extract-header #:-> DOM-Element]
                  #:body [([bfield : BFieldType [bdefval ...]] ...) #:values body-extract]
                  subrest ...) ...
                
                (defelem dom-elem : DOM-Elem
                  #:refine-element [refine-elem #:report-unknown report-unknown]
                  #:head [dom-element ([hfield : HFieldType [hdefval ...] hfield-ref] ...)
                                      #:values extract-header #:-> DOM-Element]
                  #:body [([bfield : BFieldType [bdefval ...]] ...) #:values body-extract]
                  dom-rest-definition ...) ...)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax (define-xml-attribute-extract stx)
  (syntax-parse stx #:literals [:]
    [(_ extract-attr : SVG-Attr #:inline #:with report-unknown (svg-attr [field : [XML-Type field-idx false] xml-attribute-value->datum defval ...] ...))
     (syntax/loc stx
       (define extract-attr : (->* ((Listof XML-Element-Attribute*)) ((Listof Symbol) (Option XML:Name)) (Values (Option SVG-Attr) (Listof XML-Element-Attribute*)))
         (lambda [attrs [omits null] [elem #false]]
           (let extract ([_attrs : (Listof XML-Element-Attribute*) attrs]
                         [_srtta : (Listof XML-Element-Attribute*) null]
                         [field : (Option XML-Type) #false] ...)
             (cond [(pair? _attrs)
                    (let*-values ([(self rest) (values (car _attrs) (cdr _attrs))])
                      (case (xml:name-datum (car self))
                        [(field) (with-a-field-replaced (extract rest _srtta #:fields (field ...))
                                   #:for field #:set (xml-attribute->datum/safe self xml-attribute-value->datum #false report-unknown omits elem))] ...
                        [else (extract rest (cons self _srtta) field ...)]))]
                   [(or field ...) (values (svg-attr (or field defval ...) ...) _srtta)]
                   [else (values #false attrs)])))))]
    [(_ extract-attr : SVG-Attr #:vector #:with report-unknown (svg-attr [field : [XML-Type field-idx false] xml-attribute-value->datum defval ...] ...))
     (syntax/loc stx
       (define extract-attr : (->* ((Listof XML-Element-Attribute*)) ((Listof Symbol) (Option XML:Name)) (Values (Option SVG-Attr) (Listof XML-Element-Attribute*)))
         (lambda [attrs [omits null] [elem #false]]
           (let ([avec : (Vector (Option XML-Type) ...) (vector false ...)])
             (let extract ([_attrs : (Listof XML-Element-Attribute*) attrs]
                           [_srtta : (Listof XML-Element-Attribute*) null]
                           [collected? : Any #false])
               (cond [(pair? _attrs)
                      (let*-values ([(self rest) (values (car _attrs) (cdr _attrs))])
                        (case (xml:name-datum (car self))
                          [(field) (vector-set! avec field-idx (xml-attribute->datum/safe self xml-attribute-value->datum #false report-unknown omits elem))
                                   (extract rest _srtta (or collected? (vector-ref avec field-idx)))] ...
                          [else (extract rest (cons self _srtta) collected?)]))]
                     [(not collected?) (values #false attrs)]
                     [else (values (svg-attr (or (vector-ref avec field-idx) defval ...) ...) _srtta)]))))))]
    [(_ extract-attr : SVG-Attr #:hash #:with report-unknown (svg-attr [field : [XML-Type field-idx false] xml-attribute-value->datum defval ...] ...))
     (syntax/loc stx
       (define extract-attr : (->* ((Listof XML-Element-Attribute*)) ((Listof Symbol) (Option XML:Name))
                                   (Values (Option SVG-Attr) (Listof XML-Element-Attribute*)))
         (lambda [attrs [omits null] [elem #false]]
           (let-values ([(adict _srtta) (xml-attributes*-extract attrs '(field ...) report-unknown omits elem)])
             (cond [(hash-empty? adict) (values #false attrs)]
                   [else (let ([->f (λ [] #false)])
                           (values (svg-attr (xml-attribute->datum/safe (hash-ref adict 'field ->f) xml-attribute-value->datum (or defval ...) report-unknown)
                                             ...)
                                   _srtta))])))))]))

(define-syntax (define-xml-attribute stx)
  (syntax-parse stx #:literals [:]
    [(_ attr : Attr #:-> super #:with extract-attr
        ([field : FieldType #:-> attribute->datum defval ...] ...)
        #:report-unknown report-unknown options ...)
     (with-syntax* ([make-attr (format-id #'attr "make-~a" (syntax-e #'attr))]
                    [remake-attr (format-id #'attr "remake-~a" (syntax-e #'attr))]
                    [cascade-attr (format-id #'attr "cascade-~a" (syntax-e #'attr))]
                    [(field-total field-idx ...) (make-identifier-indices #'(field ...))]
                    [(field-ref ...) (make-identifiers #'attr #'(field ...))]
                    [([kw-args ...] [kw-reargs ...]) (make-keyword-arguments #'(field ...) #'(FieldType ...) #'([defval ...] ...))]
                    [switch (let ([count (syntax-e #'field-total)])
                              ;;; TODO: find a better strategy
                              ; #:inline is definitely bad for large structs, whereas
                              ; #:vector and #:hash are almost identical for large structs. 
                              (cond [(<= count 10) #'#:inline]
                                    [(<= count 20) #'#:vector]
                                    [else          #'#:hash]))])
       (syntax/loc stx
         (begin (struct attr super ([field : FieldType] ...)
                  #:type-name Attr #:transparent
                  #:property prop:custom-write
                  (λ [[self : Attr] [/dev/stdout : Output-Port] [mode : (U Zero One Boolean)]]
                    (xml-attributes-write 'attr /dev/stdout mode (list 'field ...) (list (field-ref self) ...)))
                  options ...)

                (define (make-attr kw-args ...) : Attr
                  (attr field ...))

                (define (remake-attr [src : Attr] kw-reargs ...) : Attr
                  (attr (if (void? field) (field-ref src) field) ...))

                (define-xml-attribute-extract extract-attr : Attr switch #:with report-unknown
                  (attr [field : [FieldType field-idx #false] attribute->datum defval ...] ...))

                (define (cascade-attr [parent : Attr] [child : Attr]) : Attr
                  (attr (or (field-ref child) (field-ref parent)) ... )))))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml-attributes*-extract
  : (case-> [(Listof XML-Element-Attribute*) Symbol -> (Values (Option XML-Element-Attribute-Value*) (Listof XML-Element-Attribute*))]
            [(Listof XML-Element-Attribute*) (Listof Symbol) (-> (Option XML:Name) (Listof XML-Element-Attribute*) Void) (Listof Symbol) (Option XML:Name)
                                             -> (Values (Immutable-HashTable Symbol XML-Element-Attribute*) (Listof XML-Element-Attribute*))])
  (case-lambda
    [(attrs name)
     (let extract ([_attrs : (Listof XML-Element-Attribute*) attrs]
                   [_srtta : (Listof XML-Element-Attribute*) null])
       (cond [(null? _attrs) (values #false _srtta)]
             [else (let-values ([(self rest) (values (car _attrs) (cdr _attrs))])
                     (cond [(xml:name=:=? (car self) name) (values (cdr self) (append rest _srtta))]
                           [else (extract rest (cons self _srtta))]))]))]
    [(attrs names report-unknown omits elem)
     (let extract ([_attrs : (Listof XML-Element-Attribute*) attrs]
                   [_srtta : (Listof XML-Element-Attribute*) null]
                   [adict : (Immutable-HashTable Symbol XML-Element-Attribute*) (hasheq)])
       (cond [(null? _attrs) (values adict _srtta)]
             [else (let*-values ([(self rest) (values (car _attrs) (cdr _attrs))]
                                 [(attr) (xml:name-datum (car self))])
                     (cond [(not (memq attr names)) (extract rest (cons self _srtta) adict)]
                           [(not (memq attr omits)) (extract rest _srtta (hash-set adict attr self))]
                           [else (report-unknown elem (list self)) (extract rest _srtta adict)]))]))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml-token->source : (-> XML-Token (Option XML-Source))
  (lambda [t]
    (cons (syn-token-source t)
          (cons (syn-token-line t)
                (syn-token-column t)))))

(define xml-attribute->datum/safe : (All (a b) (->* ((Option XML-Element-Attribute*)
                                                     (-> XML-Element-Attribute-Value* a) b
                                                     (-> (Option XML:Name) (Listof XML-Element-Attribute*) Void))
                                                    ((Listof Symbol) (Option XML:Name))
                                                    (U a b)))
  (lambda [attr value->datum defvalue report-unknown [omits null] [elem #false]]
    (cond [(not attr) defvalue]
          [(null? omits) (value->datum (cdr attr))]
          [(xml:name=<-? (car attr) omits) (report-unknown elem (list attr)) defvalue]
          [else (value->datum (cdr attr))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml-element-write : (-> Symbol Output-Port (U Zero One Boolean) (Pairof Symbol (Listof Symbol)) (Pairof Any (Listof Any)) (Listof Any) Void)
  (lambda [id /dev/stdout mode fields all-data body-data]
    (define write-datum : (-> Any Output-Port Void) (stdio-select-writer mode))
    (define-values (line column pos) (port-next-location /dev/stdout))
    (define indent : String (list->string (cons #\newline (if (not column) null (make-list (+ column 2) #\space)))))
    (define-values (srtta atad)
      (for/fold ([attrs : (Listof Any) null]
                 [data : (Listof (Pairof Symbol Any)) null])
                ([datum (in-list (cdr all-data))]
                 [fname (in-list (cdr fields))]
                 #:unless (or (not datum) (null? datum)))
        (cond [(struct? datum) (values (cons datum attrs) data)]
              [else (values attrs (cons (cons fname datum) data))])))
      
    (display #\( /dev/stdout)
    (display id /dev/stdout)
    (display #\space /dev/stdout)
    (write-datum (car all-data) /dev/stdout)

    (when (pair? srtta)
      (for ([attr (in-list (reverse srtta))])
        (display indent /dev/stdout)
        (write-datum attr /dev/stdout)))

    (display indent /dev/stdout)
    (when (pair? atad)
      (write-datum (reverse atad) /dev/stdout)
      (display indent /dev/stdout))
    
    (write-datum body-data /dev/stdout)
    (write-char #\) /dev/stdout)
    (write-string indent /dev/stdout 0 (or column 1))
    (flush-output /dev/stdout)))

(define xml-attributes-write : (-> Symbol Output-Port (U Zero One Boolean) (Listof Symbol) (Listof Any) Void)
  (lambda [id /dev/stdout mode fields all-data]
    (define write-datum : (-> Any Output-Port Void) (stdio-select-writer mode))
    (define data : (Listof (Pairof Symbol Any))
      (for/list ([datum (in-list all-data)]
                 [fname (in-list fields)]
                 #:unless (or (not datum) (null? datum)))
        (cons fname datum)))
    
    (display (if (eq? mode 0) "(" "#<") /dev/stdout)
    (display id /dev/stdout)
    (when (pair? data)
      (display #\space /dev/stdout)
      (write-datum data /dev/stdout))
    (write-char (if (eq? mode 0) #\) #\>) /dev/stdout)
    (flush-output /dev/stdout)))

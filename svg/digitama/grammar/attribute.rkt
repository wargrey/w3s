#lang typed/racket/base

(provide (all-defined-out))
(provide (for-syntax (all-defined-out)))

(require digimon/syntax)

(require sgml/digitama/digicore)
(require sgml/digitama/document)
(require sgml/digitama/datatype)

(require sgml/digitama/plain/grammar)
(require sgml/digitama/grammar)

(require (for-syntax syntax/parse))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-for-syntax (racket->svg:attr-names <a>)
  (define attrib (symbol->immutable-string (syntax-e <a>)))
  (list (format-id <a> "svg:attr:~a" attrib)
        (format-id <a> "SVG:Attr:~a" (string-titlecase attrib))
        (format-id <a> "extract-svg:attr:~a" attrib)))

(define-syntax (define-svg-attribute-extract stx)
  (syntax-parse stx #:literals [:]
    [(_ extract-attr : SVG-Attr #:inline (svg-attr [field : [XML-Type field-idx false] xml-attribute-value->datum defval ...] ...))
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
                                   #:for field #:set (svg-attribute->datum/safe self xml-attribute-value->datum #false omits elem))] ...
                        [else (extract rest (cons self _srtta) field ...)]))]
                   [(or field ...) (values (svg-attr (or field defval ...) ...) _srtta)]
                   [else (values #false attrs)])))))]
    [(_ extract-attr : SVG-Attr #:vector (svg-attr [field : [XML-Type field-idx false] xml-attribute-value->datum defval ...] ...))
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
                          [(field) (vector-set! avec field-idx (svg-attribute->datum/safe self xml-attribute-value->datum #false omits elem))
                                   (extract rest _srtta (or collected? (vector-ref avec field-idx)))] ...
                          [else (extract rest (cons self _srtta) collected?)]))]
                     [(not collected?) (values #false attrs)]
                     [else (values (svg-attr (or (vector-ref avec field-idx) defval ...) ...) _srtta)]))))))]
    [(_ extract-attr : SVG-Attr #:hash (svg-attr [field : [XML-Type field-idx false] xml-attribute-value->datum defval ...] ...))
     (syntax/loc stx
       (define extract-attr : (->* ((Listof XML-Element-Attribute*)) ((Listof Symbol) (Option XML:Name))
                                   (Values (Option SVG-Attr) (Listof XML-Element-Attribute*)))
         (lambda [attrs [omits null] [elem #false]]
           (let-values ([(adict _srtta) (svg-attributes*-extract attrs '(field ...) omits elem)])
             (cond [(hash-empty? adict) (values #false attrs)]
                   [else (let ([->f (λ [] #false)])
                           (values (svg-attr (svg-attribute->datum/safe (hash-ref adict 'field ->f) xml-attribute-value->datum (or defval ...))
                                             ...)
                                   _srtta))])))))]))

(define-syntax (define-svg-attribute stx)
  (syntax-parse stx #:literals [:]
    [(_ attr-name ([field : FieldType #:=> xml-attribute-value->datum defval ...] ...) options ...)
     (with-syntax* ([(svg:attr SVG:Attr extract-svg:attr) (racket->svg:attr-names #'attr-name)]
                    [make-svg:attr (format-id #'svg:attr "make-~a" (syntax-e #'svg:attr))]
                    [remake-svg:attr (format-id #'svg:attr "remake-~a" (syntax-e #'svg:attr))]
                    [cascade-svg:attr (format-id #'svg:attr "cascade-~a" (syntax-e #'svg:attr))]
                    [(field-total field-idx ...) (make-identifier-indices #'(field ...))]
                    [(field-ref ...) (make-identifiers #'svg:attr #'(field ...))]
                    [([kw-args ...] [kw-reargs ...]) (make-keyword-arguments #'(field ...) #'(FieldType ...) #'([defval ...] ...))]
                    [switch (let ([count (syntax-e #'field-total)])
                              ;;; TODO: find a better strategy
                              ; #:inline is definitely bad for large structs, whereas
                              ; #:vector and #:hash are almost identical for large structs. 
                              (cond ;[(<= count 10) #'#:inline]
                                    [(<= count 20) #'#:vector]
                                    [else          #'#:hash]))])
       (syntax/loc stx
         (begin (struct svg:attr svg-attribute ([field : FieldType] ...) #:type-name SVG:Attr #:transparent options ...)

                (define (make-svg:attr kw-args ...) : SVG:Attr
                  (svg:attr field ...))

                (define (remake-svg:attr [src : SVG:Attr] kw-reargs ...) : SVG:Attr
                  (svg:attr (if (void? field) (field-ref src) field) ...))

                (define-svg-attribute-extract extract-svg:attr : SVG:Attr switch
                  (svg:attr [field : [FieldType field-idx #false] xml-attribute-value->datum defval ...] ...))

                (define (cascade-svg:attr [parent : SVG:Attr] [child : SVG:Attr]) : SVG:Attr
                  (svg:attr (or (field-ref child) (field-ref parent)) ... )))))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct svg-attribute () #:type-name SVG-Attribute #:transparent)

(define-svg-attribute core
  ([xml:base : (Option String)  #:=> xml-attribute-value->string #false]
   [xml:lang : (Option String)  #:=> xml-attribute-value->string #false]
   [xml:space : (Option Symbol) #:=> xml-attribute-value->symbol #false]))

(define-svg-attribute condition
  ([requiredFeatures : (Option (Listof Symbol)) #:=> xml-attribute-value->symbol-list #false]
   [requiredExtensions : (Option String)        #:=> xml-attribute-value->string #false]
   [systemLanguage : (Option String)            #:=> xml-attribute-value->string #false]))

(define-svg-attribute external
  ([externalResourcesRequired : (Option String) #:=> xml-attribute-value->string #false]))

(define-svg-attribute xlink
  ([xlink:actuate : (Option String) #:=> xml-attribute-value->string #false]
   [xlink:arcrole : (Option String) #:=> xml-attribute-value->string #false]
   [xlink:href : (Option String) #:=> xml-attribute-value->string #false]
   [xlink:role : (Option String) #:=> xml-attribute-value->string #false]
   [xlink:show : (Option String) #:=> xml-attribute-value->string #false]
   [xlink:title : (Option String) #:=> xml-attribute-value->string #false]
   [xlink:type : (Option String) #:=> xml-attribute-value->string #false]))

(define-svg-attribute filter-primitive
  ([x : (Option String) #:=> xml-attribute-value->string #false]
   [y : (Option String) #:=> xml-attribute-value->string #false]
   [width : (Option String) #:=> xml-attribute-value->string #false]
   [height : (Option String) #:=> xml-attribute-value->string #false]
   [result : (Option String) #:=> xml-attribute-value->string #false]))

(define-svg-attribute filter-primitive+in
  ([x : (Option String) #:=> xml-attribute-value->string #false]
   [y : (Option String) #:=> xml-attribute-value->string #false]
   [width : (Option String) #:=> xml-attribute-value->string #false]
   [height : (Option String) #:=> xml-attribute-value->string #false]
   [result : (Option String) #:=> xml-attribute-value->string #false]
   [in : (Option String) #:=> xml-attribute-value->string #false]))

(define-svg-attribute style
  ([class : (Listof Symbol)  #:=> xml-attribute-value->symbol-list null]
   [style : (Option String)  #:=> xml-attribute-value->string #false]))

(define-svg-attribute presentation
  ([alignment-baseline : (Option String) #:=> xml-attribute-value->string #false]
    [baseline-shift : (Option String) #:=> xml-attribute-value->string #false]
    [clip : (Option String) #:=> xml-attribute-value->string #false]
    [clip-path : (Option String) #:=> xml-attribute-value->string #false]
    [clip-rule : (Option String) #:=> xml-attribute-value->string #false]
    [color : (Option String) #:=> xml-attribute-value->string #false]
    [color-interpolation : (Option String) #:=> xml-attribute-value->string #false]
    [color-interpolation-filters : (Option String) #:=> xml-attribute-value->string #false]
    [color-profile : (Option String) #:=> xml-attribute-value->string #false]
    [color-rendering : (Option String) #:=> xml-attribute-value->string #false]
    [cursor : (Option String) #:=> xml-attribute-value->string #false]
    [direction : (Option String) #:=> xml-attribute-value->string #false]
    [display : (Option String) #:=> xml-attribute-value->string #false]
    [dominant-baseline : (Option String) #:=> xml-attribute-value->string #false]
    [enable-background : (Option String) #:=> xml-attribute-value->string #false]
    [fill : (Option String) #:=> xml-attribute-value->string #false]
    [fill-opacity : (Option String) #:=> xml-attribute-value->string #false]
    [fill-rule : (Option String) #:=> xml-attribute-value->string #false]
    [filter : (Option String) #:=> xml-attribute-value->string #false]
    [flood-color : (Option String) #:=> xml-attribute-value->string #false]
    [flood-opacity : (Option String) #:=> xml-attribute-value->string #false]
    [font-family : (Option String) #:=> xml-attribute-value->string #false]
    [font-size : (Option String) #:=> xml-attribute-value->string #false]
    [font-size-adjust : (Option String) #:=> xml-attribute-value->string #false]
    [font-stretch : (Option String) #:=> xml-attribute-value->string #false]
    [font-style : (Option String) #:=> xml-attribute-value->string #false]
    [font-variant : (Option String) #:=> xml-attribute-value->string #false]
    [font-weight : (Option String) #:=> xml-attribute-value->string #false]
    [glyph-orientation-horizontal : (Option String) #:=> xml-attribute-value->string #false]
    [glyph-orientation-vertical : (Option String) #:=> xml-attribute-value->string #false]
    [image-rendering : (Option String) #:=> xml-attribute-value->string #false]
    [kerning : (Option String) #:=> xml-attribute-value->string #false]
    [letter-spacing : (Option String) #:=> xml-attribute-value->string #false]
    [lighting-color : (Option String) #:=> xml-attribute-value->string #false]
    [marker-end : (Option String) #:=> xml-attribute-value->string #false]
    [marker-mid : (Option String) #:=> xml-attribute-value->string #false]
    [marker-start : (Option String) #:=> xml-attribute-value->string #false]
    [mask : (Option String) #:=> xml-attribute-value->string #false]
    [opacity : (Option String) #:=> xml-attribute-value->string #false]
    [overflow : (Option String) #:=> xml-attribute-value->string #false]
    [pointer-events : (Option String) #:=> xml-attribute-value->string #false]
    [shape-rendering : (Option String) #:=> xml-attribute-value->string #false]
    [stop-color : (Option String) #:=> xml-attribute-value->string #false]
    [stop-opacity : (Option String) #:=> xml-attribute-value->string #false]
    [stroke : (Option String) #:=> xml-attribute-value->string #false]
    [stroke-dasharray : (Option String) #:=> xml-attribute-value->string #false]
    [stroke-dashoffset : (Option String) #:=> xml-attribute-value->string #false]
    [stroke-linecap : (Option String) #:=> xml-attribute-value->string #false]
    [stroke-linejoin : (Option String) #:=> xml-attribute-value->string #false]
    [stroke-miterlimit : (Option String) #:=> xml-attribute-value->string #false]
    [stroke-opacity : (Option String) #:=> xml-attribute-value->string #false]
    [stroke-width : (Option String) #:=> xml-attribute-value->string #false]
    [text-anchor : (Option String) #:=> xml-attribute-value->string #false]
    [text-decoration : (Option String) #:=> xml-attribute-value->string #false]
    [text-rendering : (Option String) #:=> xml-attribute-value->string #false]
    [unicode-bidi : (Option String) #:=> xml-attribute-value->string #false]
    [visibility : (Option String) #:=> xml-attribute-value->string #false]
    [word-spacing : (Option String) #:=> xml-attribute-value->string #false]
    [writing-mode : (Option String) #:=> xml-attribute-value->string #false]))

(define-svg-attribute animation-target
  ([attributeName : (Option String) #:=> xml-attribute-value->string #false]
   [attributeType : (Option String) #:=> xml-attribute-value->string #false]))

(define-svg-attribute animation-timing
  ([begin : (Option String) #:=> xml-attribute-value->string #false]
   [dur : (Option String) #:=> xml-attribute-value->string #false]
   [end : (Option String) #:=> xml-attribute-value->string #false]
   [fill : (Option String) #:=> xml-attribute-value->string #false]
   [max : (Option String) #:=> xml-attribute-value->string #false]
   [min : (Option String) #:=> xml-attribute-value->string #false]
   [repeatCount : (Option String) #:=> xml-attribute-value->string #false]
   [repeatDur : (Option String) #:=> xml-attribute-value->string #false]
   [restart : (Option String) #:=> xml-attribute-value->string #false]))

(define-svg-attribute animation-value
  ([by : (Option String) #:=> xml-attribute-value->string #false]
   [calcMode : (Option String) #:=> xml-attribute-value->string #false]
   [from : (Option String) #:=> xml-attribute-value->string #false]
   [keySplines : (Option String) #:=> xml-attribute-value->string #false]
   [keyTimes : (Option String) #:=> xml-attribute-value->string #false]
   [to : (Option String) #:=> xml-attribute-value->string #false]
   [values : (Option String) #:=> xml-attribute-value->string #false]))

(define-svg-attribute animation-addition
  ([accumulate : (Option String) #:=> xml-attribute-value->string #false]
   [additive : (Option String) #:=> xml-attribute-value->string #false]))

(define-svg-attribute document-event
  ([onabort : (Option String) #:=> xml-attribute-value->string #false]
   [onerror : (Option String) #:=> xml-attribute-value->string #false]
   [onresize : (Option String) #:=> xml-attribute-value->string #false]
   [onscroll : (Option String) #:=> xml-attribute-value->string #false]
   [onunload : (Option String) #:=> xml-attribute-value->string #false]
   [onzoom : (Option String) #:=> xml-attribute-value->string #false]))

(define-svg-attribute animation-event
  ([onbegin : (Option String)  #:=> xml-attribute-value->string #false]
   [onend : (Option String)    #:=> xml-attribute-value->string #false]
   [onrepeat : (Option String) #:=> xml-attribute-value->string #false]
   [onload : (Option String)   #:=> xml-attribute-value->string #false]))

(define-svg-attribute graphical-event
  ([onfocusin : (Option String)   #:=> xml-attribute-value->string #false]
   [onfocusout : (Option String)  #:=> xml-attribute-value->string #false]
   [onactivate : (Option String)  #:=> xml-attribute-value->string #false]
   [onclick : (Option String)     #:=> xml-attribute-value->string #false]
   [onmousedown : (Option String) #:=> xml-attribute-value->string #false]
   [onmouseup : (Option String)   #:=> xml-attribute-value->string #false]
   [onmouseover : (Option String) #:=> xml-attribute-value->string #false]
   [onmousemove : (Option String) #:=> xml-attribute-value->string #false]
   [onmouseout : (Option String)  #:=> xml-attribute-value->string #false]
   [onload : (Option String)      #:=> xml-attribute-value->string #false]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define svg-attributes*-extract-core : (-> (Listof XML-Element-Attribute*) (Values (Option Symbol) (Option SVG:Attr:Core) (Listof XML-Element-Attribute*)))
  (lambda [attrs]
    (let*-values ([(?id rest) (svg-attributes*-extract attrs 'id)]
                  [(?core rest) (extract-svg:attr:core rest)])
      (values (and ?id (xml-attribute-value->symbol ?id)) ?core rest))))

(define svg-attributes*-extract
  : (case-> [(Listof XML-Element-Attribute*) Symbol -> (Values (Option XML-Element-Attribute-Value*) (Listof XML-Element-Attribute*))]
            [(Listof XML-Element-Attribute*) (Listof Symbol) (Listof Symbol) (Option XML:Name)
                                             -> (Values (Immutable-HashTable Symbol XML-Element-Attribute*) (Listof XML-Element-Attribute*))])
  (case-lambda
    [(attrs name)
     (let extract ([_attrs : (Listof XML-Element-Attribute*) attrs]
                   [_srtta : (Listof XML-Element-Attribute*) null])
       (cond [(null? _attrs) (values #false _srtta)]
             [else (let-values ([(self rest) (values (car _attrs) (cdr _attrs))])
                     (cond [(xml:name=:=? (car self) name) (values (cdr self) (append rest _srtta))]
                           [else (extract rest (cons self _srtta))]))]))]
    [(attrs names omits elem)
     (let extract ([_attrs : (Listof XML-Element-Attribute*) attrs]
                   [_srtta : (Listof XML-Element-Attribute*) null]
                   [adict : (Immutable-HashTable Symbol XML-Element-Attribute*) (hasheq)])
       (cond [(null? _attrs) (values adict _srtta)]
             [else (let*-values ([(self rest) (values (car _attrs) (cdr _attrs))]
                                 [(attr) (xml:name-datum (car self))])
                     (cond [(not (memq attr names)) (extract rest (cons self _srtta) adict)]
                           [(not (memq attr omits)) (extract rest _srtta (hash-set adict attr self))]
                           [else (svg-report-unrecognized-attributes elem (list self)) (extract rest _srtta adict)]))]))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define svg-report-unrecognized-attributes : (-> (Option XML:Name) (Listof XML-Element-Attribute*) Void)
  (lambda [elem srtta]
    (for ([attr (in-list (reverse srtta))])
      (define v (cdr attr))
      (make+exn:svg:unrecognized
       (cond [(list? v) (list* (car attr) v)]
             [else (list (car attr) v)])
       elem))))

(define svg-attribute->datum/safe : (All (a b) (->* ((Option XML-Element-Attribute*) (-> XML-Element-Attribute-Value* a) b)
                                                    ((Listof Symbol) (Option XML:Name))
                                                    (U a b)))
  (lambda [attr value->datum defvalue [omits null] [elem #false]]
    (cond [(not attr) defvalue]
          [(null? omits) (value->datum (cdr attr))]
          [(xml:name=<-? (car attr) omits) (svg-report-unrecognized-attributes elem (list attr)) defvalue]
          [else (value->datum (cdr attr))])))

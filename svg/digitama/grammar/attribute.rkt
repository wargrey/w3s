#lang typed/racket/base

(provide (all-defined-out))
(provide (for-syntax (all-defined-out)))

(require racket/symbol)

(require digimon/syntax)

(require sgml/digitama/dialect)
(require sgml/digitama/digicore)
(require sgml/digitama/datatype)

(require sgml/digitama/plain/grammar)
(require sgml/digitama/grammar)

(require (for-syntax syntax/parse))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-for-syntax (racket->svg:attr-names <a>)
  (define attrib (symbol->immutable-string (syntax-e <a>)))
  (list (format-id <a> "svg:attr:~a" attrib)
        (format-id <a> "SVG:Attr:~a" (string-titlecase attrib))
        (format-id <a> "extract-svg:attr:~a" attrib)
        (format-id <a> "svg:attr:~a->xml-attributes" attrib)))

(define-syntax (define-svg-attribute stx)
  (syntax-parse stx #:literals [:]
    [(_ attr-name field-defs options ...)
     (with-syntax* ([(svg:attr SVG:Attr extract-attr attr->xexpr) (racket->svg:attr-names #'attr-name)])
       (syntax/loc stx
         (define-xml-attribute svg:attr : SVG:Attr #:-> svg-attribute #:with extract-attr attr->xexpr
           field-defs
           #:report-unknown svg-report-unrecognized-attributes
           options ...)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct svg-attribute () #:type-name SVG-Attribute #:transparent)

(define-svg-attribute core
  ([xml:base : (Option String)  #:= #false #:<-> xml-attribute-value->string]
   [xml:lang : (Option String)  #:= #false #:<-> xml-attribute-value->string]
   [xml:space : (Option Symbol) #:= #false #:<-> xml-attribute-value->symbol]))

(define-svg-attribute condition
  ([requiredFeatures : (Option (Listof Symbol)) #:= #false #:<-> xml-attribute-value->symbol-list]
   [requiredExtensions : (Option String)        #:= #false #:<-> xml-attribute-value->string]
   [systemLanguage : (Option String)            #:= #false #:<-> xml-attribute-value->string]))

(define-svg-attribute external
  ([externalResourcesRequired : (Option String) #:= #false #:<-> xml-attribute-value->string]))

(define-svg-attribute xlink
  ([xlink:actuate : (Option String) #:= #false #:<-> xml-attribute-value->string]
   [xlink:arcrole : (Option String) #:= #false #:<-> xml-attribute-value->string]
   [xlink:href : (Option String) #:= #false #:<-> xml-attribute-value->string]
   [xlink:role : (Option String) #:= #false #:<-> xml-attribute-value->string]
   [xlink:show : (Option String) #:= #false #:<-> xml-attribute-value->string]
   [xlink:title : (Option String) #:= #false #:<-> xml-attribute-value->string]
   [xlink:type : (Option String) #:= #false #:<-> xml-attribute-value->string]))

(define-svg-attribute filter-primitive
  ([x : (Option String) #:= #false #:<-> xml-attribute-value->string]
   [y : (Option String) #:= #false #:<-> xml-attribute-value->string]
   [width : (Option String) #:= #false #:<-> xml-attribute-value->string]
   [height : (Option String) #:= #false #:<-> xml-attribute-value->string]
   [result : (Option String) #:= #false #:<-> xml-attribute-value->string]))

(define-svg-attribute style
  ([class : (Option (Listof Symbol)) #:= #false #:<-> xml-attribute-value->symbol-list]
   [style : (Option String)  #:= #false #:<-> xml-attribute-value->string]))

(define-svg-attribute presentation
  ([alignment-baseline : (Option String) #:= #false #:<-> xml-attribute-value->string]
   [baseline-shift : (Option String) #:= #false #:<-> xml-attribute-value->string]
   [clip : (Option String) #:= #false #:<-> xml-attribute-value->string]
   [clip-path : (Option String) #:= #false #:<-> xml-attribute-value->string]
   [clip-rule : (Option String) #:= #false #:<-> xml-attribute-value->string]
   [color : (Option String) #:= #false #:<-> xml-attribute-value->string]
   [color-interpolation : (Option String) #:= #false #:<-> xml-attribute-value->string]
   [color-interpolation-filters : (Option String) #:= #false #:<-> xml-attribute-value->string]
   [color-profile : (Option String) #:= #false #:<-> xml-attribute-value->string]
   [color-rendering : (Option String) #:= #false #:<-> xml-attribute-value->string]
   [cursor : (Option String) #:= #false #:<-> xml-attribute-value->string]
   [direction : (Option String) #:= #false #:<-> xml-attribute-value->string]
   [display : (Option String) #:= #false #:<-> xml-attribute-value->string]
   [dominant-baseline : (Option String) #:= #false #:<-> xml-attribute-value->string]
   [enable-background : (Option String) #:= #false #:<-> xml-attribute-value->string]
   [fill : (Option String) #:= #false #:<-> xml-attribute-value->string]
   [fill-opacity : (Option String) #:= #false #:<-> xml-attribute-value->string]
   [fill-rule : (Option String) #:= #false #:<-> xml-attribute-value->string]
   [filter : (Option String) #:= #false #:<-> xml-attribute-value->string]
   [flood-color : (Option String) #:= #false #:<-> xml-attribute-value->string]
   [flood-opacity : (Option String) #:= #false #:<-> xml-attribute-value->string]
   [font-family : (Option String) #:= #false #:<-> xml-attribute-value->string]
   [font-size : (Option String) #:= #false #:<-> xml-attribute-value->string]
   [font-size-adjust : (Option String) #:= #false #:<-> xml-attribute-value->string]
   [font-stretch : (Option String) #:= #false #:<-> xml-attribute-value->string]
   [font-style : (Option String) #:= #false #:<-> xml-attribute-value->string]
   [font-variant : (Option String) #:= #false #:<-> xml-attribute-value->string]
   [font-weight : (Option String) #:= #false #:<-> xml-attribute-value->string]
   [glyph-orientation-horizontal : (Option String) #:= #false #:<-> xml-attribute-value->string]
   [glyph-orientation-vertical : (Option String) #:= #false #:<-> xml-attribute-value->string]
   [image-rendering : (Option String) #:= #false #:<-> xml-attribute-value->string]
   [kerning : (Option String) #:= #false #:<-> xml-attribute-value->string]
   [letter-spacing : (Option String) #:= #false #:<-> xml-attribute-value->string]
   [lighting-color : (Option String) #:= #false #:<-> xml-attribute-value->string]
   [marker-end : (Option String) #:= #false #:<-> xml-attribute-value->string]
   [marker-mid : (Option String) #:= #false #:<-> xml-attribute-value->string]
   [marker-start : (Option String) #:= #false #:<-> xml-attribute-value->string]
   [mask : (Option String) #:= #false #:<-> xml-attribute-value->string]
   [opacity : (Option String) #:= #false #:<-> xml-attribute-value->string]
   [overflow : (Option String) #:= #false #:<-> xml-attribute-value->string]
   [pointer-events : (Option String) #:= #false #:<-> xml-attribute-value->string]
   [shape-rendering : (Option String) #:= #false #:<-> xml-attribute-value->string]
   [stop-color : (Option String) #:= #false #:<-> xml-attribute-value->string]
   [stop-opacity : (Option String) #:= #false #:<-> xml-attribute-value->string]
   [stroke : (Option String) #:= #false #:<-> xml-attribute-value->string]
   [stroke-dasharray : (Option String) #:= #false #:<-> xml-attribute-value->string]
   [stroke-dashoffset : (Option String) #:= #false #:<-> xml-attribute-value->string]
   [stroke-linecap : (Option String) #:= #false #:<-> xml-attribute-value->string]
   [stroke-linejoin : (Option String) #:= #false #:<-> xml-attribute-value->string]
   [stroke-miterlimit : (Option String) #:= #false #:<-> xml-attribute-value->string]
   [stroke-opacity : (Option String) #:= #false #:<-> xml-attribute-value->string]
   [stroke-width : (Option String) #:= #false #:<-> xml-attribute-value->string]
   [text-anchor : (Option String) #:= #false #:<-> xml-attribute-value->string]
   [text-decoration : (Option String) #:= #false #:<-> xml-attribute-value->string]
   [text-rendering : (Option String) #:= #false #:<-> xml-attribute-value->string]
   [unicode-bidi : (Option String) #:= #false #:<-> xml-attribute-value->string]
   [visibility : (Option String) #:= #false #:<-> xml-attribute-value->string]
   [word-spacing : (Option String) #:= #false #:<-> xml-attribute-value->string]
   [writing-mode : (Option String) #:= #false #:<-> xml-attribute-value->string]))

(define-svg-attribute animation-target
  ([attributeName : (Option String) #:= #false #:<-> xml-attribute-value->string]
   [attributeType : (Option String) #:= #false #:<-> xml-attribute-value->string]))

(define-svg-attribute animation-timing
  ([begin : (Option String) #:= #false #:<-> xml-attribute-value->string]
   [dur : (Option String) #:= #false #:<-> xml-attribute-value->string]
   [end : (Option String) #:= #false #:<-> xml-attribute-value->string]
   [fill : (Option String) #:= #false #:<-> xml-attribute-value->string]
   [max : (Option String) #:= #false #:<-> xml-attribute-value->string]
   [min : (Option String) #:= #false #:<-> xml-attribute-value->string]
   [repeatCount : (Option String) #:= #false #:<-> xml-attribute-value->string]
   [repeatDur : (Option String) #:= #false #:<-> xml-attribute-value->string]
   [restart : (Option String) #:= #false #:<-> xml-attribute-value->string]))

(define-svg-attribute animation-value
  ([by : (Option String) #:= #false #:<-> xml-attribute-value->string]
   [calcMode : (Option String) #:= #false #:<-> xml-attribute-value->string]
   [from : (Option String) #:= #false #:<-> xml-attribute-value->string]
   [keySplines : (Option String) #:= #false #:<-> xml-attribute-value->string]
   [keyTimes : (Option String) #:= #false #:<-> xml-attribute-value->string]
   [to : (Option String) #:= #false #:<-> xml-attribute-value->string]
   [values : (Option String) #:= #false #:<-> xml-attribute-value->string]))

(define-svg-attribute animation-addition
  ([accumulate : (Option String) #:= #false #:<-> xml-attribute-value->string]
   [additive : (Option String) #:= #false #:<-> xml-attribute-value->string]))

(define-svg-attribute document-event
  ([onabort : (Option String) #:= #false #:<-> xml-attribute-value->string]
   [onerror : (Option String) #:= #false #:<-> xml-attribute-value->string]
   [onresize : (Option String) #:= #false #:<-> xml-attribute-value->string]
   [onscroll : (Option String) #:= #false #:<-> xml-attribute-value->string]
   [onunload : (Option String) #:= #false #:<-> xml-attribute-value->string]
   [onzoom : (Option String) #:= #false #:<-> xml-attribute-value->string]))

(define-svg-attribute animation-event
  ([onbegin : (Option String)  #:= #false #:<-> xml-attribute-value->string]
   [onend : (Option String)    #:= #false #:<-> xml-attribute-value->string]
   [onrepeat : (Option String) #:= #false #:<-> xml-attribute-value->string]
   [onload : (Option String)   #:= #false #:<-> xml-attribute-value->string]))

(define-svg-attribute graphical-event
  ([onfocusin : (Option String)   #:= #false #:<-> xml-attribute-value->string]
   [onfocusout : (Option String)  #:= #false #:<-> xml-attribute-value->string]
   [onactivate : (Option String)  #:= #false #:<-> xml-attribute-value->string]
   [onclick : (Option String)     #:= #false #:<-> xml-attribute-value->string]
   [onmousedown : (Option String) #:= #false #:<-> xml-attribute-value->string]
   [onmouseup : (Option String)   #:= #false #:<-> xml-attribute-value->string]
   [onmouseover : (Option String) #:= #false #:<-> xml-attribute-value->string]
   [onmousemove : (Option String) #:= #false #:<-> xml-attribute-value->string]
   [onmouseout : (Option String)  #:= #false #:<-> xml-attribute-value->string]
   [onload : (Option String)      #:= #false #:<-> xml-attribute-value->string]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define svg-attributes*-extract-core : (-> (Listof XML-Element-Attribute*) (Values (Option Symbol) (Option SVG:Attr:Core) (Listof XML-Element-Attribute*)))
  (lambda [attrs]
    (let*-values ([(?id rest) (xml-attributes*-extract attrs 'id)]
                  [(?core rest) (extract-svg:attr:core rest)])
      (values (and ?id (xml-attribute-value->symbol ?id)) ?core rest))))

(define svg-core->xml-attributes : (-> (Option XML-Source) (Option Symbol) (Option SVG:Attr:Core) (Listof (Pairof Symbol String)))
  (lambda [?src ?id ?core]
    (define core-attrs (if (not ?core) null (svg:attr:core->xml-attributes ?core)))

    (cond [(or ?id) (cons (cons 'id (symbol->immutable-string ?id)) core-attrs)]
          [(or ?src) (cons (cons 'id (xml-source->id-value ?src)) core-attrs)]
          [else core-attrs])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define svg-report-unrecognized-attributes : (-> (Option XML:Name) (Listof XML-Element-Attribute*) Void)
  (lambda [elem srtta]
    (for ([attr (in-list (reverse srtta))])
      (define v (cdr attr))
      (make+exn:svg:unrecognized
       (cond [(list? v) (list* (car attr) v)]
             [else (list (car attr) v)])
       elem))))

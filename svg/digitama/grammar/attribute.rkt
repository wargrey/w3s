#lang typed/racket/base

(provide (all-defined-out))
(provide (for-syntax (all-defined-out)))

(require racket/symbol)

(require digimon/syntax)

(require sgml/digitama/dialect)
(require sgml/digitama/digicore)
(require sgml/digitama/grammar)
(require sgml/digitama/datatype)
(require sgml/digitama/shared/datatype)

(require "datatype.rkt")

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
           #:report svg-report-unrecognized-attributes make+exn:svg:range
           options ...)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct svg-attribute () #:type-name SVG-Attribute #:transparent)

(define-svg-attribute core
  ([xml:base : (Option String)                                #:= #false #:<-> xml:attr-value*->string]
   [xml:lang : (Option String)                                #:= #false #:<-> xml:attr-value*->string]
   [xml:space : (Option Symbol)                               #:= #false #:<-> xml:attr-value*->symbol]))

(define-svg-attribute condition
  ([requiredFeatures : (Option (Listof Symbol))               #:= #false #:<-> xml:attr-value*->symbol-list]
   [requiredExtensions : (Option String)                      #:= #false #:<-> xml:attr-value*->string]
   [systemLanguage : (Option String)                          #:= #false #:<-> xml:attr-value*->string]))

(define-svg-attribute external
  ([externalResourcesRequired : (Option XML-Boolean)          #:= #false #:<-> xml:attr-value*->boolean]))

(define-svg-attribute xlink
  ([xlink:href : (Option String)                              #:= #false #:<-> svg:attr-value*->IRI]
   [xlink:type : (Option SVG:KW:LinkType)                     #:= #false #:<-> svg:attr-value*->kw:link:type]
   [xlink:role : (Option String)                              #:= #false #:<-> svg:attr-value*->IRI]
   [xlink:arcrole : (Option String)                           #:= #false #:<-> svg:attr-value*->IRI]
   [xlink:title : (Option String)                             #:= #false #:<-> xml:attr-value*->string]
   [xlink:show : (Option SVG:KW:LinkShow)                     #:= #false #:<-> svg:attr-value*->kw:link:show]
   [xlink:actuate : (Option SVG:KW:LinkActuate)               #:= #false #:<-> svg:attr-value*->kw:link:actuate]))

(define-svg-attribute filter-primitive
  ([x : (Option XML-Dimension)                                #:= #false #:<-> svg:attr-value*->coordinate]
   [y : (Option XML-Dimension)                                #:= #false #:<-> svg:attr-value*->coordinate]
   [width : (Option XML-Dimension)                            #:= #false #:<-> svg:attr-value*->dim:length]
   [height : (Option XML-Dimension)                           #:= #false #:<-> svg:attr-value*->dim:length]
   [result : (Option Symbol)                                  #:= #false #:<-> xml:attr-value*->symbol]))

(define-svg-attribute style
  ([class : (Option (Listof Symbol))                          #:= #false #:<-> xml:attr-value*->symbol-list]
   [style : (Option String)                                   #:= #false #:<-> xml:attr-value*->string]))

(define-svg-attribute presentation
  ([alignment-baseline : (Option String) #:= #false #:<-> xml:attr-value*->string]
   [baseline-shift : (Option String) #:= #false #:<-> xml:attr-value*->string]
   [clip : (Option String) #:= #false #:<-> xml:attr-value*->string]
   [clip-path : (Option String) #:= #false #:<-> xml:attr-value*->string]
   [clip-rule : (Option String) #:= #false #:<-> xml:attr-value*->string]
   [color : (Option String) #:= #false #:<-> xml:attr-value*->string]
   [color-interpolation : (Option String) #:= #false #:<-> xml:attr-value*->string]
   [color-interpolation-filters : (Option String) #:= #false #:<-> xml:attr-value*->string]
   [color-profile : (Option String) #:= #false #:<-> xml:attr-value*->string]
   [color-rendering : (Option String) #:= #false #:<-> xml:attr-value*->string]
   [cursor : (Option String) #:= #false #:<-> xml:attr-value*->string]
   [direction : (Option String) #:= #false #:<-> xml:attr-value*->string]
   [display : (Option String) #:= #false #:<-> xml:attr-value*->string]
   [dominant-baseline : (Option String) #:= #false #:<-> xml:attr-value*->string]
   [enable-background : (Option String) #:= #false #:<-> xml:attr-value*->string]
   [fill : (Option String) #:= #false #:<-> xml:attr-value*->string]
   [fill-opacity : (Option String) #:= #false #:<-> xml:attr-value*->string]
   [fill-rule : (Option String) #:= #false #:<-> xml:attr-value*->string]
   [filter : (Option String) #:= #false #:<-> xml:attr-value*->string]
   [flood-color : (Option String) #:= #false #:<-> xml:attr-value*->string]
   [flood-opacity : (Option String) #:= #false #:<-> xml:attr-value*->string]
   [font-family : (Option String) #:= #false #:<-> xml:attr-value*->string]
   [font-size : (Option String) #:= #false #:<-> xml:attr-value*->string]
   [font-size-adjust : (Option String) #:= #false #:<-> xml:attr-value*->string]
   [font-stretch : (Option String) #:= #false #:<-> xml:attr-value*->string]
   [font-style : (Option String) #:= #false #:<-> xml:attr-value*->string]
   [font-variant : (Option String) #:= #false #:<-> xml:attr-value*->string]
   [font-weight : (Option String) #:= #false #:<-> xml:attr-value*->string]
   [glyph-orientation-horizontal : (Option String) #:= #false #:<-> xml:attr-value*->string]
   [glyph-orientation-vertical : (Option String) #:= #false #:<-> xml:attr-value*->string]
   [image-rendering : (Option String) #:= #false #:<-> xml:attr-value*->string]
   [kerning : (Option String) #:= #false #:<-> xml:attr-value*->string]
   [letter-spacing : (Option String) #:= #false #:<-> xml:attr-value*->string]
   [lighting-color : (Option String) #:= #false #:<-> xml:attr-value*->string]
   [marker-end : (Option String) #:= #false #:<-> xml:attr-value*->string]
   [marker-mid : (Option String) #:= #false #:<-> xml:attr-value*->string]
   [marker-start : (Option String) #:= #false #:<-> xml:attr-value*->string]
   [mask : (Option String) #:= #false #:<-> xml:attr-value*->string]
   [opacity : (Option String) #:= #false #:<-> xml:attr-value*->string]
   [overflow : (Option String) #:= #false #:<-> xml:attr-value*->string]
   [pointer-events : (Option String) #:= #false #:<-> xml:attr-value*->string]
   [shape-rendering : (Option String) #:= #false #:<-> xml:attr-value*->string]
   [stop-color : (Option String) #:= #false #:<-> xml:attr-value*->string]
   [stop-opacity : (Option String) #:= #false #:<-> xml:attr-value*->string]
   [stroke : (Option String) #:= #false #:<-> xml:attr-value*->string]
   [stroke-dasharray : (Option String) #:= #false #:<-> xml:attr-value*->string]
   [stroke-dashoffset : (Option String) #:= #false #:<-> xml:attr-value*->string]
   [stroke-linecap : (Option String) #:= #false #:<-> xml:attr-value*->string]
   [stroke-linejoin : (Option String) #:= #false #:<-> xml:attr-value*->string]
   [stroke-miterlimit : (Option String) #:= #false #:<-> xml:attr-value*->string]
   [stroke-opacity : (Option String) #:= #false #:<-> xml:attr-value*->string]
   [stroke-width : (Option String) #:= #false #:<-> xml:attr-value*->string]
   [text-anchor : (Option String) #:= #false #:<-> xml:attr-value*->string]
   [text-decoration : (Option String) #:= #false #:<-> xml:attr-value*->string]
   [text-rendering : (Option String) #:= #false #:<-> xml:attr-value*->string]
   [unicode-bidi : (Option String) #:= #false #:<-> xml:attr-value*->string]
   [visibility : (Option String) #:= #false #:<-> xml:attr-value*->string]
   [word-spacing : (Option String) #:= #false #:<-> xml:attr-value*->string]
   [writing-mode : (Option String) #:= #false #:<-> xml:attr-value*->string]))

(define-svg-attribute animation-target
  ([attributeName : (Option Symbol)                           #:= #false #:<-> xml:attr-value*->symbol]
   [attributeType : (Option SVG:KW:AniTargetType)             #:= #false #:<-> svg:attr-value*->kw:ani:target:type]))

(define-svg-attribute animation-timing
  ([begin : (Option String) #:= #false #:<-> xml:attr-value*->string]
   [dur : (Option String) #:= #false #:<-> xml:attr-value*->string]
   [end : (Option String) #:= #false #:<-> xml:attr-value*->string]
   [fill : (Option String) #:= #false #:<-> xml:attr-value*->string]
   [max : (Option String) #:= #false #:<-> xml:attr-value*->string]
   [min : (Option String) #:= #false #:<-> xml:attr-value*->string]
   [repeatCount : (Option String) #:= #false #:<-> xml:attr-value*->string]
   [repeatDur : (Option String) #:= #false #:<-> xml:attr-value*->string]
   [restart : (Option String) #:= #false #:<-> xml:attr-value*->string]))

(define-svg-attribute animation-value
  ([calcMode : (Option SVG:KW:AniInterpolation)               #:= #false #:<-> svg:attr-value*->kw:ani:interpolation]
   [values : (Option (Listof String))                         #:= #false #:<-> svg:attr-value*->animated-values svg:attr-animated-values->value]
   [keyTimes : (Option (Listof Flonum))                       #:= #false #:<-> svg:attr-value*->animated-times svg:attr-animated-values->value]
   [keySplines : (Option (Listof SVG-Animated-Control-Point)) #:= #false #:<-> svg:attr-value*->animated-splines svg:attr-animated-splines->value]
   [from : (Option String)                                    #:= #false #:<-> xml:attr-value*->string]
   [to : (Option String)                                      #:= #false #:<-> xml:attr-value*->string]
   [by : (Option String)                                      #:= #false #:<-> xml:attr-value*->string]))

(define-svg-attribute animation-addition
  ([additive : (Option SVG:KW:AniAdditive)                    #:= #false #:<-> svg:attr-value*->kw:ani:additive]
   [accumulate : (Option SVG:KW:AniAccumulate)                #:= #false #:<-> svg:attr-value*->kw:ani:accumulate]))

(define-svg-attribute document-event
  ([onabort : (Option String)                                 #:= #false #:<-> xml:attr-value*->string]
   [onerror : (Option String)                                 #:= #false #:<-> xml:attr-value*->string]
   [onresize : (Option String)                                #:= #false #:<-> xml:attr-value*->string]
   [onscroll : (Option String)                                #:= #false #:<-> xml:attr-value*->string]
   [onunload : (Option String)                                #:= #false #:<-> xml:attr-value*->string]
   [onzoom : (Option String)                                  #:= #false #:<-> xml:attr-value*->string]))

(define-svg-attribute animation-event
  ([onbegin : (Option String)                                 #:= #false #:<-> xml:attr-value*->string]
   [onend : (Option String)                                   #:= #false #:<-> xml:attr-value*->string]
   [onrepeat : (Option String)                                #:= #false #:<-> xml:attr-value*->string]
   [onload : (Option String)                                  #:= #false #:<-> xml:attr-value*->string]))

(define-svg-attribute graphical-event
  ([onfocusin : (Option String)                               #:= #false #:<-> xml:attr-value*->string]
   [onfocusout : (Option String)                              #:= #false #:<-> xml:attr-value*->string]
   [onactivate : (Option String)                              #:= #false #:<-> xml:attr-value*->string]
   [onclick : (Option String)                                 #:= #false #:<-> xml:attr-value*->string]
   [onmousedown : (Option String)                             #:= #false #:<-> xml:attr-value*->string]
   [onmouseup : (Option String)                               #:= #false #:<-> xml:attr-value*->string]
   [onmouseover : (Option String)                             #:= #false #:<-> xml:attr-value*->string]
   [onmousemove : (Option String)                             #:= #false #:<-> xml:attr-value*->string]
   [onmouseout : (Option String)                              #:= #false #:<-> xml:attr-value*->string]
   [onload : (Option String)                                  #:= #false #:<-> xml:attr-value*->string]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define svg-attributes*-extract-core : (-> (Listof XML-Element-Attribute*) (Values (Option Symbol) (Option SVG:Attr:Core) (Listof XML-Element-Attribute*)))
  (lambda [attrs]
    (let*-values ([(?id rest) (xml-attributes*-extract attrs 'id)]
                  [(?core rest) (extract-svg:attr:core rest)])
      (values (and ?id (xml:attr-value*->symbol ?id)) ?core rest))))

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

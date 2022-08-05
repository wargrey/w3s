#lang typed/racket/base

(provide (all-defined-out))
(provide (rename-out [XML-Source SVG-Source]))

(require digimon/syntax)

(require sgml/digitama/dialect)
(require sgml/digitama/digicore)
(require sgml/digitama/document)
(require sgml/digitama/namespace)
(require sgml/digitama/datatype)

(require sgml/digitama/plain/grammar)
(require sgml/digitama/grammar)

(require "grammar/attribute.rkt")

(require (for-syntax racket/list))
(require (for-syntax syntax/parse))
(require (for-syntax digimon/symbol))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-for-syntax (racket->svg-name <e>)
  (syntax-parse <e> #:literals [:]
    [(id tag : ID) (list (make-identifier #'id "svg:~a") #'tag (make-identifier #'ID "SVG:~a"))]
    [(id tag) (list (make-identifier #'id "svg:~a") #'tag (make-identifier #'id "SVG:~a" symbol-titlecase))]
    [(tag : ID) (list (make-identifier #'tag "svg:~a") #'tag (make-identifier #'ID "SVG:~a"))]
    [tag:id (list (make-identifier #'tag "svg:~a") #'tag (make-identifier #'tag "SVG:~a" symbol-titlecase))]
    [_ (raise-syntax-error 'define-svg-element "invalid element name" <e>)]))

(define-for-syntax (racket->attr-names <a>)
  (define attrib (symbol->immutable-string (syntax-e <a>)))
  (cons (format-id <a> "attr:~a" attrib)
        (cdr (racket->svg:attr-names <a>))))

(define-syntax (define-svg-element stx)
  (syntax-parse stx #:literals [:]
    [(_ svg-elem : SVG-Elem
        #:refine-element refine-def #:head head-defs #:body body-defs
        (~optional (~seq #:attribute-categories [attrib ...]) #:defaults ([(attrib 1) null]))
        defs-rest ...)
     (with-syntax* ([([svg:attr SVG:Attr extract-svg:attr] ...) (map racket->attr-names (syntax->list #'(attrib ...)))])
       (syntax/loc stx
         (define-dom-element svg-elem : SVG-Elem
           #:refine-element refine-def #:head head-defs #:body body-defs
           #:attribute-categories ([svg:attr : SVG:Attr extract-svg:attr] ...)
           defs-rest ...)))]))

(define-syntax (define-svg-subdom stx)
  (syntax-parse stx #:literals [:]
    [(_ subsvg : SubSVG
        #:subdom-refine refine-def #:head head-defs #:body body-defs
        (~optional (~seq #:attribute-categories [attrib ...]) #:defaults ([(attrib 1) null]))
        subfield-defs
        (defsvg svg-elem+name defs-rest ...) ...
        (~optional (~seq #:subdom [sub-defs ...]) #:defaults ([(sub-defs 1) null])))
     (with-syntax* ([([svg:attr SVG:Attr extract-svg:attr] ...) (map racket->attr-names (syntax->list #'(attrib ...)))]
                    [([svg-elem svg-tagname SVG-Elem] ...) (map racket->svg-name (syntax->list #'(svg-elem+name ...)))])
       (syntax/loc stx
         (define-xml-subdom subsvg : SubSVG
           #:subdom-refine refine-def #:head head-defs #:body body-defs
           #:attribute-categories ([svg:attr : SVG:Attr extract-svg:attr] ...)
           subfield-defs

           (defsvg [svg-elem svg-tagname] : SVG-Elem defs-rest ...) ...

           #:subdom [sub-defs ...])))]))

(define-syntax (define-svg-dom stx)
  (syntax-parse stx #:literals [:]
    [(_ svg : SVG #:dom-refine refine head-defs body-defs
        (defsvg svg-elem+name : SVG-Elem defs-rest ...) ...
        (~optional (~seq #:subdom [sub-defs ...]) #:defaults ([(sub-defs 1) null])))
     (with-syntax* ([([svg-elem svg-tagname SVG-Elem] ...) (map racket->svg-name (syntax->list #'(svg-elem+name ...)))]
                    [(unknown Unknown refine-unknown)
                     (for/list ([id (in-list '(svg-unknown SVG-Unknown svg-refine-unknown))])
                       (datum->syntax #'svg id))])
       (syntax/loc stx
         (define-xml-dom svg : SVG
           #:dom-refine [svg-attributes*-extract-core refine svg-report-unrecognized-element/attributes xml-contents*->svg-elements]
           head-defs body-defs
           #:unknown [unknown : Unknown #:refine refine-unknown]

           (defsvg [svg-elem svg-tagname] : SVG-Elem defs-rest ...) ...

           #:subdom [sub-defs ...])))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-svg-dom svg-element : SVG-Element
  #:dom-refine xml-element*->svg-element
  ([source : (Option XML-Source) #false]
   [id : (Option Symbol) #false]
   [attr:core : (Option SVG:Attr:Core) #false])
  ([content : (Listof SVG-Element) null]) #| only 5 empty elements |#
  
  #:subdom
  [(define-svg-subdom svg-descriptive-element : SVG-Descriptive-Element ()
     (define-svg-element desc #:attribute-categories [style] ())
     (define-svg-element title #:attribute-categories [style] ())
     (define-svg-element metadata ()))

   (define-svg-subdom svg-stylish-element : SVG-Stylish-Element
     #:attribute-categories [style presentation] ()

     #:subdom
     [(define-svg-subdom svg-visual-element : SVG-Visual-Element
        #:attribute-categories [graphical-event external] ()

        #:subdom
        [(define-svg-subdom svg-structural-element : SVG-Structural-Element ()
           (define-svg-element [svg : SVG]
             #:attribute-categories [condition document-event]
             ([x : (Option String) #:=> xml-attribute-value->string #false]
              [y : (Option String) #:=> xml-attribute-value->string #false]
              [width : (Option String) #:=> xml-attribute-value->string #false]
              [height : (Option String) #:=> xml-attribute-value->string #false]
              [baseProfile : (Option String) #:=> xml-attribute-value->string #false]
              [contentScriptType : (Option String) #:=> xml-attribute-value->string #false]
              [contentStyleType : (Option String) #:=> xml-attribute-value->string #false]
              [preserveAspectRatio : (Option String) #:=> xml-attribute-value->string #false]
              [version : (Option String) #:=> xml-attribute-value->string #false]
              [viewBox : (Option String) #:=> xml-attribute-value->string #false]
              [zoomAndPan : (Option String) #:=> xml-attribute-value->string #false]))

           (define-svg-element g
             #:attribute-categories [condition]
             ([transform : (Option String) #:=> xml-attribute-value->string #false]))

           (define-svg-element defs
             #:attribute-categories [condition]
             ([transform : (Option String) #:=> xml-attribute-value->string #false]))

           (define-svg-element symbol
             #:attribute-categories []
             ([preserveAspectRatio : (Option String) #:=> xml-attribute-value->string #false]
              [viewBox : (Option String) #:=> xml-attribute-value->string #false])))

         (define-svg-subdom svg-graphics-element : SVG-Graphics-Element
           #:attribute-categories [condition]
           ([transform : (Option String) #:=> xml-attribute-value->string #false])

           (define-svg-element text
             ([x : (Option String) #:=> xml-attribute-value->string #false]
              [y : (Option String) #:=> xml-attribute-value->string #false]
              [dx : (Option String) #:=> xml-attribute-value->string #false]
              [dy : (Option String) #:=> xml-attribute-value->string #false]
              [lengthAdjust : (Option String) #:=> xml-attribute-value->string #false]
              [rotate : (Option String) #:=> xml-attribute-value->string #false]
              [textLength : (Option String) #:=> xml-attribute-value->string #false]))
           
           #:subdom
           [(define-svg-subdom svg-shape-element : SVG-Shape-Element ()
              (define-svg-element path
                ([d : (Option String) #:=> xml-attribute-value->string #false]
                 [pathLength : (Option String) #:=> xml-attribute-value->string #false]))

              (define-svg-element line
                ([x1 : (Option String) #:=> xml-attribute-value->string #false]
                 [x2 : (Option String) #:=> xml-attribute-value->string #false]
                 [y1 : (Option String) #:=> xml-attribute-value->string #false]
                 [y2 : (Option String) #:=> xml-attribute-value->string #false]))
              
              (define-svg-element rect
                ([x : (Option String) #:=> xml-attribute-value->string #false]
                 [y : (Option String) #:=> xml-attribute-value->string #false]
                 [width : (Option String) #:=> xml-attribute-value->string #false]
                 [height : (Option String) #:=> xml-attribute-value->string #false]
                 [rx : (Option String) #:=> xml-attribute-value->string #false]
                 [ry : (Option String) #:=> xml-attribute-value->string #false]))
              
              (define-svg-element circle
                ([cx : (Option String) #:=> xml-attribute-value->string #false]
                 [cy : (Option String) #:=> xml-attribute-value->string #false]
                 [r : (Option String) #:=> xml-attribute-value->string #false]))
              
              (define-svg-element ellipse
                ([cx : (Option String) #:=> xml-attribute-value->string #false]
                 [cy : (Option String) #:=> xml-attribute-value->string #false]
                 [rx : (Option String) #:=> xml-attribute-value->string #false]
                 [ry : (Option String) #:=> xml-attribute-value->string #false]))

              (define-svg-element polyline
                ([points : (Option String) #:=> xml-attribute-value->string #false]))
              
              (define-svg-element polygon
                ([points : (Option String) #:=> xml-attribute-value->string #false])))

            (define-svg-subdom svg-graphics-referencing-element : SVG-Graphics-Referencing-Element
              #:attribute-categories [xlink]
              ([x : (Option String) #:=> xml-attribute-value->string #false]
               [y : (Option String) #:=> xml-attribute-value->string #false]
               [width : (Option String) #:=> xml-attribute-value->string #false]
               [height : (Option String) #:=> xml-attribute-value->string #false])

              (define-svg-element use ())
              
              (define-svg-element image
                ([preserveAspectRatio : (Option String) #:=> xml-attribute-value->string #false])))])

         (define-svg-subdom svg-text-content-element : SVG-Text-Content-Element ()
           (define-svg-element tspan
             ([x : (Option String) #:=> xml-attribute-value->string #false]
              [y : (Option String) #:=> xml-attribute-value->string #false]
              [dx : (Option String) #:=> xml-attribute-value->string #false]
              [dy : (Option String) #:=> xml-attribute-value->string #false]
              [rotate : (Option String) #:=> xml-attribute-value->string #false]
              [textLength : (Option String) #:=> xml-attribute-value->string #false]
              [lengthAdjust : (Option String) #:=> xml-attribute-value->string #false]))
           
           (define-svg-element tref #:attribute-categories [xlink] ())

           (define-svg-element [text-path textPath]
             #:attribute-categories [xlink]
             ([method : (Option String) #:=> xml-attribute-value->string #false]
              [spacing : (Option String) #:=> xml-attribute-value->string #false]
              [startOffset : (Option String) #:=> xml-attribute-value->string #false]))

           (define-svg-element [alt-glyph altGlyph]
             #:attribute-categories [xlink]
             ([x : (Option String) #:=> xml-attribute-value->string #false]
              [y : (Option String) #:=> xml-attribute-value->string #false]
              [dx : (Option String) #:=> xml-attribute-value->string #false]
              [dy : (Option String) #:=> xml-attribute-value->string #false]
              [glyphRef : (Option String) #:=> xml-attribute-value->string #false]
              [format : (Option String) #:=> xml-attribute-value->string #false]
              [rotate : (Option String) #:=> xml-attribute-value->string #false])))])])

   (define-svg-subdom svg-light-source-element : SVG-Light-Source-Element ()
     (define-svg-element [distant-light feDistantLight]
       ([azimuth : (Option String)   #:=> xml-attribute-value->string #false]
        [elevation : (Option String) #:=> xml-attribute-value->string #false]))

     (define-svg-element [svg:point-light fePointLight]
       ([x : (Option String) #:=> xml-attribute-value->string #false]
        [y : (Option String) #:=> xml-attribute-value->string #false]
        [z : (Option String) #:=> xml-attribute-value->string #false]))
     
     (define-svg-element [svg:spot-light feSpotLight]
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

     (define-svg-element animate
       #:attribute-categories [animation-target animation-value animation-addition presentation] ())

     (define-svg-element set
       #:attribute-categories [animation-target]
       ([to : (Option String) #:=> xml-attribute-value->string #false]))
     
     (define-svg-element [animate-color animateColor]
       #:attribute-categories [animation-target animation-value animation-addition presentation] ())

     (define-svg-element [animate-motion animateMotion]
       #:attribute-categories [animation-value animation-addition]
       ([path : (Option String) #:=> xml-attribute-value->string #false]
        [keyPoints : (Option String) #:=> xml-attribute-value->string #false]
        [rotate : (Option String) #:=> xml-attribute-value->string #false]
        [origin : (Option String) #:=> xml-attribute-value->string #false]))
     
     (define-svg-element [animate-transform animateTransform]
       #:attribute-categories [animation-target animation-value animation-addition presentation]
       ([type : (Option String) #:=> xml-attribute-value->string #false])))])

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

(define xml-contents*->svg-elements : (->* ((Listof (U XML-Content* XML-Subdatum*))) ((Option XML:Name) (Listof Symbol)) (Listof SVG-Element))
  (lambda [contents [elem #false] [valid-tagnames null]]
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
(define svg-report-unrecognized-element/attributes : (case-> [-> (Option XML:Name) (Listof XML-Element-Attribute*) Void]
                                                             [-> XML-Element* Void])
  (case-lambda
    [(elem)
     (make+exn:svg:unrecognized (car elem))
     (void)]
    [(elem srtta)
     (for ([attr (in-list (reverse srtta))])
      (define v (cdr attr))
      (make+exn:svg:unrecognized
       (cond [(list? v) (list* (car attr) v)]
             [else (list (car attr) v)])
       elem))]))

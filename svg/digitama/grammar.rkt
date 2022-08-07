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
           
           #:subdom [sub-defs ...]
           (defsvg [svg-elem svg-tagname] : SVG-Elem defs-rest ...) ...)))]))

(define-syntax (define-svg-dom stx)
  (syntax-parse stx #:literals [:]
    [(_ svg : SVG #:dom-refine refine head-defs body-defs
        (~optional (~seq #:subdom [sub-defs ...]) #:defaults ([(sub-defs 1) null]))
        (defsvg svg-elem+name defs-rest ...) ...)
     (with-syntax* ([([svg-elem svg-tagname SVG-Elem] ...) (map racket->svg-name (syntax->list #'(svg-elem+name ...)))]
                    [(unknown Unknown refine-unknown)
                     (for/list ([id (in-list '(svg-unknown SVG-Unknown svg-refine-unknown))])
                       (datum->syntax #'svg id))])
       (syntax/loc stx
         (define-xml-dom svg : SVG
           #:dom-refine [svg-attributes*-extract-core refine svg-report-unrecognized-element/attributes xml-contents*->svg-elements]
           head-defs body-defs
           #:unknown [unknown : Unknown #:refine refine-unknown]

           #:subdom [sub-defs ...]
           (defsvg [svg-elem svg-tagname] : SVG-Elem defs-rest ...) ...)))]))

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

     (define-svg-element filter
       #:attribute-categories [xlink external]
       ([x : (Option String) #:-> xml-attribute-value->string #false]
        [y : (Option String) #:-> xml-attribute-value->string #false]
        [width : (Option String) #:-> xml-attribute-value->string #false]
        [height : (Option String) #:-> xml-attribute-value->string #false]
        [filterRes : (Option String) #:-> xml-attribute-value->string #false]
        [filterUnits : (Option String) #:-> xml-attribute-value->string #false]
        [primitiveUnits : (Option String) #:-> xml-attribute-value->string #false]))

     (define-svg-element stop ; for gradient elements
       ([offset : (Option String) #:-> xml-attribute-value->string #false]))

     #:subdom
     [(define-svg-subdom svg-gradient-element : SVG-Gradient-Element
        #:attribute-categories [xlink external]
        ([gradientTransform : (Option String) #:-> xml-attribute-value->string #false]
         [gradientUnits : (Option String) #:-> xml-attribute-value->string #false]
         [spreadMethod : (Option String) #:-> xml-attribute-value->string #false])

        (define-svg-element linearGradient
          ([x1 : (Option String) #:-> xml-attribute-value->string #false]
           [x2 : (Option String) #:-> xml-attribute-value->string #false]
           [y1 : (Option String) #:-> xml-attribute-value->string #false]
           [y2 : (Option String) #:-> xml-attribute-value->string #false]))

        (define-svg-element radialGradient
          ([cx : (Option String) #:-> xml-attribute-value->string #false]
           [cy : (Option String) #:-> xml-attribute-value->string #false]
           [fx : (Option String) #:-> xml-attribute-value->string #false]
           [fy : (Option String) #:-> xml-attribute-value->string #false]
           [r : (Option String) #:-> xml-attribute-value->string #false])))

      (define-svg-subdom svg-container-element : SVG-Container-Element ()
        (define-svg-element pattern
          #:attribute-categories [condition xlink external]
          ([x : (Option String) #:-> xml-attribute-value->string #false]
           [y : (Option String) #:-> xml-attribute-value->string #false]
           [width : (Option String) #:-> xml-attribute-value->string #false]
           [height : (Option String) #:-> xml-attribute-value->string #false]
           [viewBox : (Option String) #:-> xml-attribute-value->string #false]
           [patternContentUnits : (Option String) #:-> xml-attribute-value->string #false]
           [patternTransform : (Option String) #:-> xml-attribute-value->string #false]
           [patternUnits : (Option String) #:-> xml-attribute-value->string #false]
           [preserveAspectRatio : (Option String) #:-> xml-attribute-value->string #false]))
        
        (define-svg-element marker
          #:attribute-categories [external]
          ([markerHeight : (Option String) #:-> xml-attribute-value->string #false]
           [markerUnits : (Option String) #:-> xml-attribute-value->string #false]
           [markerWidth : (Option String) #:-> xml-attribute-value->string #false]
           [orient : (Option String) #:-> xml-attribute-value->string #false]
           [preserveAspectRatio : (Option String) #:-> xml-attribute-value->string #false]
           [refX : (Option String) #:-> xml-attribute-value->string #false]
           [refY : (Option String) #:-> xml-attribute-value->string #false]
           [viewBox : (Option String) #:-> xml-attribute-value->string #false]))
        
        (define-svg-element mask
          #:attribute-categories [condition external]
          ([x : (Option String) #:-> xml-attribute-value->string #false]
           [y : (Option String) #:-> xml-attribute-value->string #false]
           [width : (Option String) #:-> xml-attribute-value->string #false]
           [height : (Option String) #:-> xml-attribute-value->string #false]
           [maskContentUnits : (Option String) #:-> xml-attribute-value->string #false]
           [maskUnits : (Option String) #:-> xml-attribute-value->string #false]))

        (define-svg-element clipPath
          #:attribute-categories [condition external]
          ([clipPathUnits : (Option String) #:-> xml-attribute-value->string #false]
           [transform : (Option String) #:-> xml-attribute-value->string #false]))
        
        (define-svg-element glyph
          ([d : (Option String) #:-> xml-attribute-value->string #false]
           [arabic-form : (Option String) #:-> xml-attribute-value->string #false]
           [glyph-name : (Option String) #:-> xml-attribute-value->string #false]
           [horiz-adv-x : (Option String) #:-> xml-attribute-value->string #false]
           [lang : (Option String) #:-> xml-attribute-value->string #false]
           [orientation : (Option String) #:-> xml-attribute-value->string #false]
           [unicode : (Option String) #:-> xml-attribute-value->string #false]
           [vert-adv-y : (Option String) #:-> xml-attribute-value->string #false]
           [vert-origin-x : (Option String) #:-> xml-attribute-value->string #false]
           [vert-origin-y : (Option String) #:-> xml-attribute-value->string #false]))
        
        (define-svg-element missing-glyph
          ([d : (Option String) #:-> xml-attribute-value->string #false]
           [horiz-adv-x : (Option String) #:-> xml-attribute-value->string #false]
           [vert-adv-y : (Option String) #:-> xml-attribute-value->string #false]
           [vert-origin-x : (Option String) #:-> xml-attribute-value->string #false]
           [vert-origin-y : (Option String) #:-> xml-attribute-value->string #false]))

        #:subdom
        [(define-svg-subdom svg-structural-element : SVG-Structural-Element
           #:attribute-categories [graphical-event external] ()

           (define-svg-element [svg : SVG]
             #:attribute-categories [condition document-event]
             ([x : (Option String) #:-> xml-attribute-value->string #false]
              [y : (Option String) #:-> xml-attribute-value->string #false]
              [width : (Option String) #:-> xml-attribute-value->string #false]
              [height : (Option String) #:-> xml-attribute-value->string #false]
              [viewBox : (Option String) #:-> xml-attribute-value->string #false]
              [baseProfile : (Option String) #:-> xml-attribute-value->string #false]
              [contentScriptType : (Option String) #:-> xml-attribute-value->string #false]
              [contentStyleType : (Option String) #:-> xml-attribute-value->string #false]
              [preserveAspectRatio : (Option String) #:-> xml-attribute-value->string #false]
              [version : (Option String) #:-> xml-attribute-value->string #false]
              [zoomAndPan : (Option String) #:-> xml-attribute-value->string #false]))

           (define-svg-element symbol
             ([preserveAspectRatio : (Option String) #:-> xml-attribute-value->string #false]
              [viewBox : (Option String) #:-> xml-attribute-value->string #false]))

           #:subdom
           [(define-svg-subdom svg-transformable-element : SVG-Transformable-Element
              #:attribute-categories [condition]
              ([transform : (Option String) #:-> xml-attribute-value->string #false])

              (define-svg-element g ())
              (define-svg-element defs ())
              (define-svg-element switch ())

              (define-svg-element a
                #:attribute-categories [xlink]
                ([target : (Option String) #:-> xml-attribute-value->string #false])))])])
      
      (define-svg-subdom svg-visual-element : SVG-Visual-Element
        #:attribute-categories [graphical-event external] ()

        #:subdom
        [(define-svg-subdom svg-graphics-element : SVG-Graphics-Element
           #:attribute-categories [condition]
           ([transform : (Option String) #:-> xml-attribute-value->string #false])

           (define-svg-element foreignObject
             ([x : (Option String) #:-> xml-attribute-value->string #false]
              [y : (Option String) #:-> xml-attribute-value->string #false]
              [width : (Option String) #:-> xml-attribute-value->string #false]
              [height : (Option String) #:-> xml-attribute-value->string #false]))

           (define-svg-element text
             ([x : (Option String) #:-> xml-attribute-value->string #false]
              [y : (Option String) #:-> xml-attribute-value->string #false]
              [dx : (Option String) #:-> xml-attribute-value->string #false]
              [dy : (Option String) #:-> xml-attribute-value->string #false]
              [lengthAdjust : (Option String) #:-> xml-attribute-value->string #false]
              [rotate : (Option String) #:-> xml-attribute-value->string #false]
              [textLength : (Option String) #:-> xml-attribute-value->string #false]))
           
           #:subdom
           [(define-svg-subdom svg-shape-element : SVG-Shape-Element ()
              (define-svg-element path
                ([d : (Option String) #:-> xml-attribute-value->string #false]
                 [pathLength : (Option String) #:-> xml-attribute-value->string #false]))

              (define-svg-element line
                ([x1 : (Option String) #:-> xml-attribute-value->string #false]
                 [x2 : (Option String) #:-> xml-attribute-value->string #false]
                 [y1 : (Option String) #:-> xml-attribute-value->string #false]
                 [y2 : (Option String) #:-> xml-attribute-value->string #false]))
              
              (define-svg-element rect
                ([x : (Option String) #:-> xml-attribute-value->string #false]
                 [y : (Option String) #:-> xml-attribute-value->string #false]
                 [width : (Option String) #:-> xml-attribute-value->string #false]
                 [height : (Option String) #:-> xml-attribute-value->string #false]
                 [rx : (Option String) #:-> xml-attribute-value->string #false]
                 [ry : (Option String) #:-> xml-attribute-value->string #false]))
              
              (define-svg-element circle
                ([cx : (Option String) #:-> xml-attribute-value->string #false]
                 [cy : (Option String) #:-> xml-attribute-value->string #false]
                 [r : (Option String) #:-> xml-attribute-value->string #false]))
              
              (define-svg-element ellipse
                ([cx : (Option String) #:-> xml-attribute-value->string #false]
                 [cy : (Option String) #:-> xml-attribute-value->string #false]
                 [rx : (Option String) #:-> xml-attribute-value->string #false]
                 [ry : (Option String) #:-> xml-attribute-value->string #false]))

              (define-svg-element polyline
                ([points : (Option String) #:-> xml-attribute-value->string #false]))
              
              (define-svg-element polygon
                ([points : (Option String) #:-> xml-attribute-value->string #false])))

            (define-svg-subdom svg-graphics-referencing-element : SVG-Graphics-Referencing-Element
              #:attribute-categories [xlink]
              ([x : (Option String) #:-> xml-attribute-value->string #false]
               [y : (Option String) #:-> xml-attribute-value->string #false]
               [width : (Option String) #:-> xml-attribute-value->string #false]
               [height : (Option String) #:-> xml-attribute-value->string #false])

              (define-svg-element use ())
              
              (define-svg-element image
                ([preserveAspectRatio : (Option String) #:-> xml-attribute-value->string #false])))])

         (define-svg-subdom svg-text-child-element : SVG-Text-Child-Element ()
           (define-svg-element tspan
             ([x : (Option String) #:-> xml-attribute-value->string #false]
              [y : (Option String) #:-> xml-attribute-value->string #false]
              [dx : (Option String) #:-> xml-attribute-value->string #false]
              [dy : (Option String) #:-> xml-attribute-value->string #false]
              [rotate : (Option String) #:-> xml-attribute-value->string #false]
              [textLength : (Option String) #:-> xml-attribute-value->string #false]
              [lengthAdjust : (Option String) #:-> xml-attribute-value->string #false]))
           
           (define-svg-element tref #:attribute-categories [xlink] ())

           (define-svg-element textPath
             #:attribute-categories [xlink]
             ([method : (Option String) #:-> xml-attribute-value->string #false]
              [spacing : (Option String) #:-> xml-attribute-value->string #false]
              [startOffset : (Option String) #:-> xml-attribute-value->string #false]))

           (define-svg-element altGlyph
             #:attribute-categories [xlink]
             ([x : (Option String) #:-> xml-attribute-value->string #false]
              [y : (Option String) #:-> xml-attribute-value->string #false]
              [dx : (Option String) #:-> xml-attribute-value->string #false]
              [dy : (Option String) #:-> xml-attribute-value->string #false]
              [glyphRef : (Option String) #:-> xml-attribute-value->string #false]
              [format : (Option String) #:-> xml-attribute-value->string #false]
              [rotate : (Option String) #:-> xml-attribute-value->string #false])))])

      (define-svg-subdom svg-filter-primitive-element : SVG-Filter-Primitive-Element
        #:attribute-categories [filter-primitive] ()

        (define-svg-element feFlood ())
        (define-svg-element feMerge ())

        (define-svg-element feImage
          #:attribute-categories [xlink external]
          ([preserveAspectRatio : (Option String) #:-> xml-attribute-value->string #false]))

        (define-svg-element feBlend
          ([in : (Option String) #:-> xml-attribute-value->string #false]
           [in2 : (Option String) #:-> xml-attribute-value->string #false]
           [mode : (Option String) #:-> xml-attribute-value->string #false]))

        (define-svg-element feColorMatrix
          ([in : (Option String) #:-> xml-attribute-value->string #false]
           [type : (Option String) #:-> xml-attribute-value->string #false]
           [values : (Option String) #:-> xml-attribute-value->string #false]))

        (define-svg-element feComponentTransfer
          ([in : (Option String) #:-> xml-attribute-value->string #false]))

        (define-svg-element feComposite
          ([in : (Option String) #:-> xml-attribute-value->string #false]
           [in2 : (Option String) #:-> xml-attribute-value->string #false]
           [k1 : (Option String) #:-> xml-attribute-value->string #false]
           [k2 : (Option String) #:-> xml-attribute-value->string #false]
           [k3 : (Option String) #:-> xml-attribute-value->string #false]
           [k4 : (Option String) #:-> xml-attribute-value->string #false]
           [operator : (Option String) #:-> xml-attribute-value->string #false]))
        
        (define-svg-element feConvolveMatrix
          ([in : (Option String) #:-> xml-attribute-value->string #false]
           [bias : (Option String) #:-> xml-attribute-value->string #false]
           [divisor : (Option String) #:-> xml-attribute-value->string #false]
           [edgeMode : (Option String) #:-> xml-attribute-value->string #false]
           [kernelMatrix : (Option String) #:-> xml-attribute-value->string #false]
           [kernelUnitLength : (Option String) #:-> xml-attribute-value->string #false]
           [order : (Option String) #:-> xml-attribute-value->string #false]
           [preserveAlpha : (Option String) #:-> xml-attribute-value->string #false]
           [targetX : (Option String) #:-> xml-attribute-value->string #false]
           [targetY : (Option String) #:-> xml-attribute-value->string #false]))

        (define-svg-element feDiffuseLighting
          ([in : (Option String) #:-> xml-attribute-value->string #false]
           [diffuseConstant : (Option String) #:-> xml-attribute-value->string #false]
           [kernelUnitLength : (Option String) #:-> xml-attribute-value->string #false]
           [surfaceScale : (Option String) #:-> xml-attribute-value->string #false]))

        (define-svg-element feSpecularLighting
          ([in : (Option String) #:-> xml-attribute-value->string #false]
           [kernelUnitLength : (Option String) #:-> xml-attribute-value->string #false]
           [specularConstant : (Option String) #:-> xml-attribute-value->string #false]
           [specularExponent : (Option String) #:-> xml-attribute-value->string #false]
           [surfaceScale : (Option String) #:-> xml-attribute-value->string #false]))

        (define-svg-element feDisplacementMap
          ([in : (Option String) #:-> xml-attribute-value->string #false]
           [in2 : (Option String) #:-> xml-attribute-value->string #false]
           [scale : (Option String) #:-> xml-attribute-value->string #false]
           [xChannelSelector : (Option String) #:-> xml-attribute-value->string #false]
           [yChannelSelector : (Option String) #:-> xml-attribute-value->string #false]))

        (define-svg-element feGaussianBlur
          ([in : (Option String) #:-> xml-attribute-value->string #false]
           [stdDeviation : (Option String) #:-> xml-attribute-value->string #false]))

        (define-svg-element feMorphology
          ([in : (Option String) #:-> xml-attribute-value->string #false]
           [operator : (Option String) #:-> xml-attribute-value->string #false]
           [radius : (Option String) #:-> xml-attribute-value->string #false]))
        
        (define-svg-element feOffset
          ([in : (Option String) #:-> xml-attribute-value->string #false]
           [dx : (Option String) #:-> xml-attribute-value->string #false]
           [dy : (Option String) #:-> xml-attribute-value->string #false]))

        (define-svg-element feTile
          ([in : (Option String) #:-> xml-attribute-value->string #false]))

        (define-svg-element feTurbulence
          ([baseFrequency : (Option String) #:-> xml-attribute-value->string #false]
           [numOctaves : (Option String) #:-> xml-attribute-value->string #false]
           [seed : (Option String) #:-> xml-attribute-value->string #false]
           [stitchTiles : (Option String) #:-> xml-attribute-value->string #false]
           [type : (Option String) #:-> xml-attribute-value->string #false])))])

   (define-svg-subdom svg-light-source-element : SVG-Light-Source-Element ()
     (define-svg-element feDistantLight
       ([azimuth : (Option String)   #:-> xml-attribute-value->string #false]
        [elevation : (Option String) #:-> xml-attribute-value->string #false]))

     (define-svg-element fePointLight
       ([x : (Option String) #:-> xml-attribute-value->string #false]
        [y : (Option String) #:-> xml-attribute-value->string #false]
        [z : (Option String) #:-> xml-attribute-value->string #false]))
     
     (define-svg-element feSpotLight
       ([x : (Option String) #:-> xml-attribute-value->string #false]
        [y : (Option String) #:-> xml-attribute-value->string #false]
        [z : (Option String) #:-> xml-attribute-value->string #false]
        [pointsAtX : (Option String) #:-> xml-attribute-value->string #false]
        [pointsAtY : (Option String) #:-> xml-attribute-value->string #false]
        [pointsAtZ : (Option String) #:-> xml-attribute-value->string #false]
        [specularExponent : (Option String) #:-> xml-attribute-value->string #false]
        [limitingConeAngle : (Option String) #:-> xml-attribute-value->string #false])))

   (define-svg-subdom svg-animation-element : SVG-Animation-Element
     #:attribute-categories [condition external xlink animation-event animation-timing] ()

     (define-svg-element animate
       #:attribute-categories [animation-target animation-value animation-addition presentation] ())

     (define-svg-element set
       #:attribute-categories [animation-target]
       ([to : (Option String) #:-> xml-attribute-value->string #false]))
     
     (define-svg-element animateColor
       #:attribute-categories [animation-target animation-value animation-addition presentation] ())

     (define-svg-element animateMotion
       #:attribute-categories [animation-value animation-addition]
       ([path : (Option String) #:-> xml-attribute-value->string #false]
        [keyPoints : (Option String) #:-> xml-attribute-value->string #false]
        [rotate : (Option String) #:-> xml-attribute-value->string #false]
        [origin : (Option String) #:-> xml-attribute-value->string #false]))
     
     (define-svg-element animateTransform
       #:attribute-categories [animation-target animation-value animation-addition presentation]
       ([type : (Option String) #:-> xml-attribute-value->string #false])))

   (define-svg-subdom svg-foreign-language-element : SVG-Foreign-Language-Element
     ([type : (Option String) #:-> xml-attribute-value->string #false])

     (define-svg-element script #:attribute-categories [xlink external] ())

     (define-svg-element style
       ([media : (Option String) #:-> xml-attribute-value->string #false]
        [title : (Option String) #:-> xml-attribute-value->string #false])))

   (define-svg-subdom svg-transfer-function-element : SVG-Transfer-Function-Element
     ([amplitude : (Option String) #:-> xml-attribute-value->string #false]
      [exponent : (Option String) #:-> xml-attribute-value->string #false]
      [intercept : (Option String) #:-> xml-attribute-value->string #false]
      [offset : (Option String) #:-> xml-attribute-value->string #false]
      [slope : (Option String) #:-> xml-attribute-value->string #false]
      [tableValues : (Option String) #:-> xml-attribute-value->string #false]
      [type : (Option String) #:-> xml-attribute-value->string #false])

     (define-svg-element feFuncR ())
     (define-svg-element feFuncG ())
     (define-svg-element feFuncB ())
     (define-svg-element feFuncA ()))

   (define-svg-subdom svg-font-description-element : SVG-Font-Description-Element ()
     (define-svg-element font
       #:attribute-categories [presentation style external]
       ([horiz-adv-x : (Option String) #:-> xml-attribute-value->string #false]
        [horiz-origin-x : (Option String) #:-> xml-attribute-value->string #false]
        [horiz-origin-y : (Option String) #:-> xml-attribute-value->string #false]
        [vert-adv-y : (Option String) #:-> xml-attribute-value->string #false]
        [vert-origin-x : (Option String) #:-> xml-attribute-value->string #false]
        [vert-origin-y : (Option String) #:-> xml-attribute-value->string #false]))
     
     (define-svg-element font-face-src ())
     (define-svg-element font-face-uri #:attribute-categories [xlink] ())

     (define-svg-element font-face-name
       ([name : (Option String) #:-> xml-attribute-value->string #false]))
     
     (define-svg-element font-face-format
       ([string : (Option String) #:-> xml-attribute-value->string #false]))

     (define-svg-element font-face
       ([accent-height : (Option String) #:-> xml-attribute-value->string #false]
        [alphabetic : (Option String) #:-> xml-attribute-value->string #false]
        [ascent : (Option String) #:-> xml-attribute-value->string #false]
        [bbox : (Option String) #:-> xml-attribute-value->string #false]
        [cap-height : (Option String) #:-> xml-attribute-value->string #false]
        [descent : (Option String) #:-> xml-attribute-value->string #false]
        [font-family : (Option String) #:-> xml-attribute-value->string #false]
        [font-size : (Option String) #:-> xml-attribute-value->string #false]
        [font-stretch : (Option String) #:-> xml-attribute-value->string #false]
        [font-style : (Option String) #:-> xml-attribute-value->string #false]
        [font-variant : (Option String) #:-> xml-attribute-value->string #false]
        [font-weight : (Option String) #:-> xml-attribute-value->string #false]
        [hanging : (Option String) #:-> xml-attribute-value->string #false]
        [ideographic : (Option String) #:-> xml-attribute-value->string #false]
        [mathematical : (Option String) #:-> xml-attribute-value->string #false]
        [overline-position : (Option String) #:-> xml-attribute-value->string #false]
        [overline-thickness : (Option String) #:-> xml-attribute-value->string #false]
        [panose-1 : (Option String) #:-> xml-attribute-value->string #false]
        [slope : (Option String) #:-> xml-attribute-value->string #false]
        [stemh : (Option String) #:-> xml-attribute-value->string #false]
        [stemv : (Option String) #:-> xml-attribute-value->string #false]
        [strikethrough-position : (Option String) #:-> xml-attribute-value->string #false]
        [strikethrough-thickness : (Option String) #:-> xml-attribute-value->string #false]
        [underline-position : (Option String) #:-> xml-attribute-value->string #false]
        [underline-thickness : (Option String) #:-> xml-attribute-value->string #false]
        [unicode-range : (Option String) #:-> xml-attribute-value->string #false]
        [units-per-em : (Option String) #:-> xml-attribute-value->string #false]
        [v-alphabetic : (Option String) #:-> xml-attribute-value->string #false]
        [v-hanging : (Option String) #:-> xml-attribute-value->string #false]
        [v-ideographic : (Option String) #:-> xml-attribute-value->string #false]
        [v-mathematical : (Option String) #:-> xml-attribute-value->string #false]
        [widths : (Option String) #:-> xml-attribute-value->string #false]
        [x-height : (Option String) #:-> xml-attribute-value->string #false])))
   
   (define-svg-subdom svg-glyph-element : SVG-Glyph-Element ()
     (define-svg-element altGlyphDef ())
     (define-svg-element altGlyphItem ())

     (define-svg-element glyphRef
       #:attribute-categories [presentation style xlink]
       ([x : (Option String) #:-> xml-attribute-value->string #false]
        [y : (Option String) #:-> xml-attribute-value->string #false]
        [dx : (Option String) #:-> xml-attribute-value->string #false]
        [dy : (Option String) #:-> xml-attribute-value->string #false]
        [glyphRef : (Option String) #:-> xml-attribute-value->string #false]
        [format : (Option String) #:-> xml-attribute-value->string #false]
        [rotate : (Option String) #:-> xml-attribute-value->string #false]))

     #:subdom
     [(define-svg-subdom svg-kerning-pair-element : SVG-Kerning-Pair-Element
        ([u1 : (Option String) #:-> xml-attribute-value->string #false]
         [g1 : (Option String) #:-> xml-attribute-value->string #false]
         [u2 : (Option String) #:-> xml-attribute-value->string #false]
         [g2 : (Option String) #:-> xml-attribute-value->string #false]
         [k : (Option String) #:-> xml-attribute-value->string #false])

        (define-svg-element vkern ())
        (define-svg-element hkern ()))])]

  (define-svg-element mpath #:attribute-categories [xlink external] ())

  (define-svg-element color-profile
    #:attribute-categories [xlink]
    ([local : (Option String) #:-> xml-attribute-value->string #false]
     [name : (Option String) #:-> xml-attribute-value->string #false]
     [rendering-intent : (Option String) #:-> xml-attribute-value->string #false]))
  
  (define-svg-element feMergeNode
    ([in : (Option String) #:-> xml-attribute-value->string #false]))

  (define-svg-element cursor
    #:attribute-categories [condition xlink external]
    ([x : (Option String) #:-> xml-attribute-value->string #false]
     [y : (Option String) #:-> xml-attribute-value->string #false]))
  
  (define-svg-element view
    #:attribute-categories [external]
    ([preserveAspectRatio : (Option String) #:-> xml-attribute-value->string #false]
     [viewBox : (Option String) #:-> xml-attribute-value->string #false]
     [viewTarget : (Option String) #:-> xml-attribute-value->string #false]
     [zoomAndPan : (Option String) #:-> xml-attribute-value->string #false])))

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

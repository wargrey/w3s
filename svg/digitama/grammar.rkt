#lang typed/racket/base

(provide (all-defined-out))
(provide (rename-out [XML-Source SVG-Source]))

(require digimon/syntax)

(require sgml/digitama/dialect)
(require sgml/digitama/digicore)
(require sgml/digitama/datatype)
(require sgml/digitama/convert)

(require sgml/digitama/xexpr/grammar)
(require sgml/digitama/grammar)

(require "grammar/attribute.rkt")

(require (for-syntax syntax/parse))
(require (for-syntax digimon/symbol))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-for-syntax (racket->svg-name <e>)
  (syntax-parse <e> #:literals [:]
    [(id tag : ID) (list (make-identifier #'id "svg:~a") #'tag (make-identifier #'ID "SVG:~a") (make-identifier #'id "svg:~a-flatten-attributes"))]
    [(id tag) (list (make-identifier #'id "svg:~a") #'tag (make-identifier #'id "SVG:~a" symbol-titlecase) (make-identifier #'id "svg:~a-flatten-attributes"))]
    [(tag : ID) (list (make-identifier #'tag "svg:~a") #'tag (make-identifier #'ID "SVG:~a") (make-identifier #'tag "svg:~a-flatten-attributes"))]
    [tag:id (list (make-identifier #'tag "svg:~a") #'tag (make-identifier #'tag "SVG:~a" symbol-titlecase) (make-identifier #'tag "svg:~a-flatten-attributes"))]
    [_ (raise-syntax-error 'define-svg-element "invalid element name" <e>)]))

(define-for-syntax (racket->attr-names <a>)
  (define attrib (symbol->immutable-string (syntax-e <a>)))
  (cons (format-id <a> "attr:~a" attrib)
        (cdr (racket->svg:attr-names <a>))))

(define-syntax (define-svg-element stx)
  (syntax-parse stx #:literals [:]
    [(_ svg-elem : SVG-Elem
        #:element-interface elem-defs #:head head-defs #:body body-defs
        (~optional (~seq #:attribute-categories [attrib ...]) #:defaults ([(attrib 1) null]))
        defs-rest ...)
     (with-syntax* ([([svg:attr SVG:Attr extract-svg:attr avg:attr->xexpr] ...) (map racket->attr-names (syntax->list #'(attrib ...)))])
       (syntax/loc stx
         (define-dom-element svg-elem : SVG-Elem
           #:element-interface elem-defs #:head head-defs #:body body-defs
           #:attribute-categories ([svg:attr : SVG:Attr extract-svg:attr avg:attr->xexpr] ...)
           defs-rest ...
           #:property prop:convertible
           (λ [[self : SVG-Elem] [mime : Symbol] [fallback : Any]] : Any
             (case mime
               [(svg-bytes) (xml-element->bytes self svg-element-flatten-attributes)]
               [else fallback])))))]))

(define-syntax (define-svg-subdom stx)
  (syntax-parse stx #:literals [:]
    [(_ subsvg : SubSVG
        #:subdom-interface subdom-defs #:head head-defs #:body body-defs
        (~optional (~seq #:attribute-categories [attrib ...]) #:defaults ([(attrib 1) null]))
        subfield-defs
        (defsvg svg-elem+name defs-rest ...) ...
        (~optional (~seq #:subdom [sub-defs ...]) #:defaults ([(sub-defs 1) null])))
     (with-syntax* ([([svg:attr SVG:Attr extract-svg:attr svg:attr->xexpr] ...) (map racket->attr-names (syntax->list #'(attrib ...)))]
                    [([svg-elem svg-tagname SVG-Elem svg-flatten-attributes] ...) (map racket->svg-name (syntax->list #'(svg-elem+name ...)))])
       (syntax/loc stx
         (define-xml-subdom subsvg : SubSVG
           #:subdom-interface subdom-defs #:head head-defs #:body body-defs
           #:attribute-categories ([svg:attr : SVG:Attr extract-svg:attr svg:attr->xexpr] ...)
           subfield-defs
           
           #:subdom [sub-defs ...]
           (defsvg [svg-elem svg-tagname] : SVG-Elem #:with svg-flatten-attributes defs-rest ...) ...)))]))

(define-syntax (define-svg-dom stx)
  (syntax-parse stx #:literals [:]
    [(_ svg : SVG #:dom-interface [refine flatten-attributes]
        head-defs body-defs
        (~optional (~seq #:subdom [sub-defs ...]) #:defaults ([(sub-defs 1) null]))
        (defsvg svg-elem+name defs-rest ...) ...)
     (with-syntax* ([([svg-elem svg-tagname SVG-Elem svg-flatten-attributes] ...) (map racket->svg-name (syntax->list #'(svg-elem+name ...)))]
                    [(unknown Unknown refine-unknown)
                     (for/list ([id (in-list '(svg-unknown SVG-Unknown svg-refine-unknown))])
                       (datum->syntax #'svg id))])
       (syntax/loc stx
         (define-xml-dom svg : SVG
           #:dom-interface [(svg-attributes*-extract-core svg-core->xml-attributes)
                            refine flatten-attributes
                            svg-report-unrecognized-element/attributes make+exn:svg:range
                            xml-contents*->svg-elements]
           head-defs body-defs
           #:unknown [unknown : Unknown #:refine refine-unknown]

           #:subdom [sub-defs ...]
           (defsvg [svg-elem svg-tagname] : SVG-Elem #:with svg-flatten-attributes defs-rest ...) ...)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-svg-dom svg-element : SVG-Element
  #:dom-interface [xml-element*->svg-element svg-element-flatten-attributes]
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
       ([x : (Option String) #:= #false #:<-> xml:attr-value*->string]
        [y : (Option String) #:= #false #:<-> xml:attr-value*->string]
        [width : (Option String) #:= #false #:<-> xml:attr-value*->string]
        [height : (Option String) #:= #false #:<-> xml:attr-value*->string]
        [filterRes : (Option String) #:= #false #:<-> xml:attr-value*->string]
        [filterUnits : (Option String) #:= #false #:<-> xml:attr-value*->string]
        [primitiveUnits : (Option String) #:= #false #:<-> xml:attr-value*->string]))

     (define-svg-element stop ; for gradient elements
       ([offset : (Option String) #:= #false #:<-> xml:attr-value*->string]))

     #:subdom
     [(define-svg-subdom svg-gradient-element : SVG-Gradient-Element
        #:attribute-categories [xlink external]
        ([gradientTransform : (Option String) #:= #false #:<-> xml:attr-value*->string]
         [gradientUnits : (Option String) #:= #false #:<-> xml:attr-value*->string]
         [spreadMethod : (Option String) #:= #false #:<-> xml:attr-value*->string])

        (define-svg-element linearGradient
          ([x1 : (Option String) #:= #false #:<-> xml:attr-value*->string]
           [x2 : (Option String) #:= #false #:<-> xml:attr-value*->string]
           [y1 : (Option String) #:= #false #:<-> xml:attr-value*->string]
           [y2 : (Option String) #:= #false #:<-> xml:attr-value*->string]))

        (define-svg-element radialGradient
          ([cx : (Option String) #:= #false #:<-> xml:attr-value*->string]
           [cy : (Option String) #:= #false #:<-> xml:attr-value*->string]
           [fx : (Option String) #:= #false #:<-> xml:attr-value*->string]
           [fy : (Option String) #:= #false #:<-> xml:attr-value*->string]
           [r : (Option String) #:= #false #:<-> xml:attr-value*->string])))

      (define-svg-subdom svg-container-element : SVG-Container-Element ()
        (define-svg-element pattern
          #:attribute-categories [condition xlink external]
          ([x : (Option String) #:= #false #:<-> xml:attr-value*->string]
           [y : (Option String) #:= #false #:<-> xml:attr-value*->string]
           [width : (Option String) #:= #false #:<-> xml:attr-value*->string]
           [height : (Option String) #:= #false #:<-> xml:attr-value*->string]
           [viewBox : (Option String) #:= #false #:<-> xml:attr-value*->string]
           [patternContentUnits : (Option String) #:= #false #:<-> xml:attr-value*->string]
           [patternTransform : (Option String) #:= #false #:<-> xml:attr-value*->string]
           [patternUnits : (Option String) #:= #false #:<-> xml:attr-value*->string]
           [preserveAspectRatio : (Option String) #:= #false #:<-> xml:attr-value*->string]))
        
        (define-svg-element marker
          #:attribute-categories [external]
          ([markerHeight : (Option String) #:= #false #:<-> xml:attr-value*->string]
           [markerUnits : (Option String) #:= #false #:<-> xml:attr-value*->string]
           [markerWidth : (Option String) #:= #false #:<-> xml:attr-value*->string]
           [orient : (Option String) #:= #false #:<-> xml:attr-value*->string]
           [preserveAspectRatio : (Option String) #:= #false #:<-> xml:attr-value*->string]
           [refX : (Option String) #:= #false #:<-> xml:attr-value*->string]
           [refY : (Option String) #:= #false #:<-> xml:attr-value*->string]
           [viewBox : (Option String) #:= #false #:<-> xml:attr-value*->string]))
        
        (define-svg-element mask
          #:attribute-categories [condition external]
          ([x : (Option String) #:= #false #:<-> xml:attr-value*->string]
           [y : (Option String) #:= #false #:<-> xml:attr-value*->string]
           [width : (Option String) #:= #false #:<-> xml:attr-value*->string]
           [height : (Option String) #:= #false #:<-> xml:attr-value*->string]
           [maskContentUnits : (Option String) #:= #false #:<-> xml:attr-value*->string]
           [maskUnits : (Option String) #:= #false #:<-> xml:attr-value*->string]))

        (define-svg-element clipPath
          #:attribute-categories [condition external]
          ([clipPathUnits : (Option String) #:= #false #:<-> xml:attr-value*->string]
           [transform : (Option String) #:= #false #:<-> xml:attr-value*->string]))
        
        (define-svg-element glyph
          ([d : (Option String) #:= #false #:<-> xml:attr-value*->string]
           [arabic-form : (Option String) #:= #false #:<-> xml:attr-value*->string]
           [glyph-name : (Option String) #:= #false #:<-> xml:attr-value*->string]
           [horiz-adv-x : (Option String) #:= #false #:<-> xml:attr-value*->string]
           [lang : (Option String) #:= #false #:<-> xml:attr-value*->string]
           [orientation : (Option String) #:= #false #:<-> xml:attr-value*->string]
           [unicode : (Option String) #:= #false #:<-> xml:attr-value*->string]
           [vert-adv-y : (Option String) #:= #false #:<-> xml:attr-value*->string]
           [vert-origin-x : (Option String) #:= #false #:<-> xml:attr-value*->string]
           [vert-origin-y : (Option String) #:= #false #:<-> xml:attr-value*->string]))
        
        (define-svg-element missing-glyph
          ([d : (Option String) #:= #false #:<-> xml:attr-value*->string]
           [horiz-adv-x : (Option String) #:= #false #:<-> xml:attr-value*->string]
           [vert-adv-y : (Option String) #:= #false #:<-> xml:attr-value*->string]
           [vert-origin-x : (Option String) #:= #false #:<-> xml:attr-value*->string]
           [vert-origin-y : (Option String) #:= #false #:<-> xml:attr-value*->string]))

        #:subdom
        [(define-svg-subdom svg-structural-element : SVG-Structural-Element
           #:attribute-categories [graphical-event external] ()

           (define-svg-element [svg : SVG]
             #:attribute-categories [condition document-event]
             ([x : (Option String) #:= #false #:<-> xml:attr-value*->string]
              [y : (Option String) #:= #false #:<-> xml:attr-value*->string]
              [width : (Option XML-Nonnegative-Dimension) #:= #false #:<-> xml:attr-value*+>dimension xml:attr-dimension->value]
              [height : (Option XML-Nonnegative-Dimension) #:= #false #:<-> xml:attr-value*+>dimension xml:attr-dimension->value]
              [viewBox : (Option String) #:= #false #:<-> xml:attr-value*->string]
              [baseProfile : (Option String) #:= #false #:<-> xml:attr-value*->string]
              [contentScriptType : (Option String) #:= #false #:<-> xml:attr-value*->string]
              [contentStyleType : (Option String) #:= #false #:<-> xml:attr-value*->string]
              [preserveAspectRatio : (Option String) #:= #false #:<-> xml:attr-value*->string]
              [version : (Option String) #:= #false #:<-> xml:attr-value*->string]
              [zoomAndPan : (Option String) #:= #false #:<-> xml:attr-value*->string]))

           (define-svg-element symbol
             ([preserveAspectRatio : (Option String) #:= #false #:<-> xml:attr-value*->string]
              [viewBox : (Option String) #:= #false #:<-> xml:attr-value*->string]))

           #:subdom
           [(define-svg-subdom svg-transformable-element : SVG-Transformable-Element
              #:attribute-categories [condition]
              ([transform : (Option String) #:= #false #:<-> xml:attr-value*->string])

              (define-svg-element g ())
              (define-svg-element defs ())
              (define-svg-element switch ())

              (define-svg-element a
                #:attribute-categories [xlink]
                ([target : (Option String) #:= #false #:<-> xml:attr-value*->string])))])])
      
      (define-svg-subdom svg-visual-element : SVG-Visual-Element
        #:attribute-categories [graphical-event external] ()

        #:subdom
        [(define-svg-subdom svg-graphics-element : SVG-Graphics-Element
           #:attribute-categories [condition]
           ([transform : (Option String) #:= #false #:<-> xml:attr-value*->string])

           (define-svg-element foreignObject
             ([x : (Option String) #:= #false #:<-> xml:attr-value*->string]
              [y : (Option String) #:= #false #:<-> xml:attr-value*->string]
              [width : (Option String) #:= #false #:<-> xml:attr-value*->string]
              [height : (Option String) #:= #false #:<-> xml:attr-value*->string]))

           (define-svg-element text
             ([x : (Option String) #:= #false #:<-> xml:attr-value*->string]
              [y : (Option String) #:= #false #:<-> xml:attr-value*->string]
              [dx : (Option String) #:= #false #:<-> xml:attr-value*->string]
              [dy : (Option String) #:= #false #:<-> xml:attr-value*->string]
              [lengthAdjust : (Option String) #:= #false #:<-> xml:attr-value*->string]
              [rotate : (Option String) #:= #false #:<-> xml:attr-value*->string]
              [textLength : (Option String) #:= #false #:<-> xml:attr-value*->string]))
           
           #:subdom
           [(define-svg-subdom svg-shape-element : SVG-Shape-Element ()
              (define-svg-element path
                ([d : (Option String) #:= #false #:<-> xml:attr-value*->string]
                 [pathLength : (Option String) #:= #false #:<-> xml:attr-value*->string]))

              (define-svg-element line
                ([x1 : (Option String) #:= #false #:<-> xml:attr-value*->string]
                 [x2 : (Option String) #:= #false #:<-> xml:attr-value*->string]
                 [y1 : (Option String) #:= #false #:<-> xml:attr-value*->string]
                 [y2 : (Option String) #:= #false #:<-> xml:attr-value*->string]))
              
              (define-svg-element rect
                ([x : (Option String) #:= #false #:<-> xml:attr-value*->string]
                 [y : (Option String) #:= #false #:<-> xml:attr-value*->string]
                 [width : (Option String) #:= #false #:<-> xml:attr-value*->string]
                 [height : (Option String) #:= #false #:<-> xml:attr-value*->string]
                 [rx : (Option String) #:= #false #:<-> xml:attr-value*->string]
                 [ry : (Option String) #:= #false #:<-> xml:attr-value*->string]))
              
              (define-svg-element circle
                ([cx : (Option String) #:= #false #:<-> xml:attr-value*->string]
                 [cy : (Option String) #:= #false #:<-> xml:attr-value*->string]
                 [r : (Option String) #:= #false #:<-> xml:attr-value*->string]))
              
              (define-svg-element ellipse
                ([cx : (Option String) #:= #false #:<-> xml:attr-value*->string]
                 [cy : (Option String) #:= #false #:<-> xml:attr-value*->string]
                 [rx : (Option String) #:= #false #:<-> xml:attr-value*->string]
                 [ry : (Option String) #:= #false #:<-> xml:attr-value*->string]))

              (define-svg-element polyline
                ([points : (Option String) #:= #false #:<-> xml:attr-value*->string]))
              
              (define-svg-element polygon
                ([points : (Option String) #:= #false #:<-> xml:attr-value*->string])))

            (define-svg-subdom svg-graphics-referencing-element : SVG-Graphics-Referencing-Element
              #:attribute-categories [xlink]
              ([x : (Option String) #:= #false #:<-> xml:attr-value*->string]
               [y : (Option String) #:= #false #:<-> xml:attr-value*->string]
               [width : (Option String) #:= #false #:<-> xml:attr-value*->string]
               [height : (Option String) #:= #false #:<-> xml:attr-value*->string])

              (define-svg-element use ())
              
              (define-svg-element image
                ([preserveAspectRatio : (Option String) #:= #false #:<-> xml:attr-value*->string])))])

         (define-svg-subdom svg-text-child-element : SVG-Text-Child-Element ()
           (define-svg-element tspan
             ([x : (Option String) #:= #false #:<-> xml:attr-value*->string]
              [y : (Option String) #:= #false #:<-> xml:attr-value*->string]
              [dx : (Option String) #:= #false #:<-> xml:attr-value*->string]
              [dy : (Option String) #:= #false #:<-> xml:attr-value*->string]
              [rotate : (Option String) #:= #false #:<-> xml:attr-value*->string]
              [textLength : (Option String) #:= #false #:<-> xml:attr-value*->string]
              [lengthAdjust : (Option String) #:= #false #:<-> xml:attr-value*->string]))
           
           (define-svg-element tref #:attribute-categories [xlink] ())

           (define-svg-element textPath
             #:attribute-categories [xlink]
             ([method : (Option String) #:= #false #:<-> xml:attr-value*->string]
              [spacing : (Option String) #:= #false #:<-> xml:attr-value*->string]
              [startOffset : (Option String) #:= #false #:<-> xml:attr-value*->string]))

           (define-svg-element altGlyph
             #:attribute-categories [xlink]
             ([x : (Option String) #:= #false #:<-> xml:attr-value*->string]
              [y : (Option String) #:= #false #:<-> xml:attr-value*->string]
              [dx : (Option String) #:= #false #:<-> xml:attr-value*->string]
              [dy : (Option String) #:= #false #:<-> xml:attr-value*->string]
              [glyphRef : (Option String) #:= #false #:<-> xml:attr-value*->string]
              [format : (Option String) #:= #false #:<-> xml:attr-value*->string]
              [rotate : (Option String) #:= #false #:<-> xml:attr-value*->string])))])

      (define-svg-subdom svg-filter-primitive-element : SVG-Filter-Primitive-Element
        #:attribute-categories [filter-primitive] ()

        (define-svg-element feFlood ())
        (define-svg-element feMerge ())

        (define-svg-element feImage
          #:attribute-categories [xlink external]
          ([preserveAspectRatio : (Option String) #:= #false #:<-> xml:attr-value*->string]))

        (define-svg-element feBlend
          ([in : (Option String) #:= #false #:<-> xml:attr-value*->string]
           [in2 : (Option String) #:= #false #:<-> xml:attr-value*->string]
           [mode : (Option String) #:= #false #:<-> xml:attr-value*->string]))

        (define-svg-element feColorMatrix
          ([in : (Option String) #:= #false #:<-> xml:attr-value*->string]
           [type : (Option String) #:= #false #:<-> xml:attr-value*->string]
           [values : (Option String) #:= #false #:<-> xml:attr-value*->string]))

        (define-svg-element feComponentTransfer
          ([in : (Option String) #:= #false #:<-> xml:attr-value*->string]))

        (define-svg-element feComposite
          ([in : (Option String) #:= #false #:<-> xml:attr-value*->string]
           [in2 : (Option String) #:= #false #:<-> xml:attr-value*->string]
           [k1 : (Option String) #:= #false #:<-> xml:attr-value*->string]
           [k2 : (Option String) #:= #false #:<-> xml:attr-value*->string]
           [k3 : (Option String) #:= #false #:<-> xml:attr-value*->string]
           [k4 : (Option String) #:= #false #:<-> xml:attr-value*->string]
           [operator : (Option String) #:= #false #:<-> xml:attr-value*->string]))
        
        (define-svg-element feConvolveMatrix
          ([in : (Option String) #:= #false #:<-> xml:attr-value*->string]
           [bias : (Option String) #:= #false #:<-> xml:attr-value*->string]
           [divisor : (Option String) #:= #false #:<-> xml:attr-value*->string]
           [edgeMode : (Option String) #:= #false #:<-> xml:attr-value*->string]
           [kernelMatrix : (Option String) #:= #false #:<-> xml:attr-value*->string]
           [kernelUnitLength : (Option String) #:= #false #:<-> xml:attr-value*->string]
           [order : (Option String) #:= #false #:<-> xml:attr-value*->string]
           [preserveAlpha : (Option String) #:= #false #:<-> xml:attr-value*->string]
           [targetX : (Option String) #:= #false #:<-> xml:attr-value*->string]
           [targetY : (Option String) #:= #false #:<-> xml:attr-value*->string]))

        (define-svg-element feDiffuseLighting
          ([in : (Option String) #:= #false #:<-> xml:attr-value*->string]
           [diffuseConstant : (Option String) #:= #false #:<-> xml:attr-value*->string]
           [kernelUnitLength : (Option String) #:= #false #:<-> xml:attr-value*->string]
           [surfaceScale : (Option String) #:= #false #:<-> xml:attr-value*->string]))

        (define-svg-element feSpecularLighting
          ([in : (Option String) #:= #false #:<-> xml:attr-value*->string]
           [kernelUnitLength : (Option String) #:= #false #:<-> xml:attr-value*->string]
           [specularConstant : (Option String) #:= #false #:<-> xml:attr-value*->string]
           [specularExponent : (Option String) #:= #false #:<-> xml:attr-value*->string]
           [surfaceScale : (Option String) #:= #false #:<-> xml:attr-value*->string]))

        (define-svg-element feDisplacementMap
          ([in : (Option String) #:= #false #:<-> xml:attr-value*->string]
           [in2 : (Option String) #:= #false #:<-> xml:attr-value*->string]
           [scale : (Option String) #:= #false #:<-> xml:attr-value*->string]
           [xChannelSelector : (Option String) #:= #false #:<-> xml:attr-value*->string]
           [yChannelSelector : (Option String) #:= #false #:<-> xml:attr-value*->string]))

        (define-svg-element feGaussianBlur
          ([in : (Option String) #:= #false #:<-> xml:attr-value*->string]
           [stdDeviation : (Option String) #:= #false #:<-> xml:attr-value*->string]))

        (define-svg-element feMorphology
          ([in : (Option String) #:= #false #:<-> xml:attr-value*->string]
           [operator : (Option String) #:= #false #:<-> xml:attr-value*->string]
           [radius : (Option String) #:= #false #:<-> xml:attr-value*->string]))
        
        (define-svg-element feOffset
          ([in : (Option String) #:= #false #:<-> xml:attr-value*->string]
           [dx : (Option String) #:= #false #:<-> xml:attr-value*->string]
           [dy : (Option String) #:= #false #:<-> xml:attr-value*->string]))

        (define-svg-element feTile
          ([in : (Option String) #:= #false #:<-> xml:attr-value*->string]))

        (define-svg-element feTurbulence
          ([baseFrequency : (Option String) #:= #false #:<-> xml:attr-value*->string]
           [numOctaves : (Option String) #:= #false #:<-> xml:attr-value*->string]
           [seed : (Option String) #:= #false #:<-> xml:attr-value*->string]
           [stitchTiles : (Option String) #:= #false #:<-> xml:attr-value*->string]
           [type : (Option String) #:= #false #:<-> xml:attr-value*->string])))])

   (define-svg-subdom svg-light-source-element : SVG-Light-Source-Element ()
     (define-svg-element feDistantLight
       ([azimuth : (Option String)   #:= #false #:<-> xml:attr-value*->string]
        [elevation : (Option String) #:= #false #:<-> xml:attr-value*->string]))

     (define-svg-element fePointLight
       ([x : (Option String) #:= #false #:<-> xml:attr-value*->string]
        [y : (Option String) #:= #false #:<-> xml:attr-value*->string]
        [z : (Option String) #:= #false #:<-> xml:attr-value*->string]))
     
     (define-svg-element feSpotLight
       ([x : (Option String) #:= #false #:<-> xml:attr-value*->string]
        [y : (Option String) #:= #false #:<-> xml:attr-value*->string]
        [z : (Option String) #:= #false #:<-> xml:attr-value*->string]
        [pointsAtX : (Option String) #:= #false #:<-> xml:attr-value*->string]
        [pointsAtY : (Option String) #:= #false #:<-> xml:attr-value*->string]
        [pointsAtZ : (Option String) #:= #false #:<-> xml:attr-value*->string]
        [specularExponent : (Option String) #:= #false #:<-> xml:attr-value*->string]
        [limitingConeAngle : (Option String) #:= #false #:<-> xml:attr-value*->string])))

   (define-svg-subdom svg-animation-element : SVG-Animation-Element
     #:attribute-categories [condition external xlink animation-event animation-timing] ()

     (define-svg-element animate
       #:attribute-categories [animation-target animation-value animation-addition presentation] ())

     (define-svg-element set
       #:attribute-categories [animation-target]
       ([to : (Option String) #:= #false #:<-> xml:attr-value*->string]))
     
     (define-svg-element animateColor
       #:attribute-categories [animation-target animation-value animation-addition presentation] ())

     (define-svg-element animateMotion
       #:attribute-categories [animation-value animation-addition]
       ([path : (Option String) #:= #false #:<-> xml:attr-value*->string]
        [keyPoints : (Option String) #:= #false #:<-> xml:attr-value*->string]
        [rotate : (Option String) #:= #false #:<-> xml:attr-value*->string]
        [origin : (Option String) #:= #false #:<-> xml:attr-value*->string]))
     
     (define-svg-element animateTransform
       #:attribute-categories [animation-target animation-value animation-addition presentation]
       ([type : (Option String) #:= #false #:<-> xml:attr-value*->string])))

   (define-svg-subdom svg-foreign-language-element : SVG-Foreign-Language-Element
     ([type : (Option String) #:= #false #:<-> xml:attr-value*->string])

     (define-svg-element script #:attribute-categories [xlink external] ())

     (define-svg-element style
       ([media : (Option String) #:= #false #:<-> xml:attr-value*->string]
        [title : (Option String) #:= #false #:<-> xml:attr-value*->string])))

   (define-svg-subdom svg-transfer-function-element : SVG-Transfer-Function-Element
     ([amplitude : (Option String) #:= #false #:<-> xml:attr-value*->string]
      [exponent : (Option String) #:= #false #:<-> xml:attr-value*->string]
      [intercept : (Option String) #:= #false #:<-> xml:attr-value*->string]
      [offset : (Option String) #:= #false #:<-> xml:attr-value*->string]
      [slope : (Option String) #:= #false #:<-> xml:attr-value*->string]
      [tableValues : (Option String) #:= #false #:<-> xml:attr-value*->string]
      [type : (Option String) #:= #false #:<-> xml:attr-value*->string])

     (define-svg-element feFuncR ())
     (define-svg-element feFuncG ())
     (define-svg-element feFuncB ())
     (define-svg-element feFuncA ()))

   (define-svg-subdom svg-font-description-element : SVG-Font-Description-Element ()
     (define-svg-element font
       #:attribute-categories [presentation style external]
       ([horiz-adv-x : (Option String) #:= #false #:<-> xml:attr-value*->string]
        [horiz-origin-x : (Option String) #:= #false #:<-> xml:attr-value*->string]
        [horiz-origin-y : (Option String) #:= #false #:<-> xml:attr-value*->string]
        [vert-adv-y : (Option String) #:= #false #:<-> xml:attr-value*->string]
        [vert-origin-x : (Option String) #:= #false #:<-> xml:attr-value*->string]
        [vert-origin-y : (Option String) #:= #false #:<-> xml:attr-value*->string]))
     
     (define-svg-element font-face-src ())
     (define-svg-element font-face-uri #:attribute-categories [xlink] ())

     (define-svg-element font-face-name
       ([name : (Option String) #:= #false #:<-> xml:attr-value*->string]))
     
     (define-svg-element font-face-format
       ([string : (Option String) #:= #false #:<-> xml:attr-value*->string]))

     (define-svg-element font-face
       ([accent-height : (Option String) #:= #false #:<-> xml:attr-value*->string]
        [alphabetic : (Option String) #:= #false #:<-> xml:attr-value*->string]
        [ascent : (Option String) #:= #false #:<-> xml:attr-value*->string]
        [bbox : (Option String) #:= #false #:<-> xml:attr-value*->string]
        [cap-height : (Option String) #:= #false #:<-> xml:attr-value*->string]
        [descent : (Option String) #:= #false #:<-> xml:attr-value*->string]
        [font-family : (Option String) #:= #false #:<-> xml:attr-value*->string]
        [font-size : (Option String) #:= #false #:<-> xml:attr-value*->string]
        [font-stretch : (Option String) #:= #false #:<-> xml:attr-value*->string]
        [font-style : (Option String) #:= #false #:<-> xml:attr-value*->string]
        [font-variant : (Option String) #:= #false #:<-> xml:attr-value*->string]
        [font-weight : (Option String) #:= #false #:<-> xml:attr-value*->string]
        [hanging : (Option String) #:= #false #:<-> xml:attr-value*->string]
        [ideographic : (Option String) #:= #false #:<-> xml:attr-value*->string]
        [mathematical : (Option String) #:= #false #:<-> xml:attr-value*->string]
        [overline-position : (Option String) #:= #false #:<-> xml:attr-value*->string]
        [overline-thickness : (Option String) #:= #false #:<-> xml:attr-value*->string]
        [panose-1 : (Option String) #:= #false #:<-> xml:attr-value*->string]
        [slope : (Option String) #:= #false #:<-> xml:attr-value*->string]
        [stemh : (Option String) #:= #false #:<-> xml:attr-value*->string]
        [stemv : (Option String) #:= #false #:<-> xml:attr-value*->string]
        [strikethrough-position : (Option String) #:= #false #:<-> xml:attr-value*->string]
        [strikethrough-thickness : (Option String) #:= #false #:<-> xml:attr-value*->string]
        [underline-position : (Option String) #:= #false #:<-> xml:attr-value*->string]
        [underline-thickness : (Option String) #:= #false #:<-> xml:attr-value*->string]
        [unicode-range : (Option String) #:= #false #:<-> xml:attr-value*->string]
        [units-per-em : (Option String) #:= #false #:<-> xml:attr-value*->string]
        [v-alphabetic : (Option String) #:= #false #:<-> xml:attr-value*->string]
        [v-hanging : (Option String) #:= #false #:<-> xml:attr-value*->string]
        [v-ideographic : (Option String) #:= #false #:<-> xml:attr-value*->string]
        [v-mathematical : (Option String) #:= #false #:<-> xml:attr-value*->string]
        [widths : (Option String) #:= #false #:<-> xml:attr-value*->string]
        [x-height : (Option String) #:= #false #:<-> xml:attr-value*->string])))
   
   (define-svg-subdom svg-glyph-element : SVG-Glyph-Element ()
     (define-svg-element altGlyphDef ())
     (define-svg-element altGlyphItem ())

     (define-svg-element glyphRef
       #:attribute-categories [presentation style xlink]
       ([x : (Option String) #:= #false #:<-> xml:attr-value*->string]
        [y : (Option String) #:= #false #:<-> xml:attr-value*->string]
        [dx : (Option String) #:= #false #:<-> xml:attr-value*->string]
        [dy : (Option String) #:= #false #:<-> xml:attr-value*->string]
        [glyphRef : (Option String) #:= #false #:<-> xml:attr-value*->string]
        [format : (Option String) #:= #false #:<-> xml:attr-value*->string]
        [rotate : (Option String) #:= #false #:<-> xml:attr-value*->string]))

     #:subdom
     [(define-svg-subdom svg-kerning-pair-element : SVG-Kerning-Pair-Element
        ([u1 : (Option String) #:= #false #:<-> xml:attr-value*->string]
         [g1 : (Option String) #:= #false #:<-> xml:attr-value*->string]
         [u2 : (Option String) #:= #false #:<-> xml:attr-value*->string]
         [g2 : (Option String) #:= #false #:<-> xml:attr-value*->string]
         [k : (Option String) #:= #false #:<-> xml:attr-value*->string])

        (define-svg-element vkern ())
        (define-svg-element hkern ()))])]

  (define-svg-element mpath #:attribute-categories [xlink external] ())

  (define-svg-element color-profile
    #:attribute-categories [xlink]
    ([local : (Option String) #:= #false #:<-> xml:attr-value*->string]
     [name : (Option String) #:= #false #:<-> xml:attr-value*->string]
     [rendering-intent : (Option String) #:= #false #:<-> xml:attr-value*->string]))
  
  (define-svg-element feMergeNode
    ([in : (Option String) #:= #false #:<-> xml:attr-value*->string]))

  (define-svg-element cursor
    #:attribute-categories [condition xlink external]
    ([x : (Option String) #:= #false #:<-> xml:attr-value*->string]
     [y : (Option String) #:= #false #:<-> xml:attr-value*->string]))
  
  (define-svg-element view
    #:attribute-categories [external]
    ([preserveAspectRatio : (Option String) #:= #false #:<-> xml:attr-value*->string]
     [viewBox : (Option String) #:= #false #:<-> xml:attr-value*->string]
     [viewTarget : (Option String) #:= #false #:<-> xml:attr-value*->string]
     [zoomAndPan : (Option String) #:= #false #:<-> xml:attr-value*->string])))

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

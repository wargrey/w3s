#lang typed/racket

(require "../main.rkt")
(require "../sugar.rkt")
(require "../background.rkt")
(require "configure.rkt")
(require "media.css")

(current-css-element-color 'Silver)
(default-stroke (desc-border #:width 1 #:color (current-css-element-color) #:style 'transparent))
(css-configure-@media)

(default-css-feature-support?
  (λ [[desc : Symbol] [value : Any]]
    (implies (equal? (cons 'font-variant-ligatures 'normal)
                     (cons desc value))
             (not (eq? (system-type)
                       'macosx)))))

(define-preference* Box
  ([width : Nonnegative-Flonum             #:= (css-vw)]
   [height : Nonnegative-Flonum            #:= (css-vh)]
   [vertical-margin : Nonnegative-Flonum   #:= 3.0]
   [horizontal-margin : Nonnegative-Flonum #:= 3.0]
   [vertical-inset : Nonnegative-Flonum    #:= 0.0]
   [horizontal-inset : Nonnegative-Flonum  #:= 0.0]
   [icon-height : Nonnegative-Flonum       #:= (default-bitmap-icon-height)]
   [font : Font                            #:= (current-css-element-font)]
   [color : Color                          #:= 'currentcolor]
   [border : Stroke-Paint                  #:= (default-stroke)]
   [brush : Fill-Paint                     #:= 'transparent])
  #:transparent)

(define box-parsers : CSS-Declaration-Parsers
  ;;; NOTE
  ;; This is simple box, it only sets vertical/horizontal margins and paddings.
  ;; but to keep the css file portable, no new properties are introduced.
  (lambda [suitcased-name deprecated!]
    (or (css-font-property-parsers suitcased-name)
        (css-color-property-parsers suitcased-name)
        (css-background-property-parsers suitcased-name)
        (case suitcased-name
          [(icon-height) (<css-line-height>)]))))

(define box-filter : (CSS-Cascaded-Value-Filter Box)
  (lambda [declared-values inherited-values]
    (call-with-css-box declared-values inherited-values 1.4
      #:with [css-box-size css-box-size-ref css-box-color-ref css-box-icon-ref css-box-ref]
      (define-values (width height) (css-box-size (#%Box-width) (#%Box-height)))
      (Box* #:width width #:height height
            #:vertical-inset (css-box-size-ref 'padding-top height)
            #:horizontal-inset (css-box-size-ref 'padding-right width)
            #:vertical-margin (css-box-size-ref 'margin-top height)
            #:horizontal-margin (css-box-size-ref 'margin-right width)
            #:border (css-extract-border declared-values inherited-values 'border-top)
            #:brush (css-extract-background declared-values inherited-values)))))

(define ~root:n : CSS-Subject (css-subject* #::classes '(root)))
(define ~root:s : CSS-Subject (css-subject* #::classes '(selected)))
(define-values ($root:n *root:n) (css-cascade (list media.css) (list ~root:n) box-parsers box-filter #false))
(define-values ($root:s *root:s) (css-cascade (list media.css) (list ~root:s) box-parsers box-filter #false))

media.css
(default-css-media-features)
(bitmap-hb-append #:gapsize (+ (Box-vertical-margin $root:n) (Box-vertical-margin $root:s))
                  (bitmap-frame #:border (desc-border #:color (current-css-element-color) #:width 1 #:style 'dotted)
                                (bitmap-frame #:margin (Box-vertical-margin $root:n) #:padding (Box-vertical-inset $root:n)
                                              #:border (Box-border $root:n) #:fill (Box-brush $root:n)
                                              (bitmap-paragraph #:color (Box-color $root:n) #:max-width (Box-width $root:n)
                                                                (pretty-format $root:n) (Box-font $root:n))))
                  (bitmap-frame #:border (desc-border #:color (current-css-element-color) #:width 1 #:style 'dotted)
                                (bitmap-frame #:margin (Box-vertical-margin $root:s) #:padding (Box-vertical-inset $root:s)
                                              #:border (Box-border $root:s) #:fill (Box-brush $root:s)
                                              (bitmap-paragraph #:color (Box-color $root:s) #:max-width (Box-width $root:s) 
                                                                (pretty-format $root:s) (Box-font $root:s)))))

#lang typed/racket/base

;;; https://www.w3.org/TR/CSS2/box.html#box-model
;;; https://drafts.csswg.org/css-backgrounds/#borders

(provide (all-defined-out))

(require geofun/digitama/paint/stroke)

(require "color.rkt")
(require "syntax/digicore.rkt")
(require "syntax/dimension.rkt")
(require "../recognizer.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-css-disjoint-filter <border-width> #:-> (U Symbol Nonnegative-Inexact-Real CSS:Length:Font)
  ;;; https://drafts.csswg.org/css-backgrounds/#line-width
  (<css+length:font>)
  (CSS:<~> (<css+real>) exact->inexact)
  (<css-keyword> css-border-thickness-options))

(define <:margin:> : CSS-Shorthand+Parser
  (css-make-pair-parser (<css-size>) 'margin-top 'margin-right 'margin-bottom 'margin-left))

(define <:padding:> : CSS-Shorthand+Parser
  (css-make-pair-parser (<css-size>) 'padding-top 'padding-right 'padding-bottom 'padding-left))

(define <:border-color:> : CSS-Shorthand+Parser
  (css-make-pair-parser (<css-color>) 'border-top-color 'border-right-color 'border-bottom-color 'border-left-color))

(define <:border-style:> : CSS-Shorthand+Parser
  (css-make-pair-parser (<css-keyword> css-border-style-options)
                        'border-top-style 'border-right-style 'border-bottom-style 'border-left-style))

(define <:border-width:> : CSS-Shorthand+Parser
  (css-make-pair-parser (<border-width>) 'border-top-width 'border-right-width 'border-bottom-width 'border-left-width))

;;; https://drafts.csswg.org/css-backgrounds/#the-border-shorthands
(define <:border-top:> : CSS-Shorthand+Parser
  (css-make-pair-parser (list (<css-color>)     (<border-width>)  (<css-keyword> css-border-style-options))
                        (list 'border-top-color 'border-top-width 'border-top-style)))

(define <:border-right:> : CSS-Shorthand+Parser
  (css-make-pair-parser (list (<css-color>)       (<border-width>)    (<css-keyword> css-border-style-options))
                        (list 'border-right-color 'border-right-width 'border-right-style)))

(define <:border-bottom:> : CSS-Shorthand+Parser
  (css-make-pair-parser (list (<css-color>)        (<border-width>)     (<css-keyword> css-border-style-options))
                        (list 'border-bottom-color 'border-bottom-width 'border-bottom-style)))

(define <:border-left:> : CSS-Shorthand+Parser
  (css-make-pair-parser (list (<css-color>)      (<border-width>)   (<css-keyword> css-border-style-options))
                        (list 'border-left-color 'border-left-width 'border-left-style)))

(define <:border:> : CSS-Shorthand+Parser
  ;;; TODO: also reset border-image
  (css-make-pair-parser
   (list (cons (<css-color>) '(border-top-color border-right-color border-bottom-color border-left-color))
         (cons (<border-width>) '(border-top-width border-right-width border-bottom-width border-left-width))
         (cons (<css-keyword> css-border-style-options) '(border-top-style border-right-style border-bottom-style border-left-style)))))

(define css->border-width : (CSS->Racket (U Real Symbol False))
  (lambda [_ value]
    (cond [(symbol? value) value]
          [(nonnegative-flonum? value) value]
          [(css+length? value) (css:length->scalar value #false)]
          [else #false])))

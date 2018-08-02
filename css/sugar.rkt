#lang typed/racket/base

(provide (all-defined-out))

(require "digitama/syntax/misc.rkt")
(require "syntax.rkt")
(require "font.rkt")
(require "color.rkt")
(require "image.rkt")

(require (for-syntax racket/base))
(require (for-syntax racket/syntax))
(require (for-syntax syntax/parse))

(define-type Font+Color (Pairof Font Color))
(define-type Font+Colors (Pairof Font (Pairof Color Color)))

(define-syntax (define-preference* stx)
  (syntax-parse stx #:literals [:]
    [(self Preference (fields ...) options ...)
     #'(self Preference #:with [] (fields ...) options ...)]
    [(_ Preference #:with extra-bindings (field-info ...) options ...)
     (with-syntax* ([(property-definitions ...)
                     (for/list ([<field-info> (in-list (syntax->list #'(field-info ...)))])
                       (syntax-parse <field-info> #:datum-literals [Unitless]
                        [(p : Unitless #:= dv) #'[p : Nonnegative-Flonum #:= dv #:~> (U Nonnegative-Flonum Negative-Single-Flonum)
                                                    select-size]]
                         [(p : Unitless #:= dv nv) #'[p : Nonnegative-Flonum #:= dv #:~> (U Nonnegative-Flonum Negative-Single-Flonum)
                                                        (λ [[v : (U Nonnegative-Flonum Negative-Single-Flonum)]]
                                                          (select-size v nv))]]
                         [_ <field-info>]))])
       #'(define-preference Preference #:with extra-bindings (property-definitions ...) options ...))]))

(define-syntax (call-with-css-box stx)
  (syntax-parse stx
    [(_ declared-values inherited-values #:with [box-size size-ref color-ref local-ref] sexp ...)
     (with-syntax ([___ (datum->syntax #'local-ref '...)])
       #'(parameterize ([current-css-element-color (css-rgba-ref declared-values inherited-values)])
           (css-extract-font declared-values inherited-values)
           (define-values (box-size size-ref color-ref icon-ref) (css-property-accessors declared-values inherited-values))
           (define-syntax (local-ref stx) (syntax-case stx [] [(_ argl ___) #'(css-ref declared-values inherited-values argl ___)]))
           sexp ...))]
    [(_ declared-values inherited-values (~optional normal-icon-height) #:with [box-size size-ref color-ref icon-ref local-ref] sexp ...)
     (with-syntax ([___ (datum->syntax #'local-ref '...)]
                   [nih (if (attribute normal-icon-height) #'(real->double-flonum normal-icon-height) #'(css-normal-line-height))])
       #'(parameterize ([current-css-element-color (css-rgba-ref declared-values inherited-values)])
           (css-extract-font declared-values inherited-values)
           (define-values (box-size size-ref color-ref icon-ref) (css-property-accessors declared-values inherited-values))
           (define-syntax (local-ref stx) (syntax-case stx [] [(_ argl ___) #'(css-ref declared-values inherited-values argl ___)]))
           (parameterize ([default-bitmap-icon-height (select-size (local-ref 'icon-height css->line-height) nih)])
             sexp ...)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define css-box-size : (->* (CSS-Values (Option CSS-Values) Nonnegative-Flonum Nonnegative-Flonum)
                            (Nonnegative-Flonum Nonnegative-Flonum)
                            (Values Nonnegative-Flonum Nonnegative-Flonum))
  (lambda [declares inherits initial-width initial-height [width% (css-vw)] [height% (css-vh)]]
    (values (css-ref declares inherits 'width (make-css->size initial-width #:100% width%))
            (css-ref declares inherits 'height (make-css->size initial-height #:100% height%)))))

(define css-size-ref : (case-> [CSS-Values (Option CSS-Values) Symbol Nonnegative-Flonum -> (CSS-Maybe Nonnegative-Flonum)]
                               [CSS-Values (Option CSS-Values) Symbol Nonnegative-Flonum Nonnegative-Flonum -> Nonnegative-Flonum])
  (case-lambda
    [(declares inherits property 100%)
     (css-ref declares inherits property (make-css->size css:initial #:100% 100%))]
    [(declares inherits property 100% defsize)
     (css-ref declares inherits property (make-css->size defsize #:100% 100%))]))

(define css-property-accessors : (-> CSS-Values (Option CSS-Values)
                                     (Values (->* (Nonnegative-Flonum Nonnegative-Flonum) (Nonnegative-Flonum Nonnegative-Flonum)
                                                  (Values Nonnegative-Flonum Nonnegative-Flonum))
                                             (case-> [Symbol Nonnegative-Flonum -> (CSS-Maybe Nonnegative-Flonum)]
                                                     [Symbol Nonnegative-Flonum Nonnegative-Flonum -> Nonnegative-Flonum])
                                             (-> Symbol (CSS-Maybe Color))
                                             (-> Symbol CSS-Make-Icon Nonnegative-Real Color Bitmap)))
  (lambda [declared-values inherited-values]
    (values (lambda [initial-width initial-height [width% (css-vw)] [height% (css-vh)]]
              (css-box-size declared-values inherited-values initial-width initial-height width% height%))
            (case-lambda [(property 100%) (css-size-ref declared-values inherited-values property 100%)]
                         [(property 100% defsize) (css-size-ref declared-values inherited-values property 100% defsize)])
            (lambda [property] (css-rgba-ref declared-values inherited-values property))
            (lambda [property default-icon height color]
              (css-icon-ref declared-values inherited-values property default-icon height color)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define css-font+colors-parsers : CSS-Declaration-Parsers
  (lambda [suitcased-name deprecated!]
    (or (css-font-property-parsers suitcased-name)
        (css-color-property-parsers suitcased-name null))))

(define css-font-filter : (CSS-Cascaded-Value-Filter Font)
  (lambda [declared-values inherited-values]
    (css-extract-font declared-values inherited-values)))

(define css-colors-filter : (CSS-Cascaded-Value-Filter (Pairof Color Color))
  (lambda [declared-values inherited-values]
    (cons (css-rgba-ref declared-values inherited-values)
          (let ([bgcolor (css-rgba-ref declared-values inherited-values 'background-color)])
            (if (css-wide-keyword? bgcolor) (rgb* 'transparent) bgcolor)))))

(define css-font+color-filter : (CSS-Cascaded-Value-Filter Font+Color)
  (lambda [declared-values inherited-values]
    (cons (css-font-filter declared-values inherited-values)
          (css-rgba-ref declared-values inherited-values))))

(define css-font+colors-filter : (CSS-Cascaded-Value-Filter Font+Colors)
  (lambda [declared-values inherited-values]
    (cons (css-font-filter declared-values inherited-values)
          (css-colors-filter declared-values inherited-values))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define css-cascade-colors : (-> (Listof+ CSS-StyleSheet) (Listof+ CSS-Subject) (Option CSS-Values) (Pairof Color Color))
  (lambda [stylesheets stcejbus inherited-values]
    (define-values ($ _) (css-cascade stylesheets stcejbus css-font+colors-parsers css-colors-filter inherited-values))
    $))

(define css-cascade-font+color : (-> (Listof+ CSS-StyleSheet) (Listof+ CSS-Subject) (Option CSS-Values) Font+Color)
  (lambda [stylesheets stcejbus inherited-values]
    (define-values ($ _) (css-cascade stylesheets stcejbus css-font+colors-parsers css-font+color-filter inherited-values))
    $))

(define css-cascade-font+colors : (-> (Listof+ CSS-StyleSheet) (Listof+ CSS-Subject) (Option CSS-Values) Font+Colors)
  (lambda [stylesheets stcejbus inherited-values]
    (define-values ($ _) (css-cascade stylesheets stcejbus css-font+colors-parsers css-font+colors-filter inherited-values))
    $))

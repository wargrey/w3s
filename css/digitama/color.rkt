#lang typed/racket/base

;;; https://drafts.csswg.org/css-color

(provide (all-defined-out))

(require bitmap/color)
(require bitmap/digitama/color)
(require colorspace/misc)

(require racket/keyword)

(require "syntax/digicore.rkt")
(require "syntax/dimension.rkt")
(require "../recognizer.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type CSS-Color-Datum (U Symbol FlColor CSS-Wide-Keyword))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-css-atomic-filter <css#color> #:-> Hexa #:with [[color-value : css:hash?] [no-alpha? : (Option '#:no-alpha) #false]]
  (or (css-hex-color->rgba color-value)
      (make-exn:css:range color-value))
  #:where
  [(define (css-hex-color->rgba [color-value : CSS:Hash]) : (CSS-Option Hexa)
     (define-values (rgb a)
       (cond [(or no-alpha?) (values (css-#hex-color->rgb (css:hash-datum color-value)) 1.0)]
             [else (css-#hex-color->rgba (css:hash-datum color-value))]))
     (cond [(not rgb) #false]
           [(symbol? rgb) (make-exn:css:digit color-value)]
           [else (hexa rgb a)]))])

(define-css-function-filter <css-color-notation> #:-> FlColor
  ;;; https://drafts.csswg.org/css-color/#rgb-functions
  ;;; https://drafts.csswg.org/css-color/#the-hsl-notation
  ;;; https://drafts.csswg.org/css-color/#the-hwb-notation
  [(rgba rgb) #:=> [(rgba [r ? flonum?] [g ? flonum?] [b ? flonum?] [alpha ? flonum?])]
   (make-parser <:rgb:> <:rgb:>)]
  [(hsla hsl) #:=> [(hsl [h ? real?] [s ? flonum?] [l ? flonum?] [alpha ? flonum?])]
   (make-parser <:hue:> (CSS:<^> (<css:percentage>)))]
  [(hsva hsv) #:=> [(hsv [h ? real?] [s ? flonum?] [v ? flonum?] [alpha ? flonum?])]
   (make-parser <:hue:> (CSS:<^> (<css:percentage>)))]
  [(hsia hsi) #:=> [(hsi [h ? real?] [s ? flonum?] [i ? flonum?] [alpha ? flonum?])]
   (make-parser <:hue:> (CSS:<^> (<css:percentage>)))]
  [(hwba hwb) #:=> [(hwb [h ? real?] [w ? flonum?] [b ? flonum?] [alpha ? flonum?])]
   (make-parser <:hue:> (CSS:<^> (<css:percentage>)))]
  #:where
  [(define-css-disjoint-filter <rgb-gamut> #:-> Flonum
     (CSS:<~> (<css:integer> byte?) byte->gamut)
     (CSS:<~> (<css:percentage>) (λ [[% : Flonum]] (fl* % 255.0)))
     (CSS:<~> (<css:flonum>) (λ [[v : Flonum]] (fl/ v 255.0))))

   (define make-alpha-parser : (-> (-> (CSS:Filter Char)) (CSS-Parser (Listof Any)))
     (lambda [<delimiter>]
       (CSS<$> (CSS<?> [(<delimiter>) ((inst CSS:<^> Any) (<css-%flunit>))]) 1.0)))
     
   (define make-parser : (-> (CSS-Parser (Listof Any)) (CSS-Parser (Listof Any)) (CSS-Parser (Listof Any)))
     ;;; https://github.com/w3c/csswg-drafts/issues/266
     (lambda [c1 c2]
       (CSS<&> c1 (CSS<?> [(<css-comma>) (CSS<#> c2 '(2)) (make-alpha-parser <css-comma>)]
                          [else          (CSS<*> c2 '(2)) (make-alpha-parser <css-slash>)]))))

   (define <:rgb:> ((inst CSS:<^> Any) (<rgb-gamut>)))
   (define <:hue:> ((inst CSS:<^> Any) (<css-angle>)))])

(define-css-disjoint-filter <css-color> #:-> CSS-Color-Datum
  ;;; https://drafts.csswg.org/css-color/#color-type
  ;;; https://drafts.csswg.org/css-color/#named-colors
  #:with [[hint? : (Option '#:inherit-currentcolor) #false]]
  (CSS:<~> (<css-keyword> (cons 'currentcolor (cons 'transparent (hash-keys css-named-colors))))
           (λ [[c : Symbol]] (cond [(not (eq? c 'currentcolor)) c]
                                   [(not hint?) c]
                                   [else css:inherit])))
  (<css#color>)
  (<css-color-notation>))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define css-color-datum? : (-> Any Boolean : CSS-Color-Datum)
  (lambda [v]
    (or (symbol? v)
        (flcolor? v)
        (css-wide-keyword? v))))

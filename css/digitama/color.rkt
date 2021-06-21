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
(define-css-atomic-filter <css#color> #:-> Hexa #:with [[color-value : css:hash?] [no-alpha? : (Option '#:no-alpha) #false]]
  (or (css-hex-color->rgba color-value)
      (make-exn:css:range color-value))
  #:where
  [(define (short-color->number [color : String]) : (Option Nonnegative-Fixnum)
     (for/fold ([hexcolor : (Option Nonnegative-Fixnum) 0])
               ([ch : Char (in-string color)])
       (define digit : (U Integer Void)
         (cond [(char-numeric? ch)   (fx- (char->integer ch) #x30)]
               [(char<=? #\a ch #\f) (fx- (char->integer ch) #x37)]
               [(char<=? #\A ch #\F) (fx- (char->integer ch) #x57)]))
       (and hexcolor (byte? digit)
            (fxior (fxlshift hexcolor 8)
                   (fxior (fxlshift digit 4)
                          digit)))))
   
   (define (css-hex-color->rgba [color-value : CSS:Hash]) : (CSS-Option Hexa)
     ;;; https://drafts.csswg.org/css-color/#numeric-rgb
     (define color : String (keyword->immutable-string (css:hash-datum color-value)))
     (define digits : Index (string-length color))
     (define ?hexcolor : (CSS-Option Number)
       (if (not no-alpha?)
           (case digits
             [(6 8) (string->number color 16)]
             [(3 4) (short-color->number color)]
             [else (make-exn:css:digit color-value)])
           (case digits
             [(6) (string->number color 16)]
             [(3) (short-color->number color)]
             [else (make-exn:css:digit color-value)])))
     (cond [(exn:css? ?hexcolor) ?hexcolor]
           [(or no-alpha? (fx= digits 3) (fx= digits 6)) (and (index? ?hexcolor) (hexa ?hexcolor 1.0))]
           [else (and (exact-integer? ?hexcolor)
                      (let ([hex-rgb (arithmetic-shift ?hexcolor -8)])
                        (and (index? hex-rgb)
                             (hexa hex-rgb
                                   (fl/ (fx->fl (fxand ?hexcolor #xFF))
                                        255.0)))))]))])

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
       (CSS<$> (CSS<?> [(<delimiter>) (CSS:<^> (<css-%flunit>))]) 1.0)))
     
   (define make-parser : (-> (CSS-Parser (Listof Any)) (CSS-Parser (Listof Any)) (CSS-Parser (Listof Any)))
     ;;; https://github.com/w3c/csswg-drafts/issues/266
     (lambda [c1 c2]
       (CSS<&> c1 (CSS<?> [(<css-comma>) (CSS<#> c2 '(2)) (make-alpha-parser <css-comma>)]
                          [else          (CSS<*> c2 '(2)) (make-alpha-parser <css-slash>)]))))

   (define <:rgb:> (CSS:<^> (<rgb-gamut>)))
   (define <:hue:> (CSS:<^> (<css-angle>)))])

(define-css-disjoint-filter <css-color> #:-> (U Symbol FlColor CSS-Wide-Keyword)
  ;;; https://drafts.csswg.org/css-color/#color-type
  ;;; https://drafts.csswg.org/css-color/#named-colors
  #:with [[hint? : Any #false]]
  (CSS:<~> (<css-keyword> (cons 'currentcolor (cons 'transparent (hash-keys css-named-colors))))
           (λ [[c : Symbol]] (cond [(not (eq? c 'currentcolor)) c]
                                   [(not hint?) c]
                                   [else css:inherit])))
  (<css#color>)
  (<css-color-notation>))

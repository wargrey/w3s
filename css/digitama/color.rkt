#lang typed/racket/base

;;; https://drafts.csswg.org/css-color

(provide (all-defined-out))

(require bitmap/color)
(require bitmap/digitama/color)
(require colorspace/misc)

(require "syntax/digicore.rkt")
(require "syntax/dimension.rkt")
(require "../recognizer.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-css-atomic-filter <css#color> #:-> Hexa #:with [[color-value : css:hash?]]
  (or (css-hex-color->rgba (css:hash-datum color-value))
      (make-exn:css:range color-value))
  #:where
  [(define (css-hex-color->rgba [hash-color : Keyword]) : (Option Hexa)
     ;;; https://drafts.csswg.org/css-color/#numeric-rgb
     (define color : String (keyword->string hash-color))
     (define digits : Index (string-length color))
     (define ?hexcolor : (Option Number)
       (case digits
         [(6 8) (string->number color 16)]
         [(3 4) (for/fold ([hexcolor : (Option Nonnegative-Fixnum) 0])
                          ([ch : Char (in-string color)])
                  (define digit : (U Integer Void)
                    (cond [(char-numeric? ch)   (fx- (char->integer ch) #x30)]
                          [(char<=? #\a ch #\f) (fx- (char->integer ch) #x37)]
                          [(char<=? #\A ch #\F) (fx- (char->integer ch) #x57)]))
                  (and hexcolor (byte? digit)
                       (fxior (fxlshift hexcolor 8)
                              (fxior (fxlshift digit 4)
                                     digit))))]
         [else #false]))
     (if (or (fx= digits 3) (fx= digits 6))
         (and (index? ?hexcolor)
              (hexa ?hexcolor 1.0))
         (and (exact-integer? ?hexcolor)
              (let ([hex-rgb (arithmetic-shift ?hexcolor -8)])
                (and (index? hex-rgb)
                     (hexa hex-rgb
                           (fl/ (fx->fl (fxand ?hexcolor #xFF))
                                255.0)))))))])

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
   (define <:hue:> (CSS:<^> (CSS:<+> (<css:integer>) (<css:flonum>) (<css:angle>))))])

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

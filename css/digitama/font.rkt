#lang typed/racket/base

;;; https://drafts.csswg.org/css-fonts
;;; https://drafts.csswg.org/css-fonts-4

(provide (all-defined-out))

(require racket/string)
(require racket/symbol)

(require bitmap/font)
(require bitmap/digitama/font)

(require "syntax/digicore.rkt")
(require "syntax/dimension.rkt")
(require "../recognizer.rkt")

(define &font : (Boxof Font) (box (default-font)))

(define css-font-synthesis-options : (Listof Symbol) '(weight style small-caps))

(define css-font-kerning-options : (Listof Symbol) '(auto normal none))
(define css-font-variant-ligatures-options : (Listof Symbol) '(normal none))
(define css-font-position-options : (Listof Symbol) '(normal sub super))
(define css-font-caps-options : (Listof Symbol) '(normal small-caps all-small-caps petite-caps all-petite-caps unicase titling-caps))
(define css-font-variant-options/21 : (Listof Symbol) '(normal small-caps))
  
(define-css-prefab-filter <css-system-font> #:-> Font #:format "default-css-~a-font"
  [caption       (default-font)]
  [icon          (default-font)]
  [menu          (default-font)]
  [message-box   (default-font)]
  [small-caption (default-font)]
  [status-bar    (default-font)])

(define css-font->longhand-properties : (->* (Font) ((HashTable Symbol Any)) (HashTable Symbol Any))
  (lambda [font [longhand css-longhand]]
    (let* ([longhand++ (hash-set longhand 'font-stretch (font-stretch font))]
           [longhand++ (hash-set longhand++ 'font-weight (font-weight font))]
           [longhand++ (hash-set longhand++ 'font-style (font-style font))]
           [longhand++ (hash-set longhand++ 'font-size (font-size font))])
      (hash-set longhand++ 'font-family (list (font-face->family* (font-face font)))))))

(define-css-disjoint-filter <font-stretch> #:-> (U Symbol CSS+%)
  ;;; https://drafts.csswg.org/css-fonts-4/#font-stretch-prop
  (<css-keyword> css-font-stretch-options)
  (<css+percentage>))

(define-css-disjoint-filter <font-weight> #:-> (U Symbol Integer)
  ;;; https://drafts.csswg.org/css-fonts/#font-weight-prop
  (<css-keyword> (remove 'medium css-font-weight-options))
  (<css:integer> 0 < 1000))
  
(define-css-disjoint-filter <font-size> #:-> (U Symbol Nonnegative-Inexact-Real CSS:Length:Font)
  ;;; https://drafts.csswg.org/css-fonts/#font-size-prop
  (<css-size>)
  (<css-keyword> css-font-size-option?))

(define-css-disjoint-filter <line-height> #:-> (U Symbol Nonnegative-Flonum CSS+Unitless CSS+% CSS:Length:Font CSS-Wide-Keyword)
  ;;; http://www.w3.org/TR/CSS2/visudet.html#propdef-line-height
  (<css+unitless>)
  (<css+percentage>)
  (<css+length:font>)
  (CSS:<~> (<css-keyword> '(normal inherit)) css-wide-keywords-filter-map))

(define <:font-family:> : (CSS-Parser (Listof Any))
  ;;; https://drafts.csswg.org/css-fonts/#font-family-prop
  ;;; https://drafts.csswg.org/css-fonts-4/#extended-generics
  (CSS<#> (CSS<+> (CSS:<^> (CSS:<+> (<css:string>) (<css-keyword> css-font-generic-families)))
                  (CSS<!> (CSS:<^> (<css:ident>))))))
  
(define <:font-shorthand:> : CSS-Shorthand+Parser
  ;;; https://drafts.csswg.org/css-fonts/#font-prop
  (cons (CSS<+> (CSS:<^> (<css-system-font>) css-font->longhand-properties)
                (CSS<&> (CSS<*> (CSS<+> (CSS<_> (CSS:<^> (<css-keyword> 'normal) '|Ignoring, some properties use it as defaults|))
                                        (CSS:<^> (<font-weight>) 'font-weight)
                                        (CSS:<^> (<css-keyword> css-font-style-option?) 'font-style)
                                        (CSS:<^> (<css-keyword> css-font-variant-options/21) 'font-variant)
                                        ; <font-stretch> also accepts percentage in css-fonts-4 specification,
                                        ; however it preempts the 'font-size
                                        (CSS:<^> (<css-keyword> css-font-stretch-options) 'font-stretch)))
                        (CSS:<^> (<font-size>) 'font-size)
                        (CSS<*> (CSS<?> [(<css-slash>) (CSS:<^> (<line-height>) 'line-height)]) '?)
                        (CSS<^> <:font-family:> 'font-family)))
        '(font-style font-variant font-weight font-stretch font-size line-height font-family
                     font-size-adjust font-kerning font-language-override)))

(define css->font-family : (CSS->Racket (U String Font-Family))
  (lambda [_ value]
    (let select ([families (if (list? value) value null)])
      (cond [(null? families) (let ([pfont (unbox &font)]) (font-face->family* (font-face pfont)))]
            [else (let ([family (car families)])
                    (or (and (css-font-generic-family? family) family)
                        (and (string? family) (face-filter family))
                        (and (list? family) (face-filter (string-join (map symbol->immutable-string (filter symbol? family)))))
                        (select (cdr families))))]))))

(define css->font-size : (CSS->Racket Nonnegative-Real)
  (lambda [property value]
    (cond [(symbol? value) (generic-font-size-filter value (font-size (unbox &font)) (font-size (default-font)))]
          [(nonnegative-flonum? value) value]
          [(css+%? value) (fl* (css+%-value value) (css-em))]
          [(css+length? value) (css:length->scalar value #false)]
          [(eq? property 'min-font-size) 0.0]
          [(eq? property 'max-font-size) +inf.0]
          [(eq? value css:inherit) (css-em)]
          [else (generic-font-size-filter 'medium (font-size (unbox &font)) (font-size (default-font)))])))

(define css->line-height : (-> Symbol Any (U Nonnegative-Flonum CSS+Unitless))
  (lambda [_ value]
    (cond [(css+length? value) (css:length->scalar value #false)]
          [(nonnegative-flonum? value) value #|computed value is the used value|#]
          [(css+%? value) (fl* (css+%-value value) (css-em))]
          [(css+unitless? value) value #|computed value is *not* the used value|#]
          [else #|'normal|# +nan.0 #|computed value is *not* the used value|#])))

(define font-weight*? : (-> Any Boolean : (U Symbol Nonnegative-Integer))
  (lambda [v]
    (or (symbol? v)
        (exact-nonnegative-integer? v))))

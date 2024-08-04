#lang typed/racket/base

(provide (all-defined-out) <css-color> <css#color>)
(provide (all-from-out geofun/color))

(require geofun/digitama/color)
(require geofun/color)

(require racket/symbol)

(require "digitama/syntax/digicore.rkt")
(require "digitama/color.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define current-css-element-color : (Parameterof Color FlRGBA) (make-parameter (rgb* #x000000) rgb*))
(default-make-currentcolor current-css-element-color)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define css-color-property-parsers : (->* (Symbol) ((U Regexp (Listof Symbol))) (Option CSS-Declaration-Parser))
  (lambda [name [px.names #px"-color$"]]
    (or (and (eq? name 'color) (<css-color> '#:inherit-currentcolor))
        (and (or (and (list? px.names) (memq name px.names))
                 (and (regexp? px.names) (regexp-match? px.names (symbol->immutable-string name))))
             (<css-color>)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define css->color : (CSS->Racket (U FlColor CSS-Wide-Keyword 'currentcolor))
  (lambda [desc-name color]
    (cond [(flcolor? color) color]
          [(eq? color 'currentcolor) color #| evaluated at used-value time |#]
          [(symbol? color) (rgb* color)]
          [(exact-integer? color) (rgb* color)]
          [else css:initial])))

(define css-rgba-ref : (case-> [CSS-Values (Option CSS-Values) -> FlRGBA]
                               [CSS-Values (Option CSS-Values) Symbol -> (CSS-Maybe FlRGBA)]
                               [CSS-Values (Option CSS-Values) Symbol Color -> FlRGBA]
                               [CSS-Values (Option CSS-Values) Symbol (Option Color) -> (Option FlRGBA)])
  ;;; NOTE
  ;; `css-ref` will save all the values as computed value if it knows how to transform the cascaded values,
  ;; hence the `css-rgba-ref` to generate a more useful used value for clients so that clients do not need
  ;; to trace the `currentcolor` all the time. The correct current color may escape from the `parameterize`.
  (case-lambda
    [(declared-values inherited-values)
     (define color : (CSS-Maybe (U 'currentcolor FlColor)) (css-ref declared-values inherited-values 'color css->color))
     (if (css-wide-keyword? color) (current-css-element-color) (rgb* color))]
    [(declared-values inherited-values property)
     (define xxx-color : (CSS-Maybe (U 'currentcolor FlColor)) (css-ref declared-values inherited-values property css->color))
     (cond [(eq? xxx-color 'currentcolor) (current-css-element-color)]
           [(css-wide-keyword? xxx-color) xxx-color]
           [else (rgb* xxx-color)])]
    [(declared-values inherited-values property defcolor)
     (define xxx-color : (CSS-Maybe (U 'currentcolor FlColor)) (css-ref declared-values inherited-values property css->color))
     (cond [(eq? xxx-color 'currentcolor) (current-css-element-color)]
           [(css-wide-keyword? xxx-color) (and defcolor (rgb* defcolor))]
           [else (rgb* xxx-color)])]))

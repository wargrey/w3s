#lang typed/racket

;;; https://drafts.csswg.org/css-device-adapt
;;; https://drafts.csswg.org/css-round-display/#extending-viewport-rule
;;; https://github.com/w3c/csswg-drafts/issues/258

(provide (all-defined-out))

(require "syntax/condition.rkt")
(require "syntax/digicore.rkt")
(require "syntax/dimension.rkt")
(require "../recognizer.rkt")

(define-type CSS-Viewport-Filter (CSS-Cascaded-Value+Filter (HashTable Symbol CSS-Media-Datum) (HashTable Symbol CSS-Media-Datum)))

;; https://drafts.csswg.org/css-device-adapt/#viewport-desc
(define css-viewport-parsers : CSS-Declaration-Parsers
  (lambda [suitcased-name !]
    (define-css-disjoint-filter viewport-length-filter #:-> (U Symbol Nonnegative-Inexact-Real)
      (<css-keyword> 'auto)
      (<css:percentage> nonnegative-flonum?)
      (<css+length>))
    (case suitcased-name
      [(width) (css-make-pair-parser (viewport-length-filter) 'min-width 'max-width)]
      [(height) (css-make-pair-parser (viewport-length-filter) 'min-height 'max-height)]
      [(zoom min-zoom max-zoom) (CSS:<+> (<css-keyword> 'auto) (CSS:<~> (<css+%real>) real->double-flonum))]
      [(min-width max-width min-height max-height) (viewport-length-filter)]
      [(orientation) (<css-keyword> '(auto portrait landscape))]
      [(user-zoom) (<css-keyword> '(zoom fixed))]
      [(viewport-fit) (<css-keyword> '(auto contain cover))])))

(define css-viewport-filter : CSS-Viewport-Filter
  ;;; https://drafts.csswg.org/css-device-adapt/#constraining
  ;;; https://drafts.csswg.org/css-device-adapt/#handling-auto-zoom
  ;;; https://drafts.csswg.org/css-device-adapt/#media-queries
  ;;; https://drafts.csswg.org/css-device-adapt/#viewport-desc
  (lambda [cascaded-values _ initial-viewport]
    ;;; Notes
    ;; * @viewport is a controversial @rule which is easy to be used incorrectly,
    ;; * @viewport is rarely used in desktop applications, and
    (define-values (initial-width initial-height)
      (let ([w (hash-ref initial-viewport 'width (const #false))]
            [h (hash-ref initial-viewport 'height (const #false))])
        (values (if (and (real? w) (positive? w)) (flmax (real->double-flonum w) 1.0) 1.0)
                (if (and (real? h) (positive? h)) (flmax (real->double-flonum h) 1.0) 1.0))))
    (define defzoom : Nonnegative-Flonum (default-css-viewport-auto-zoom))
    (define (smart [maix : (-> Flonum Flonum Flonum)] [v1 : (U Flonum Symbol)] [v2 : (U Flonum Symbol)]) : (U Flonum Symbol)
      (cond [(and (symbol? v1) (symbol? v2)) 'auto]
            [(symbol? v1) v2]
            [(symbol? v2) v1]
            [else (maix v1 v2)]))
    (define css->width (make-css->size 'auto #:100% initial-width))
    (define css->height (make-css->size 'auto #:100% initial-height))
    (define min-zoom : Flonum (css-ref cascaded-values #false 'min-zoom nonnegative-flonum? 0.0))
    (define max-zoom : Flonum (flmax min-zoom (css-ref cascaded-values #false 'max-zoom nonnegative-flonum? +inf.0)))
    (define min-width : (U Flonum Symbol) (css-ref cascaded-values #false 'min-width css->width))
    (define max-width : (U Flonum Symbol) (css-ref cascaded-values #false 'max-width css->width))
    (define min-height : (U Flonum Symbol) (css-ref cascaded-values #false 'min-height css->height))
    (define max-height : (U Flonum Symbol) (css-ref cascaded-values #false 'max-height css->height))
    (define-values (width height)
      (let* ([width (smart flmax min-width (smart flmin max-width initial-width))]
             [height (smart flmax min-height (smart flmin max-height initial-height))]
             [width (cond [(and (symbol? width) (symbol? height)) initial-width]
                          [(symbol? width) (if (zero? initial-height) initial-width (fl* height (fl/ initial-width initial-height)))]
                          [else width])])
        (values width (cond [(flonum? height) height]
                            [(zero? initial-width) initial-height]
                            [else (fl* width (fl/ initial-height initial-width))]))))
    (define orientation : Symbol (css-ref cascaded-values #false 'orientation symbol? 'auto))
    (define user-zoom : Symbol (css-ref cascaded-values #false 'user-zoom symbol? 'zoom))
    (define zone : Flonum (max min-zoom (min max-zoom (css-ref cascaded-values #false 'zoom nonnegative-flonum? defzoom))))
    (define viewport-fit : Symbol (css-ref cascaded-values #false 'viewport-fit symbol? 'auto))
    (define actual-viewport (hash-copy initial-viewport))
    (for/fold ([actual-viewport : (HashTable Symbol CSS-Media-Datum) initial-viewport])
              ([name (in-list      '(min-zoom max-zoom width height orientation user-zoom zone viewport-fit))]
               [value (in-list (list min-zoom max-zoom width height orientation user-zoom zone viewport-fit))])
      (define old : (Option CSS-Media-Datum) (hash-ref actual-viewport name (thunk #false)))
      (if (eq? old value) actual-viewport (hash-set actual-viewport name value)))))

(define-values (default-css-viewport-parsers default-css-viewport-filter default-css-viewport-auto-zoom)
  (values (make-parameter css-viewport-parsers)
          (make-parameter css-viewport-filter)
          (ann (make-parameter 1.0) (Parameterof Nonnegative-Flonum))))

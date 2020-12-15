#lang typed/racket/base

;;; https://drafts.csswg.org/css-conditional
;;; https://drafts.csswg.org/mediaqueries
;;; https://drafts.csswg.org/css-round-display/#extending-media-queries

(provide (all-defined-out))

(require "digicore.rkt")
(require "dimension.rkt")
(require "../../recognizer.rkt")

(require (for-syntax racket/base))
(require (for-syntax racket/syntax))
(require (for-syntax racket/symbol))

(define-syntax (define-make-media-features stx)
  (syntax-case stx [:]
    [(_ make-immutable-features [feature : DataType #:= defval] ...)
     (with-syntax ([(args ...)
                    (for/fold ([args null])
                              ([argument (in-list (syntax->list #'([feature : DataType defval] ...)))])
                      (cons (datum->syntax argument (string->keyword (symbol->immutable-string (car (syntax->datum argument)))))
                            (cons argument args)))])
       (syntax/loc stx
         (define (make-immutable-features args ...) : CSS-Media-Features
           (make-immutable-hasheq
            (list (cons 'feature feature) ...)))))]))

;; https://drafts.csswg.org/css-conditional/#at-supports
;; https://drafts.csswg.org/mediaqueries/#media-types
;; https://drafts.csswg.org/mediaqueries/#mq-syntax
;; https://drafts.csswg.org/mediaqueries/#mq-features
(define-type CSS-Media-Datum (U Symbol Exact-Rational Flonum))
(define-type CSS-Media-Features (HashTable Symbol CSS-Media-Datum))
(define-type CSS-Media-Feature-Query (Vector Symbol Char CSS-Media-Datum))
(define-type CSS-Media-Query (U (Boxof Symbol) CSS-Feature-Query (Pairof CSS-Media-Query CSS-Feature-Query)))
(define-type CSS-Feature-Query (U CSS-Not CSS-And CSS-Or CSS-Media-Feature-Query CSS-Declaration Symbol CSS-Syntax-Error))
(define-type CSS-Condition (U CSS-Media-Query CSS-Feature-Query))

(struct css-not ([condition : CSS-Condition]) #:transparent #:type-name CSS-Not)
(struct css-and ([conditions : (Listof CSS-Condition)]) #:transparent #:type-name CSS-And)
(struct css-or ([conditions : (Listof CSS-Condition)]) #:transparent #:type-name CSS-Or)

(define css-condition-okay? : (-> CSS-Media-Query (-> CSS-Condition Boolean) Boolean)
  (lambda [query okay?]
    (cond [(css-not? query) (not (css-condition-okay? (css-not-condition query) okay?))]
          [(css-and? query) (andmap (λ [[q : CSS-Condition]] (css-condition-okay? q okay?)) (css-and-conditions query))]
          [(css-or? query) (ormap (λ [[q : CSS-Condition]] (css-condition-okay? q okay?)) (css-or-conditions query))]
          [else (okay? query)])))

;; https://drafts.csswg.org/mediaqueries/#media-descriptor-table
;; https://drafts.csswg.org/mediaqueries/#mf-deprecated
(define-type CSS-@Supports-Okay? (-> Symbol Any Boolean))
(define-type CSS-@Media-Filters (-> Symbol Boolean (-> Void) (U Void (CSS:Filter CSS-Media-Datum))))

(define css-media-feature-filters : CSS-@Media-Filters
  (lambda [downcased-name min/max? deprecated!]
    (case downcased-name
      [(width height device-width device-height)
       (when (or (eq? downcased-name 'device-width) (eq? downcased-name 'device-height)) (deprecated!))
       (<css+length>)]
      [(aspect-ratio device-aspect-ratio)
       (when (eq? downcased-name 'device-aspect-ratio) (deprecated!))
       (CSS:<~> (<css:ratio>) real->double-flonum)]
      [(resolution) (CSS:<+> (CSS:<=> (<css-keyword> 'infinite) +inf.0) (<css+resolution>))]
      [(color color-index monochrome) (<css:integer> exact-nonnegative-integer?)]
      [(grid) #|legacy descriptor|# (when (false? min/max?) (<css-boolean>))]
      [(orientation) (<css-keyword> '(portrait landscape))]
      [(scan) (<css-keyword> '(interlace progressive))]
      [(update) (<css-keyword> '(none slow fast))]
      [(overflow-block) (<css-keyword> '(none scroll optional-paged paged))]
      [(overflow-inline) (<css-keyword> '(none scroll))]
      [(color-gamut) (<css-keyword> '(srgb p3 rec2020))]
      [(pointer any-pointer) (<css-keyword> '(none coarse fine))]
      [(havor any-havor) (<css-keyword> '(none havor))]
      [(scripting) (<css-keyword> '(none initial-only enabled))]
      [(shape) (<css-keyword> '(rect round))])))

(define css-deprecate-media-type : (Parameterof Boolean) (make-parameter #false))
(define default-css-media-type : (Parameterof Symbol) (make-parameter 'all))

;;; NOTE: The default values are assumed for bitmap device.
(define-make-media-features make-css-media-features
  [width           : Nonnegative-Flonum      #:= +inf.0]
  [height          : Nonnegative-Flonum      #:= +inf.0]
  [aspect-ratio    : Positive-Exact-Rational #:= 1/1]
  [orientation     : Symbol                  #:= 'portrait]
  [shape           : Symbol                  #:= 'rect]

  [resolution      : Nonnegative-Flonum      #:= 2.0] ; the drafts don't mention if 0.0 is allowed.
  [scan            : Symbol                  #:= 'progressive]
  [grid            : (U One Zero)            #:= 0]
  [update          : Symbol                  #:= 'slow]
  [overflow-block  : Symbol                  #:= 'none]
  [overflow-inline : Symbol                  #:= 'none]

  [color           : Natural                 #:= 8]
  [color-index     : Natural                 #:= 0]
  [monochrome      : Natural                 #:= 0]
  [color-gamut     : Symbol                  #:= 'srgb]

  [pointer         : Symbol                  #:= 'none]
  [hover           : Symbol                  #:= 'none]
  [any-pointer     : Symbol                  #:= 'none]
  [any-hover       : Symbol                  #:= 'none]

  [script          : Symbol                  #:= 'none])

(define-values (default-css-media-features default-css-media-feature-filters default-css-feature-support?)
  (values (make-parameter (make-css-media-features))
          (make-parameter css-media-feature-filters)
          (make-parameter (ann (λ _ #true) CSS-@Supports-Okay?))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define css-@media-okay? : (-> (Listof CSS-Media-Query) CSS-Media-Features Boolean)
  ;;; https://drafts.csswg.org/mediaqueries/#evaluating
  ;;; https://drafts.csswg.org/mediaqueries/#media-types
  ;;; https://drafts.csswg.org/mediaqueries/#boolean-context
  (lambda [queries features]
    (define (okay? [query : CSS-Condition]) : Boolean
      (cond [(vector? query)
             (define name : Symbol (vector-ref query 0))
             (define value : CSS-Media-Datum (vector-ref query 2))
             (define feature : (U CSS-Media-Datum EOF) (hash-ref features name (λ _ eof)))
             (cond [(symbol? value) (eq? value feature)] ; TODO: values of color-gamut are ordered symbols
                   [(real? feature) (case (vector-ref query 1)
                                      [(#\>) (> feature value)] [(#\≥) (>= feature value)]
                                      [(#\<) (< feature value)] [(#\≤) (<= feature value)]
                                      [else (= feature value)])]
                   [else #false])]
            [(symbol? query)
             (define feature : CSS-Media-Datum (hash-ref features query (λ _ 'none)))
             (nor (eq? feature 'none) (eq? feature 0)
                  (and (number? feature) (zero? feature)))]
            [(box? query)
             (and (memq (unbox query) (list (default-css-media-type) 'all))
                  #true)]
            [else (and (pair? query)
                       (css-condition-okay? (car query) okay?)
                       (css-condition-okay? (cdr query) okay?))]))
    (or (null? queries)
        (for/or ([query (in-list queries)])
          (css-condition-okay? query okay?)))))

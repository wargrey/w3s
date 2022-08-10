#lang typed/racket/base

(provide (all-defined-out))

;;; https://drafts.csswg.org/css-values/#absolute-lengths
;;; https://drafts.csswg.org/css-values/#relative-lengths
;;; https://drafts.csswg.org/css-egg/#astro-units
;;; https://drafts.csswg.org/css-egg/#traditional-time

(require racket/math)

(require digimon/dimension)

(require "digicore.rkt")
(require "misc.rkt")

(require (for-syntax racket/base))
(require (for-syntax racket/string))
(require (for-syntax racket/symbol))
(require (for-syntax racket/syntax))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax (define-dimensional-tokens stx)
  (syntax-case stx []
    [(_ parent
        ([id #:+ ID #:=> canonical-unit #:with id->scalar argv ...] ...)
        ([ctoken #:+ CToken #:-> cparent] ...))
     (with-syntax* ([token->datum (format-id #'parent "~a->datum" (syntax-e #'parent))]
                    [token-filter (format-id #'parent "~a-filter" (syntax-e #'parent))]
                    [([id? +id? css:id->scalar <id> <+id>] ...)
                     (for/list ([<id> (in-list (syntax->list #'(id ...)))])
                       (define varname (symbol->immutable-string (syntax-e <id>)))
                       (list (format-id <id> "~a?" (syntax-e <id>))
                             (format-id <id> "~a?" (string-replace varname ":" "+"))
                             (format-id <id> "~a->scalar" (syntax-e <id>))
                             (format-id <id> "<~a>" (syntax-e <id>))
                             (format-id <id> "<~a>" (string-replace varname "css:" "css+"))))])
       (syntax/loc stx
         (begin (struct id parent () #:transparent #:type-name ID) ...
                (struct ctoken cparent () #:transparent #:type-name CToken) ...

                (define css:id->scalar : (case-> [(U ID CSS-Zero) -> Flonum]
                                                 [(U ID CSS-Zero) True -> Flonum]
                                                 [(U ID CSS-Zero) False -> Nonnegative-Flonum])
                  (lambda [token [direction? #false]]
                    (cond [(not (id? token)) 0.0]
                          [(and direction?) (id->scalar (css:dimension-datum token) (css:dimension-unit token) argv ...)]
                          [else (id->scalar (flabs (css:dimension-datum token)) (css:dimension-unit token) argv ...)])))
                ...

                (define +id? : (-> Any Boolean : #:+ (U ID CSS-Zero))
                  (lambda [token]
                    (or (css-zero? token)
                        (and (id? token)
                             (fl>= (css:dimension-datum token) 0.0)))))
                ...

                (define <id> : (-> (CSS:Filter Flonum))
                  (lambda []
                    (λ [[token : CSS-Syntax-Any]]
                      (cond [(id? token) (css:id->scalar token)]
                            [(css:dimension? token) (make-exn:css:unit token)]
                            [else #false]))))
                ...

                (define <+id> : (-> (CSS:Filter Nonnegative-Flonum))
                  (lambda []
                    (λ [[token : CSS-Syntax-Any]]
                      (cond [(+id? token) (css:id->scalar token #false)]
                            [(id? token) (make-exn:css:range token)]
                            [(css:dimension? token) (make-exn:css:unit token)]
                            [else #false]))))
                ...
                
                (define token->datum : (-> (U CSS:Dimension CSS-Zero) Flonum)
                  (lambda [instance]
                    (cond [(id? instance) (id->scalar (css:dimension-datum instance) (css:dimension-unit instance) argv ...)] ...
                          [(css-zero? instance) 0.0]
                          [else +nan.0]))))))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define css-dimenv : Dimension-Environment
  (make-dimension-environment #:em css-em #:ex css-ex #:cap css-cap #:ch css-ch #:ic css-ic #:lh css-lh
                              #:vw css-vw #:vh css-vh #:rem css-rem #:rlh css-rlh))

(define-dimensional-tokens css:dimension
  ([css:length          #:+ CSS:Length          #:=> px         #:with dim:length css-dimenv]
   [css:angle           #:+ CSS:Angle           #:=> deg        #:with dim:angle]
   [css:time            #:+ CSS:Time            #:=> s          #:with dim:time]
   [css:frequency       #:+ CSS:Frequency       #:=> kz         #:with dim:frequency]
   [css:resolution      #:+ CSS:Resolution      #:=> dppx       #:with dim:resolution])
  ([css:length:font     #:+ CSS:Length:Font     #:-> css:length]
   [css:length:viewport #:+ CSS:Length:Viewport #:-> css:length]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define <css:length:font> : (-> (CSS:Filter (U Flonum CSS:Length:Font)))
  (let ([<fallback> (<css:length>)])
    (lambda []
      (λ [[token : CSS-Syntax-Any]]
        (cond [(css:length:font? token) token]
              [else (<fallback> token)])))))

(define <css+length:font> : (-> (CSS:Filter (U Nonnegative-Flonum CSS:Length:Font)))
  (let ([<fallback> (<css+length>)])
    (lambda []
      (λ [[token : CSS-Syntax-Any]]
        (cond [(and (css:length:font? token) (css+length? token)) token]
              [else (<fallback> token)])))))

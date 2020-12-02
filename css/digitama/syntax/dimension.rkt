#lang typed/racket/base

(provide (all-defined-out))

;;; https://drafts.csswg.org/css-values/#absolute-lengths
;;; https://drafts.csswg.org/css-values/#relative-lengths
;;; https://drafts.csswg.org/css-egg/#astro-units
;;; https://drafts.csswg.org/css-egg/#traditional-time

(require racket/math)

(require "digicore.rkt")
(require "misc.rkt")

(require (for-syntax racket/base))
(require (for-syntax racket/string))
(require (for-syntax racket/syntax))
(require (for-syntax syntax/parse))

(define-syntax (define-dimensional-tokens stx)
  (syntax-case stx []
    [(_ parent
        ([id #:+ ID #:=> canonical-unit [conversion ...]] ...)
        ([ctoken #:+ CToken #:-> cparent] ...))
     (with-syntax* ([token->datum (format-id #'parent "~a->datum" (syntax-e #'parent))]
                    [token-filter (format-id #'parent "~a-filter" (syntax-e #'parent))]
                    [([id? +id? css:id->scalar css-id->scalar <id> <+id>] ...)
                     (for/list ([<id> (in-list (syntax->list #'(id ...)))])
                       (define varname (symbol->string (syntax-e <id>)))
                       (list (format-id <id> "~a?" (syntax-e <id>))
                             (format-id <id> "~a?" (string-replace varname ":" "+"))
                             (format-id <id> "~a->scalar" (syntax-e <id>))
                             (format-id <id> "~a->scalar" (string-replace varname ":" "-"))
                             (format-id <id> "<~a>" (syntax-e <id>))
                             (format-id <id> "<~a>" (string-replace varname "css:" "css+"))))])
       (syntax/loc stx
         (begin (struct id parent () #:transparent) ... (define-type ID id) ...
                (struct ctoken cparent () #:transparent) ... (define-type CToken ctoken) ...

                (define css-id->scalar : (case-> [Nonnegative-Flonum Symbol -> Nonnegative-Flonum]
                                                 [Flonum Symbol -> Flonum])
                  (lambda [canonical-unit unit]
                    (case unit
                      [(canonical-unit) canonical-unit]
                      conversion ...
                      [else +nan.0])))
                ...

                (define css:id->scalar : (case-> [(U ID CSS-Zero) -> Flonum]
                                                 [(U ID CSS-Zero) True -> Flonum]
                                                 [(U ID CSS-Zero) False -> Nonnegative-Flonum])
                  (lambda [token [direction? #false]]
                    (cond [(not (id? token)) 0.0]
                          [(and direction?) (css-id->scalar (css:dimension-datum token) (css:dimension-unit token))]
                          [else (css-id->scalar (flabs (css:dimension-datum token)) (css:dimension-unit token))])))
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
                    (cond [(id? instance) (css-id->scalar (css:dimension-datum instance) (css:dimension-unit instance))] ...
                          [(css-zero? instance) 0.0]
                          [else +nan.0]))))))]))

(define-dimensional-tokens css:dimension
  ([css:length         #:+ CSS:Length          #:=> px
                       [[(cm)    (fl* (fl/ 96.0 2.54) px)]
                        [(mm)    (fl* (fl/ 96.0 25.4) px)]                    #;1cm/10
                        [(q)     (fl* (fl/ 96.0 101.6) px)]                   #;1cm/40
                        [(in)    (fl* 96.0 px)]
                        [(pc)    (fl* 16.0 px)]                               #;1in/6
                        [(pt)    (fl* (fl/ 96.0 72.0) px)]                    #;1in/72
                        [(em)    (fl* (css-em) px)]
                        [(ex)    (fl* (css-ex) px)]
                        [(cap)   (fl* (css-cap) px)]
                        [(ch)    (fl* (css-ch) px)]
                        [(ic)    (fl* (css-ic) px)]
                        [(lh)    (fl* (css-lh) px)]
                        [(rem)   (fl* (css-rem) px)]
                        [(rlh)   (fl* (css-rlh) px)]
                        [(vw vi) (fl* (fl* 0.01 (css-vw)) px)]
                        [(vh vb) (fl* (fl* 0.01 (css-vh)) px)]
                        [(vmin)  (fl* (fl* 0.01 (min (css-vw) (css-vh))) px)]
                        [(vmax)  (fl* (fl* 0.01 (max (css-vw) (css-vh))) px)]
                        [(apc)   (fl* (fl* (fl/ 96.0 2.54) 3.086) px)]        #;3.086cm
                        [(pls)   (fl* 1.133 px)]
                        [(ls)    (fl* 1.133e12 px)]                           #;1e12pls
                        #;[(pc)    (fl* (fl* (fl/ 96.0 2.54) 3.086e18) px)]   #;1e18apc]]
   ;;; https://drafts.csswg.org/css-values/#angles
   [css:angle          #:+ CSS:Angle           #:=> deg
                       [[(grad)  (fl* 0.9 deg)]
                        [(rad)   (fl* (fl/ 180.0 pi) deg)]
                        [(turn)  (fl* 360.0 deg)]]]
   ;;; https://drafts.csswg.org/css-values/#time
   [css:time           #:+ CSS:Time            #:=> s
                       [[(ms)    (fl* 0.001 s)]
                        [(min)   (fl* 60.0 s)]
                        [(h)     (fl* 3600.0 s)]
                        [(ft)    (fl* 1.2096e6 s)]
                        [(mft)   (fl* 1.2096e3 s)]]]
   ;;; https://drafts.csswg.org/css-values/#frequency
   [css:frequency      #:+ CSS:Frequency       #:=> kz
                       [[(khz)   (fl* 0.001 kz)]]]
   ;;; https://drafts.csswg.org/css-values/#resolution
   [css:resolution     #:+ CSS:Resolution      #:=> dppx
                       [[(dpcm)  (fl* (fl/ 2.54 96.0) dppx)]
                        [(dpi)   (fl* (fl/ 1.0 96.0) dppx)]
                        [(x)     dppx]]])
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

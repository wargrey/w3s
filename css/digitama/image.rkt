#lang typed/racket/base

;;; https://drafts.csswg.org/css-images

(provide (all-defined-out))

(require racket/string)

(require bitmap/base)
(require bitmap/resize)
(require bitmap/misc)
(require bitmap/composite)
(require bitmap/constructor)
(require bitmap/invalid)

(require "syntax/digicore.rkt")
(require "syntax/dimension.rkt")
(require "syntax/misc.rkt")
(require "../recognizer.rkt")
(require "../color.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Image-Set-Option (List CSS-Image-Datum Positive-Flonum))
(define-type Image-Set-Options (Listof Image-Set-Option))
(define-type Linear-Color-Hint (U Flonum CSS-%)) ; <length-percentage>
(define-type Linear-Color-Stop (Pairof Color Linear-Color-Hint))

(define-type CSS-Image-Datum (U CSS-Image String))

(define-predicate css-image-datum? CSS-Image-Datum)

(define-css-value css-image #:as CSS-Image ())
(define-css-value image #:as Image #:=> css-image ([content : CSS-Image-Datum] [fallback : Color]))
(define-css-value image-set #:as Image-Set #:=> css-image ([options : Image-Set-Options]))
(define-css-value linear-gradient #:as Linear-Gradient #:=> css-image ([direction : Flonum] [stops : (Listof (U Linear-Color-Stop Linear-Color-Hint))]))

(define css-image-rendering-option : (Listof Symbol) '(auto crisp-edges pixelated))
(define css-image-fit-option : (Listof Symbol) '(fill contain cover none scale-down))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-css-function-filter <css-image-notation> #:-> CSS-Image
  ;;; https://drafts.csswg.org/css-images/#image-notation
  ;;; https://drafts.csswg.org/css-images/#image-set-notation
  [(image) #:=> [(image "" [fallback ? index? symbol? flcolor?])
                 (image [content ? css-image? string?] [fallback ? symbol? index? flcolor?])]
   (CSS<+> (CSS:<^> (<css-color>)) ; NOTE: both color and url accept strings, however their domains are not intersective.
           (CSS<&> (CSS:<^> (CSS:<+> (<css-image>) (<css:string>)))
                   (CSS<$> (CSS<?> [(<css-comma>) (CSS:<^> (<css-color>))]) 'transparent)))]
  [(image-set) #:=> [(image-set [options ? css-image-sets?])]
   (CSS<!> (CSS<#> (CSS<!> (CSS:<^> (list (CSS:<+> (<css:string>) (<css-image>)) (<css+resolution>))))))]
  #:where
  [(define-predicate css-image-sets? Image-Set-Options)])

#;(define-css-function-filter <css-gradient-notation> #:-> CSS-Image
  ;;; https://drafts.csswg.org/css-images/#gradients
  [(linear-gradient) #:=> [(linear-gradient [direction ? flonum?])
                           (linear-gradient #;to-bottom 180.0)]
   (CSS<+> (CSS:<^> (<css-color>)) ; NOTE: both color and url accept strings, however their domains are not intersective.
           (CSS<&> (CSS:<^> (CSS:<+> (<css-image>) (<css:string>)))
                   (CSS<$> (CSS<?> [(<css-comma>) (CSS:<^> (<css-color>))]) 'transparent)))]
  #:where
  [(define-predicate css-image-sets? Image-Set-Options)])

(define-css-disjoint-filter <css-image> #:-> CSS-Image-Datum
  ;;; https://drafts.csswg.org/css-images/#image-values
  ;;; https://drafts.csswg.org/css-images/#invalid-image
  (<css-image-notation>)
  (<css:url>))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define make-image-normalizer : (case-> [Nonnegative-Real Positive-Flonum (-> Bitmap) -> (-> (CSS-Maybe Bitmap) Bitmap)]
                                        [Nonnegative-Real Positive-Flonum -> (-> (CSS-Maybe Bitmap) (CSS-Maybe Bitmap))])
  (let ([normalize (λ [[h : Nonnegative-Real] [d : Positive-Flonum] [b : Bitmap]]
                     (bitmap-scale (bitmap-alter-density b d) (/ h (bitmap-height b))))])
    (case-lambda
      [(height density)
       (λ [[raw : (CSS-Maybe Bitmap)]]
         (if (bitmap? raw) (normalize height density raw) css:initial))]
      [(height density mk-image)
       (λ [[raw : (CSS-Maybe Bitmap)]]
         (cond [(bitmap? raw) (normalize height density raw)]
               [else (let ([alt-image : Bitmap (mk-image)])
                       (cond [(> (bitmap-height alt-image) height) (normalize height density alt-image)]
                             [else (bitmap-cc-superimpose (bitmap-blank height height #:density density)
                                                          (bitmap-alter-density alt-image density))]))]))])))

(define image->bitmap : (->* (CSS-Image-Datum) (Positive-Flonum) Bitmap)
  (lambda [img [the-density (default-bitmap-density)]]
    (cond [(non-empty-string? img)
           (with-handlers ([exn? (λ [[e : exn]] (css-log-read-error e img) the-invalid-bitmap)])
             (bitmap img the-density))]
          [(image? img)
           (define bmp : Bitmap (image->bitmap (image-content img)))
           (cond [(bitmap-invalid? bmp) bmp]
                 [else (let ([color (css->color '_ (image-fallback img))])
                         (bitmap-solid (if (or (flcolor? color) (symbol? color)) color 'transparent)))])]
          [(image-set? img)
           (define-values (src density)
             (for/fold ([the-src : CSS-Image-Datum ""]
                        [resolution : Nonnegative-Flonum 0.0])
                       ([option (in-list (image-set-options img))])
               (define this-density : Nonnegative-Flonum (cadr option))
               ; - resoltions should not duplicate, but if so, use the first one.
               ; - if there is no specific resolution, use the highest one.
               (if (or (= resolution the-density) (fl<= this-density resolution))
                   (values the-src resolution)
                   (values (car option) this-density))))
           (cond [(zero? density) the-invalid-bitmap]
                 [else (image->bitmap src density)])]
          [else the-invalid-bitmap])))

(define css->normalized-image : (All (racket) (-> (-> (CSS-Maybe Bitmap) racket) (CSS->Racket racket)))
  (lambda [normalize]
    (λ [_ image]
      (cond [(bitmap? image) (normalize image)]
            [(css-image-datum? image) (normalize (image->bitmap image))]
            [else (normalize css:initial)]))))

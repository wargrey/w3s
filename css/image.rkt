#lang typed/racket/base

(provide (all-defined-out) <css-image>)
(provide (all-from-out bitmap/base))

(require racket/symbol)

(require bitmap/base)
(require bitmap/constructor)
(require bitmap/color)

(require "digitama/syntax/digicore.rkt")
(require "digitama/syntax/dimension.rkt")
(require "digitama/image.rkt")
(require "recognizer.rkt")

(define-type CSS-Make-Icon (-> #:height Nonnegative-Real #:color Color Bitmap))

;;; TODO: meanwhile <css+resolution> accepts 0
;;; TODO: deal with the default resolution

(define css-image-property-parsers : (->* (Symbol) ((U Regexp (Listof Symbol))) (Option CSS-Declaration-Parser))
  ;;; https://drafts.csswg.org/css-images/#image-processing
  (lambda [name [px.names #px"-(image|icon|logo)$"]]
    (case name
      [(image-resolution) (CSS<*> (CSS:<^> (CSS:<+> (<css-keyword> '(from-image snap)) (<css+resolution>))) '+)]
      [(image-rendering) (<css-keyword> css-image-rendering-option)]
      [else (and (or (and (list? px.names) (memq name px.names))
                     (and (regexp? px.names) (regexp-match? px.names (symbol->immutable-string name))))
                 (<css-image>))])))

(define make-css->bitmap : (All (racket) (case-> [(-> (CSS-Maybe Bitmap) racket) -> (CSS->Racket racket)]
                                                 [Nonnegative-Real (-> Bitmap) -> (CSS->Racket Bitmap)]
                                                 [Nonnegative-Real Positive-Flonum (-> Bitmap) -> (CSS->Racket Bitmap)]
                                                 [Nonnegative-Real -> (CSS->Racket (CSS-Maybe Bitmap))]
                                                 [Nonnegative-Real Positive-Flonum -> (CSS->Racket (CSS-Maybe Bitmap))]))
  (case-lambda
    [(height density/image)
     (cond [(real? density/image) (css->normalized-image (make-image-normalizer height density/image))]
           [else (css->normalized-image (make-image-normalizer height (default-bitmap-density) density/image))])]
    [(height density mk-image) (css->normalized-image (make-image-normalizer height density mk-image))]
    [(height/normalize)
     (cond [(not (real? height/normalize)) (css->normalized-image height/normalize)]
           [else (css->normalized-image (make-image-normalizer height/normalize (default-bitmap-density)))])]))

(define-values (css->bitmap css->image)
  (values (css->normalized-image (λ [[raw : (CSS-Maybe Bitmap)]] raw))
          (css->normalized-image (λ [[raw : (CSS-Maybe Bitmap)]]
                                   (cond [(bitmap? raw) raw]
                                         [else (bitmap-solid)])))))

(define css-icon-ref : (-> CSS-Values (Option CSS-Values) Symbol CSS-Make-Icon Nonnegative-Real Color Bitmap)
  (let ([cache : (HashTable Any (-> Symbol Any Bitmap)) (make-hash)])
    (lambda [declared-values inherited-values property default-icon icon-height icon-color]
      (define color : FlRGBA (rgb* icon-color))
      (css-ref declared-values inherited-values property
               (hash-ref! cache (list default-icon icon-height color)
                          (λ [] (let ([make-icon (λ [] (default-icon #:color color #:height icon-height))])
                                  (make-css->bitmap icon-height make-icon))))))))

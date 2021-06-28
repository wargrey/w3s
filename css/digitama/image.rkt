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
(require "color.rkt")
(require "../recognizer.rkt")
(require "../color.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Image-Set-Option (List CSS-Image-Datum Flonum))
(define-type Image-Set-Options (Listof Image-Set-Option))
(define-type Linear-Color-Hint (U Flonum CSS-%)) ; <length-percentage>
(define-type Linear-Color-Stop (U CSS-Color-Datum (Pairof CSS-Color-Datum Linear-Color-Hint)))

(define-type CSS-Image-Datum (U CSS-Image String))

(define-css-value css-image #:as CSS-Image ())
(define-css-value image #:as Image #:=> css-image ([tag : (Option Symbol)] [content : CSS-Image-Datum] [fallback : Color]))
(define-css-value image-set #:as Image-Set #:=> css-image ([options : Image-Set-Options]))

(define-css-value css-gradient #:as CSS-Gradient #:=> css-image ())
(define-css-value linear-gradient #:as Linear-Gradient #:=> css-gradient
  ([direction : Flonum]
   [stop : Linear-Color-Stop]
   [stops : (Listof+ (U Linear-Color-Stop Linear-Color-Hint))]))

(define css-image-rendering-option : (Listof Symbol) '(auto crisp-edges pixelated))
(define css-image-fit-option : (Listof Symbol) '(fill contain cover none scale-down))
(define css-image-tag : (Listof Symbol) '(ltr rtl))
(define css-gradient-hside-option : (Listof Symbol) '(left right))
(define css-gradient-vside-option : (Listof Symbol) '(top bottom))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-css-function-filter <css-image-notation> #:-> CSS-Image
  ;;; https://drafts.csswg.org/css-images-4/#image-notation
  ;;; https://drafts.csswg.org/css-images-4/#image-set-notation
  [(image) #:=> [(image #false "" [fallback ? index? symbol? flcolor?])
                 (image #false [content ? css-image? string?] [fallback ? symbol? index? flcolor?])
                 (image [tag ? symbol? false?] "" [fallback ? symbol? index? flcolor?])
                 (image [tag ? symbol? false?] [content ? css-image? string?] [fallback ? symbol? index? flcolor?])]
   (CSS<&> (CSS:<*> (<css-keyword> css-image-tag) '?)
           (CSS<+> (CSS:<^> (<css-color>)) ; NOTE: both color and url accept strings, however their domains are not intersective.
                   (CSS<&> (CSS:<^> (CSS:<+> (<css-image>) (<css:string>)))
                           (CSS<$> (CSS<?> [(<css-comma>) (CSS:<^> (<css-color>))]) 'transparent))))]
  [(image-set) #:=> [(image-set [options ? image-set-options?])]
   (CSS<!> (CSS<#> (CSS<!> (CSS:<^> (list (CSS:<+> (<css:string>) (<css-image>)) (<css+resolution>))))))])

(define-css-function-filter <css-gradient-notation> #:-> CSS-Gradient
  ;;; https://drafts.csswg.org/css-images-4/#gradients
  [(linear-gradient) #:=> [(linear-gradient [direction ? flonum?] [stop ? linear-color-stop?] [stops ? color-stop-list?])
                           (linear-gradient (css-named-direction->degree 'bottom) [stop ? linear-color-stop?] [stops ? color-stop-list?])]
   (CSS<&> (CSS<?> [(<css-keyword:to>) (CSS<~> (CSS:<++> (<css-keyword> css-gradient-hside-option)
                                                                 (<css-keyword> css-gradient-vside-option))
                                               named-directions->degrees)]
                   [else (CSS:<*> (<css-angle>) '?)])
           (CSS<#> (<:color-stop:>) 1)
           (CSS<!> (CSS<#> (CSS<+> (<:color-stop:>)
                                   (CSS<&> (<:color-hint:>) (<:color-stop:>))) '+)))]
  #:where
  [(define (<:color-stop:>)
     (CSS<+> (CSS<~> (CSS:<&> (<css-color>) (<css:length-percentage>)) fold-color+position)
             (CSS:<&> (<css-color>))
             (CSS<~> (CSS:<&> (<css:length-percentage>) (<css-color>)) fold-position+color)))

   (define (<:color-hint:>)
     (CSS:<&> (<css:length-percentage>) (<css-comma>)))
   
   (define (named-directions->degrees [side-or-corners : (Listof Any)]) : (Listof Any)
     (define degrees : (Listof Nonnegative-Flonum) (map css-named-direction->degree side-or-corners))
     (cond [(or (null? degrees) (null? (cdr degrees))) degrees]
           [else (let*-values ([(c1 c2) (values (car degrees) (cadr degrees))]
                               [(sum) (+ c1 c2)])
                   (list (cond [(> (* c1 c2) 0.0) (* sum 0.5)]
                               [(= sum 90.0) 45.0]
                               [else 315.0])))]))

   (define (fold-color+position [atad : (Listof Any)]) : (Listof Any)
     (cons (cons (cadr atad) (car atad)) (cddr atad)))
   
   (define (fold-position+color [atad : (Listof Any)]) : (Listof Any)
     (cons (cons (car atad) (cadr atad)) (cddr atad)))])

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
                        [resolution : Flonum 0.0])
                       ([option (in-list (image-set-options img))])
               (define this-density : Flonum (cadr option))
               ; - resoltions should not duplicate, but if so, use the first one.
               ; - if there is no specific resolution, use the highest one.
               (if (or (= resolution the-density) (<= this-density resolution))
                   (values the-src resolution)
                   (values (car option) this-density))))
           (cond [(> density 0.0) (image->bitmap src density)]
                 [else the-invalid-bitmap])]
          [else the-invalid-bitmap])))

(define css->normalized-image : (All (racket) (-> (-> (CSS-Maybe Bitmap) racket) (CSS->Racket racket)))
  (lambda [normalize]
    (λ [_ image]
      (cond [(bitmap? image) (normalize image)]
            [(css-image-datum? image) (normalize (image->bitmap image))]
            [else (normalize css:initial)]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define css-named-direction->degree : (->* (Any) (Symbol) Nonnegative-Flonum)
  (lambda [p [fallback 'bottom]]
    (case p
      [(top) 0.0]
      [(right) 90.0]
      [(bottom) 180.0]
      [(left) 270.0]
      [else (css-named-direction->degree 'bottom)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define css-image-datum? : (-> Any Boolean : CSS-Image-Datum)
  (lambda [v]
    (or (css-image? v)
        (string? v))))

(define image-set-option? : (-> Any Boolean : Image-Set-Option)
  (lambda [v]
    (and (pair? v)
         (css-image-datum? (car v))
         (pair? (cdr v))
         (flonum? (cadr v))
         (null? (cddr v)))))

(define image-set-options? : (-> Any Boolean : Image-Set-Options)
  (lambda [v]
    (and (list? v)
         (andmap image-set-option? v))))

(define linear-color-hint? : (-> Any Boolean : Linear-Color-Hint)
  (lambda [v]
    (or (flonum? v)
        (css-%? v))))

(define linear-color-stop? : (-> Any Boolean : Linear-Color-Stop)
  (lambda [v]
    (or (css-color-datum? v)
        (and (pair? v)
             (css-color-datum? (car v))
             (linear-color-hint? (cdr v))))))

(define linear-stop-or-hint? :  (-> Any Boolean : (U Linear-Color-Stop Linear-Color-Hint))
  (lambda [v]
    (or (linear-color-stop? v)
        (linear-color-hint? v))))

(define color-stop-list? : (-> Any Boolean : (Listof+ (U Linear-Color-Stop Linear-Color-Hint)))
  (lambda [datum]
    (and (list? datum)
         (pair? datum)
         (andmap linear-stop-or-hint? datum))))

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
(define-type Linear-Color-Stop (Pairof CSS-Color-Datum CSS-Flonum-%))
(define-type Linear-Color-Stops (Pairof Linear-Color-Stop (Listof+ Linear-Color-Stop)))
(define-type Radial-Elliptical-Size (U Nonnegative-Flonum CSS+%))
(define-type Radial-Size (U Symbol Nonnegative-Flonum (Pairof Radial-Elliptical-Size Radial-Elliptical-Size)))

(define-type CSS-Image-Datum (U CSS-Image String))

(define-css-value css-image #:as CSS-Image ())
(define-css-value image #:as Image #:=> css-image ([tag : (Option Symbol)] [content : CSS-Image-Datum] [fallback : Color]))
(define-css-value image-set #:as Image-Set #:=> css-image ([options : Image-Set-Options]))

(define-css-value css-gradient #:as CSS-Gradient #:=> css-image ())
(define-css-value linear-gradient #:as Linear-Gradient #:=> css-gradient ([direction : Flonum] [stops : Linear-Color-Stops] [repeat? : Boolean]))
(define-css-value radial-gradient #:as Radial-Gradient #:=> css-gradient ([shape : Symbol] [size : Radial-Size] [stops : Linear-Color-Stops] [repeat? : Boolean]))

(define css-image-rendering-option : (Listof Symbol) '(auto crisp-edges pixelated))
(define css-image-fit-option : (Listof Symbol) '(fill contain cover none scale-down))
(define css-image-tag : (Listof Symbol) '(ltr rtl))
(define css-gradient-hside-option : (Listof Symbol) '(left right))
(define css-gradient-vside-option : (Listof Symbol) '(top bottom))
(define css-gradient-ending-shape : (Listof Symbol) '(circle ellipse))
(define css-gradient-size : (Listof Symbol) '(farthest-corner farthest-side closest-corner closest-side))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-css-function-filter <css-image-notation> #:-> CSS-Image
  ;;; https://drafts.csswg.org/css-images-4/#image-notation
  ;;; https://drafts.csswg.org/css-images-4/#image-set-notation
  [(image) #:=> [(image #false "" [fallback ? index? symbol? flcolor?])
                 (image #false [content ? css-image? string?] [fallback ? symbol? index? flcolor?])
                 (image [tag ? symbol? false?] "" [fallback ? symbol? index? flcolor?])
                 (image [tag ? symbol? false?] [content ? css-image? string?] [fallback ? symbol? index? flcolor?])]
   (CSS<&> ((inst CSS:<*> Any) (<css-keyword> css-image-tag) '?)
           (CSS<+> ((inst CSS:<^> Any) (<css-color>)) ; NOTE: both color and url accept strings, however their domains are not intersective.
                   (CSS<&> ((inst CSS:<^> Any) (CSS:<+> (<css-image>) (<css:string>)))
                           (CSS<$> (CSS<?> [(<css-comma>) ((inst CSS:<^> Any) (<css-color>))]) 'transparent))))]
  [(image-set) #:=> [(image-set [options ? image-set-options?])]
   (CSS<!> (CSS<#> (CSS<!> (CSS:<^> (list (CSS:<+> (<css:string>) (<css-image>)) (<css+resolution>))))))])

(define-css-function-filter <css-gradient-notation> #:-> CSS-Gradient
  ;;; https://drafts.csswg.org/css-images-4/#gradients
  [(linear-gradient) #:=> [(linear-gradient [direction ? flonum?] [stops ? color-stop-list?] #false)
                           (linear-gradient (css-named-direction->degree 'bottom) [stops ? color-stop-list?] #false)]
   (CSS<&> (<:angle-or-direction:>) (<:color-stop-list:>))]
  [(repeating-linear-gradient) #:=> [(linear-gradient [direction ? flonum?] [stops ? color-stop-list?] #true)
                                     (linear-gradient (css-named-direction->degree 'bottom) [stops ? color-stop-list?] #true)]
   (CSS<&> (<:angle-or-direction:>) (<:color-stop-list:>))]
  #:where
  [(define (<:angle-or-direction:>)
     (css-comma-followed-parser
      (CSS<?> [(<css-keyword:to>) (CSS<~> (CSS:<++> (<css-keyword> css-gradient-hside-option)
                                                    (<css-keyword> css-gradient-vside-option))
                                          named-directions->degrees
                                          null)]
              [else (CSS:<*> (<css-angle>) '?)])))
   
   (define (<:color-stop+comma:>)
     (css-comma-followed-parser
      ;; NOTE: it's much more efficient to parse <length-percentage> than to parse <css-color>, hence the `fold-color+maybe-position`
      (CSS<+> (CSS<~> (CSS:<&> (<css-length-percentage>) (<css-color>)) fold-position+color)
              (CSS<~> (CSS<&> (CSS:<^> (<css-color>)) (CSS:<*> (<css-length-percentage>) '?)) fold-color+maybe-position))))
   
   (define (<:color-hint+comma:>)
     (CSS<*> (CSS<&> (CSS:<^> (CSS:<~> (<css-length-percentage>) linear-hint->fake-stop)) (<:css-skip-comma:>)) '?))

   (define (<:color-stop-list:>)
     (CSS<!> (CSS<&> (<:color-stop+comma:>)
                     (CSS<*> (CSS<&> (<:color-hint+comma:>) (<:color-stop+comma:>)) '*))))

   (define (linear-hint->fake-stop [h : CSS-Flonum-%]) : Linear-Color-Stop
     (cons 'hint-only h))

   (define (fold-color+maybe-position [atad : (Listof (U CSS-Color-Datum CSS-Flonum-%))]) : Any
     ; NOTE: `atad` are data in reversed order
     (cond [(null? (cdr atad)) (cons (car atad) +nan.0)]
           [else (cons (cadr atad) (car atad))]))
   
   (define (fold-position+color [atad : (Listof (U CSS-Color-Datum CSS-Flonum-%))]) : Any
     ; NOTE: `atad` are data in reversed order
     (cons (car atad) (cadr atad)))

   (define (named-directions->degrees [side-or-corners : (Listof Symbol)]) : Flonum
     (define degrees : (Listof Nonnegative-Flonum) (map css-named-direction->degree side-or-corners))
     (cond [(null? degrees) +nan.0]
           [(null? (cdr degrees)) (car degrees)]
           [else (let*-values ([(c1 c2) (values (car degrees) (cadr degrees))]
                               [(sum) (+ c1 c2)])
                   (cond [(> (* c1 c2) 0.0) (* sum 0.5)]
                         [(= sum 90.0) 45.0]
                         [else 315.0]))]))])

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

(define linear-color-stop? : (-> Any Boolean : Linear-Color-Stop)
  (lambda [v]
    (and (pair? v)
         (css-color-datum? (car v))
         (css-flonum-%? (cdr v)))))

(define color-stops-or-hints? : (-> (Listof Any) Boolean : (Listof+ Linear-Color-Stop))
  (lambda [datum]
    (and (pair? datum)
         (andmap linear-color-stop? datum))))

(define color-stop-list? : (-> Any Boolean : Linear-Color-Stops)
  (lambda [datum]
    (and (list? datum)
         (pair? datum)
         (linear-color-stop? (car datum))
         (color-stops-or-hints? (cdr datum)))))

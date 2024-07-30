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

(require digimon/predicate)

(require "syntax/digicore.rkt")
(require "syntax/dimension.rkt")
(require "syntax/misc.rkt")
(require "color.rkt")
(require "../recognizer.rkt")
(require "../color.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Image-Set-Option (List CSS-Image-Datum Flonum))
(define-type Image-Set-Options (Listof Image-Set-Option))
(define-type Linear-Color-Stop (Pairof CSS-Color-Datum (Listof CSS-Flonum-%)))
(define-type Linear-Color-Stops (Pairof Linear-Color-Stop (Listof+ Linear-Color-Stop)))
(define-type Radial-Size (U Symbol Nonnegative-Flonum (Pairof (U Nonnegative-Flonum Nonnegative-FlPercentage) (U Nonnegative-Flonum Nonnegative-FlPercentage))))
(define-type Radial-Shape (Pairof Symbol Radial-Size))

(define-type CSS-Image-Datum (U CSS-Image String))

(define-css-value css-image #:as CSS-Image ())
(define-css-value image #:as Image #:=> css-image ([tag : (Option Symbol)] [content : CSS-Image-Datum] [fallback : Color]))
(define-css-value image-set #:as Image-Set #:=> css-image ([options : Image-Set-Options]))

(define-css-value css-gradient #:as CSS-Gradient #:=> css-image ())
(define-css-value linear-gradient #:as Linear-Gradient #:=> css-gradient
  ([direction : Flonum] [stops : Linear-Color-Stops] [repeat? : Boolean]))
(define-css-value radial-gradient #:as Radial-Gradient #:=> css-gradient
  ([shape : Radial-Shape] [center : CSS-Position] [stops : Linear-Color-Stops] [repeat? : Boolean]))
(define-css-value conic-gradient #:as Conic-Gradient #:=> css-gradient
  ([angle : Flonum] [center : CSS-Position] [stops : Linear-Color-Stops] [repeat? : Boolean]))

(define css-image-rendering-option : (Listof Symbol) '(auto crisp-edges pixelated))
(define css-image-fit-option : (Listof Symbol) '(fill contain cover none scale-down))
(define css-image-tag : (Listof Symbol) '(ltr rtl))
(define css-gradient-hside-option : (Listof Symbol) '(left right))
(define css-gradient-vside-option : (Listof Symbol) '(top bottom))
(define css-gradient-ending-shape : (Listof Symbol) '(circle ellipse))
(define css-gradient-extent-size : (Listof Symbol) '(farthest-corner farthest-side closest-corner closest-side))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-css-function-filter <css-image-notation> #:-> CSS-Image
  ;;; https://drafts.csswg.org/css-images-4/#image-notation
  ;;; https://drafts.csswg.org/css-images-4/#image-set-notation
  [(image)
   #:=> [(image #false "" [fallback ? index? symbol? flcolor?])
         (image #false [content ? css-image? string?] [fallback ? symbol? index? flcolor?])
         (image [tag ? symbol? false?] "" [fallback ? symbol? index? flcolor?])
         (image [tag ? symbol? false?] [content ? css-image? string?] [fallback ? symbol? index? flcolor?])]
   (CSS<&> ((inst CSS:<*> Any) (<css-keyword> css-image-tag) '?)
           (CSS<+> ((inst CSS:<^> Any) (<css-color>)) ; NOTE: both color and url accept strings, however their domains are not intersective.
                   (CSS<&> ((inst CSS:<^> Any) (CSS:<+> (<css-image>) (<css:string>)))
                           (CSS<$> (CSS<?> [(<css-comma>) ((inst CSS:<^> Any) (<css-color>))]) 'transparent))))]
  [(image-set)
   #:=> [(image-set [options ? image-set-options?])]
   (CSS<!> (CSS<#> (CSS<!> (CSS:<^> (list (CSS:<+> (<css:string>) (<css-image>)) (<css+resolution>))))))])

(define-css-function-filter <css-gradient-notation> #:-> CSS-Gradient
  #:with [[<:css-color:> : (CSS-Parser (Listof Any)) (CSS:<^> (<css-color>))]]
  ;;; https://drafts.csswg.org/css-images-4/#gradients
  [(linear-gradient)
   #:=> [(linear-gradient [direction ? flonum?] [stops ? linear-color-stop-list?] #false)
         (linear-gradient (css-named-direction->degree 'bottom) [stops ? linear-color-stop-list?] #false)]
   (CSS<&> (<:angle-or-direction:>) (<:css-length-color-stop-list:>))]
  [(repeating-linear-gradient)
   #:=> [(linear-gradient [direction ? flonum?] [stops ? linear-color-stop-list?] #true)
         (linear-gradient (css-named-direction->degree 'bottom) [stops ? linear-color-stop-list?] #true)]
   (CSS<&> (<:angle-or-direction:>) (<:css-length-color-stop-list:>))]
  [(radial-gradient)
   #:=> [(radial-gradient [shape ? radial-shape?] [center ? css-position?] [stops ? linear-color-stop-list?] #false)
         (radial-gradient default-shape [center ? css-position?] [stops ? linear-color-stop-list?] #false)
         (radial-gradient [shape ? radial-shape?] css-center-position [stops ? linear-color-stop-list?] #false)
         (radial-gradient default-shape css-center-position [stops ? linear-color-stop-list?] #false)]
   (CSS<&> (<:ending-shape+comma:>) (<:css-length-color-stop-list:>))]
  [(repeating-radial-gradient)
   #:=> [(radial-gradient [shape ? radial-shape?] [center ? css-position?] [stops ? linear-color-stop-list?] #false)
         (radial-gradient default-shape [center ? css-position?] [stops ? linear-color-stop-list?] #false)
         (radial-gradient [shape ? radial-shape?] css-center-position [stops ? linear-color-stop-list?] #false)
         (radial-gradient default-shape css-center-position [stops ? linear-color-stop-list?] #false)]
   (CSS<&> (<:ending-shape+comma:>) (<:css-length-color-stop-list:>))]
  [(conic-gradient)
   #:=> [(conic-gradient [angle ? flonum?] [center ? css-position?] [stops ? linear-color-stop-list?] #false)
         (conic-gradient default-angle [center ? css-position?] [stops ? linear-color-stop-list?] #false)
         (conic-gradient [angle ? flonum?] css-center-position [stops ? linear-color-stop-list?] #false)
         (conic-gradient default-angle css-center-position [stops ? linear-color-stop-list?] #false)]
   (CSS<&> (<:from-angle+comma:>) (<:css-angle-color-stop-list:>))]
  [(repeating-conic-gradient)
   #:=> [(conic-gradient [angle ? flonum?] [center ? css-position?] [stops ? linear-color-stop-list?] #true)
         (conic-gradient default-angle [center ? css-position?] [stops ? linear-color-stop-list?] #true)
         (conic-gradient [angle ? flonum?] css-center-position [stops ? linear-color-stop-list?] #true)
         (conic-gradient default-angle css-center-position [stops ? linear-color-stop-list?] #true)]
   (CSS<&> (<:from-angle+comma:>) (<:css-angle-color-stop-list:>))]
  #:where
  [(define default-shape : Radial-Shape (cons 'ellipse 'farthest-corner))
   (define default-angle : Flonum 0.0)
   
   (define (<:css-length-color-stop-list:>) (<:css-color-stop-list:> <:css-color:> (<css-length-percentage>)))
   (define (<:css-angle-color-stop-list:>) (<:css-color-stop-list:> <:css-color:> (<css-angle-percentage>)))

   (define (<:angle-or-direction:>)
     (css-comma-followed-parser
      (CSS<?> [(<css-keyword:to>) (CSS<~> (CSS:<++> (<css-keyword> css-gradient-hside-option)
                                                    (<css-keyword> css-gradient-vside-option))
                                          css-named-directions->degrees)]
              [else (CSS:<*> (<css-angle>) '?)])))

   (define (<:ending-shape+comma:>)
     (css-comma-followed-parser
      (CSS<&> (CSS<*> (CSS<+> (CSS<~> ((inst CSS<++> (Listof (U Symbol CSS+Flonum-%)))
                                       (CSS:<^> (<css-keyword> 'ellipse)) (CSS:<*> (<css+length-percentage>) 2))
                                      fold-ellipse)
                              (CSS<~> (CSS:<++> (<css-keyword> 'circle) (<css+length>))
                                      fold-circle)
                              (CSS<~> (CSS:<++> (<css-keyword> css-gradient-ending-shape) (<css-keyword> css-gradient-extent-size))
                                      fold-keyword-shape))
                      '?)
              (CSS<*> (CSS<?> [(<css-keyword:at>) (CSS<^> (<:css-position:>))]) '?))))

   (define (<:from-angle+comma:>)
     (css-comma-followed-parser
      (CSS<&> (CSS<*> (CSS<?> [(<css-keyword:from>) (CSS:<^> (<css:angle>))]) '?)
              (CSS<*> (CSS<?> [(<css-keyword:at>) (CSS<^> (<:css-position:>))]) '?))))

   (define (fold-circle [data : (Listof (U Symbol Nonnegative-Flonum))]) : Any
     (cons 'circle
           (let ([s (filter flonum? data)])
             (if (null? s) (cdr default-shape) (car s)))))

   (define (fold-ellipse [data : (Listof (U Symbol CSS+Flonum-%))]) : Any
     (cons 'ellipse
           (let ([s (filter-not symbol? data)])
             (if (or (null? s) (null? (cdr s)))
                 (cdr default-shape)
                 (cons (car s) (cadr s))))))

   (define (fold-keyword-shape [data : (Listof Symbol)]) : Any
     (cond [(null? data) '#:deadcode default-shape]
           [else (let ([kws (remove* css-gradient-ending-shape data)])
                   (cond [(null? kws) (cons (car data) (cdr default-shape))]
                         [else (cons (car default-shape) (car kws))]))]))])

(define-css-disjoint-filter <css-image> #:-> CSS-Image-Datum
  ;;; https://drafts.csswg.org/css-images/#image-values
  ;;; https://drafts.csswg.org/css-images/#invalid-image
  (<css-image-notation>)
  (<css:url>))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define <:css-color-stop-list:> : (All (c) (-> (CSS-Parser (Listof c)) (CSS:Filter CSS-Flonum-%) (CSS-Parser (Listof Any))))
  (let ()
    (define #:forall (c) (fold-color+positions [data : (Listof (U c CSS-Flonum-%))]) : Any
      ; yes, it's nothing but another `identity` function
      ; please let `CSS<~>` isolate these `color stop`s from preceding color stops and hints
      data)
    
    (define #:forall (c) (fold-positions+color [data : (Listof (U c CSS-Flonum-%))]) : Any
      (let-values ([(positions color) (split-at-right data 1)])
        (cons (car color) positions)))

    (define #:forall (c) (<:color-stop:> [<:color:> : (CSS-Parser (Listof c))] [<number%> : (CSS:Filter CSS-Flonum-%)]) : (CSS-Parser (Listof Any))
      (css-comma-followed-parser
       ;; NOTE: it's much more efficient to parse <length-percentage> than to parse <css-color>, hence the `fold-color+maybe-position`
       (CSS<+> ((inst CSS<~> (U c CSS-Flonum-%) Any) (CSS<&> (CSS:<*> <number%> '(1 . 2)) <:color:>) fold-positions+color)
               ((inst CSS<~> (U c CSS-Flonum-%) Any) (CSS<&> <:color:> (CSS:<*> <number%> '(0 . 2))) fold-color+positions))))

    (define (interpolation-hint->fake-stop [h : CSS-Flonum-%]) : Linear-Color-Stop
      (cons 'hint-only (list h)))
    
    (lambda [<:color:> <number%>]
      (let ([<:color-stop+comma:> (<:color-stop:> <:color:> <number%>)])
        (CSS<!> (CSS<&> <:color-stop+comma:>
                        (CSS<*> (CSS<&> (css-comma-followed-parser (CSS:<*> (CSS:<~> <number%> interpolation-hint->fake-stop) '?))
                                        <:color-stop+comma:>) '*)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define make-image-normalizer : (case-> [Nonnegative-Real Positive-Flonum (-> Bitmap) -> (-> (CSS-Maybe Bitmap) Bitmap)]
                                        [Nonnegative-Real Positive-Flonum -> (-> (CSS-Maybe Bitmap) (CSS-Maybe Bitmap))])
  (let ([normalize (λ [[h : Nonnegative-Real] [d : Positive-Flonum] [b : Bitmap]]
                     (bitmap-scale (bitmap-alter-density b d) (/ h (bitmap-intrinsic-height b))))])
    (case-lambda
      [(height density)
       (λ [[raw : (CSS-Maybe Bitmap)]]
         (if (bitmap? raw) (normalize height density raw) css:initial))]
      [(height density mk-image)
       (λ [[raw : (CSS-Maybe Bitmap)]]
         (cond [(bitmap? raw) (normalize height density raw)]
               [else (let ([alt-image : Bitmap (mk-image)])
                       (cond [(> (bitmap-intrinsic-height alt-image) height) (normalize height density alt-image)]
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
   
(define css-named-directions->degrees : (-> (Listof Symbol) Flonum)
  (lambda [side-or-corners]
    (define degrees : (Listof Nonnegative-Flonum) (map css-named-direction->degree side-or-corners))
    (cond [(null? degrees) +nan.0]
          [(null? (cdr degrees)) (car degrees)]
          [else (let*-values ([(c1 c2) (values (car degrees) (cadr degrees))]
                              [(sum) (+ c1 c2)])
                  (cond [(> (* c1 c2) 0.0) (* sum 0.5)]
                        [(= sum 90.0) 45.0]
                        [else 315.0]))])))

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
    (listof? v image-set-option?)))

(define linear-color-stop? : (-> Any Boolean : Linear-Color-Stop)
  (lambda [v]
    (and (pair? v)
         (css-color-datum? (car v))
         ((inst listof? CSS-Flonum-%) (cdr v) css-flonum-%?))))

(define linear-color-stop-list? : (-> Any Boolean : Linear-Color-Stops)
  (lambda [datum]
    (and (list? datum)
         (pair? datum)
         (linear-color-stop? (car datum))
         ((inst listof+? Linear-Color-Stop) (cdr datum) linear-color-stop?))))

(define radial-shape? : (-> Any Boolean : #:+ Radial-Shape)
  (lambda [v]
    (and (pair? v)
         (symbol? (car v))
         (radial-size? (cdr v)))))

(define radial-size? : (-> Any Boolean : #:+ Radial-Size)
  (lambda [v]
    (or (symbol? v)
        (nonnegative-flonum? v)
        (and (pair? v)
             (css+flonum-%? (car v))
             (css+flonum-%? (cdr v))))))

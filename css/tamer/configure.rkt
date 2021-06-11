#lang typed/racket/gui

(provide (all-defined-out) time*)

(require digimon/debug)

(require "../syntax.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define DrRacket? : Boolean (regexp-match? #px"DrRacket$" (find-system-path 'run-file)))

(define css-configure-@media : (-> Void)
  (lambda []
    (define-values (width height) (get-display-size))
    (css-deprecate-media-type #true)
    (default-css-media-type 'screen)
    (default-css-media-features
      (make-css-media-features #:width (fx->fl (or width 0))
                               #:height (fx->fl (or height 0))
                               #:resolution (real->double-flonum (or (get-display-backing-scale) 1.0))
                               #:update 'fast
                               #:overflow-block 'scroll
                               #:overflow-inline 'scroll
                               #:color (get-display-depth)
                               #:pointer 'fine #:any-pointer 'fine
                               #:hover 'hover #:any-hover 'hover))))

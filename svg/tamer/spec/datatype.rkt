#lang typed/racket/base

(require digimon/spec)
(require digimon/format)

(require pangocairo/color)

(require sgml/digitama/digicore)
(require sgml/digitama/tokenizer)
(require sgml/digitama/grammar)

(require svg/digitama/grammar/datatype)

(require (for-syntax racket/base))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define open-tamer-input : (-> (U String (Listof Char)) Input-Port)
  (lambda [stream.svg]
    (define /dev/svgin : Input-Port
      (open-input-string
       (cond [(string? stream.svg) (string-append "\"" stream.svg "\"")]
             [else (apply string (append (list #\") stream.svg (list #\")))])))

    (port-count-lines! /dev/svgin)
    /dev/svgin))

(define tamer-tokens : (All (a) (-> String (-> XML-Element-Attribute-Value* (XML-Option a)) (U (XML-Option a) Void)))
  (lambda [src svg:attr-value*->datum]
    (define /dev/svgin (open-tamer-input src))
    (define-values (token evn) (xml-consume-token* /dev/svgin '/dev/svgin #false))

    (when (xml:string? token)
      (or (svg:attr-value*->datum token)
          (make+exn:svg:range token)))))

(define tamer-datum : (-> Any Any)
  (lambda [c]
    (cond [(procedure? c) (object-name c)]
          [(string? c) (string->quoted-symbol c)]
          [else c])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax (do-check-parser stx)
  (syntax-case stx [:]
    [(_ datum-expr expected-datum)
     (syntax/loc stx
       (let ([datum datum-expr])
         (cond [(procedure? expected-datum) (expect-satisfy (cast expected-datum (-> Any Boolean)) datum)]
               [(not expected-datum) (expect-satisfy exn:xml? datum)]
               [else (expect-equal datum expected-datum)])))]))

(define-behavior (it-check-parser/error src.svg svg-attr->datum expected-datum logsrc expected-message)
  #:it
  ["should be parsed into `~a`, with logging message matching ~a, when fed with `~a`" (tamer-datum expected-datum) (tamer-datum expected-message) src.svg]
  #:when expected-datum
  ["should be parsed into an error matching ~a, when fed with `~a`" (tamer-datum expected-message) src.svg]
  #:do
  (expect-log-message* logsrc expected-message
                       (λ [] (tamer-tokens src.svg svg-attr->datum))
                       (λ [[results : (List* Any Any)]] (do-check-parser (car results) expected-datum))))

(define-behavior (it-check-parser src.svg svg-attr->datum expected-values)
  #:it
  ["should be parsed into `~a`, when fed with `~a`" (tamer-datum expected-values) src.svg]
  #:do
  (do-check-parser (tamer-tokens src.svg svg-attr->datum) expected-values))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  (require digimon/dtrace)
  
  (current-logger /dev/dtrace)
  (default-xml-error-topic 'exn:svg:syntax)

  (define logsrc : Log-Receiver (make-log-receiver /dev/dtrace 'warning (default-xml-error-topic)))
  
  (spec-begin SVG-Data-Type #:do
              (describe "Color" #:do
                (it-check-parser "#def" svg:attr-value*->color (hexa #xDDEEFF 1.0))
                (it-check-parser "#123456" svg:attr-value*->color (hexa #x123456 1.0))
                (it-check-parser "GhostWhite" svg:attr-value*->color (rgb* 'ghostwhite))
                (it-check-parser "rgb(255, -10, 0)" svg:attr-value*->color (rgb* #xFF0000))
                (it-check-parser "rgb(110%, -10%, 0%)" svg:attr-value*->color (rgb* #xFF0000))
                (it-check-parser/error "#ABCD" svg:attr-value*->color #false logsrc exn:svg:digit?)
                (it-check-parser/error "rgb(110%, -10, 0%)" svg:attr-value*->color #false logsrc exn:svg:malformed?)
                (it-check-parser/error "hsv(30%, 100%, 100%)" svg:attr-value*->color #false logsrc exn:svg:function?)
                (it-check-parser/error "rgb(30%, 100%, 100%, 100%)" svg:attr-value*->color #false logsrc exn:svg:malformed?)
                (it-check-parser/error "rgb(120, 100%, 100%)" svg:attr-value*->color #false logsrc exn:svg:malformed?))

              (describe "ICC Color" #:do
                (it-check-parser "icc-color(name, 1.0, 1.0, 1.0)" svg:attr-value*->icc-color (svg-icccolor 'name (list 1.0 1.0 1.0)))
                (it-check-parser/error "icc-color(n(ame), 2, 4, 6)" svg:attr-value*->icc-color #false logsrc exn:svg:malformed?)
                (it-check-parser/error "icc-color(name, 2 4 6)" svg:attr-value*->icc-color (svg-icccolor 'name (list 2.0 4.0 6.0)) logsrc exn:svg:missing-comma?)
                (it-check-parser/error "icc-color(name)" svg:attr-value*->icc-color #false logsrc exn:svg:malformed?)
                (it-check-parser/error "icccolor(name, 2)" svg:attr-value*->icc-color #false logsrc exn:svg:function?))

              (describe "Paint" #:do
                (it-check-parser "inherit" svg:attr-value*->paint 'inherit)
                (it-check-parser "none" svg:attr-value*->paint 'none)
                (it-check-parser "currentColor" svg:attr-value*->paint 'currentColor)
                (it-check-parser "currentcolor" svg:attr-value*->paint (rgb* 'currentcolor))
                (it-check-parser "Snow" svg:attr-value*->paint (rgb* 'snow))
                (it-check-parser "#def icc-color(rgb, 0.25, 0.5, 0.75)" svg:attr-value*->paint (cons (hexa #xDDEEFF 1.0) (svg-icccolor 'rgb (list 0.25 0.50 0.75))))
                (it-check-parser "url(#svg) royalblue icc-color(rgb, 0.618)" svg:attr-value*->paint
                                 (svg-paint-server "#svg" (cons (rgb* 'royalblue) (svg-icccolor 'rgb (list 0.618)))))
                (it-check-parser/error "#svg" svg:attr-value*->paint #false logsrc exn:svg:unrecognized?)
                (it-check-parser/error "#svg none icc-color(rgb, 0.382)" svg:attr-value*->paint #false logsrc exn:svg:range?))

              (describe "Transform" #:do
                (it-check-parser "matrix(1, 2, 3 , 4 5 6)" svg:attr-value*->transform-list (list (svg:matrix 1.0 2.0 3.0 4.0 5.0 6.0)))
                (it-check-parser "skewX(30), skewY(-30)" svg:attr-value*->transform-list (list (svg:skewX 30.0) (svg:skewY -30.0)))
                (it-check-parser "translate(-10,-20) scale(2) rotate(4.5E1) translate(5)" svg:attr-value*->transform-list
                                 (list (svg:translate -10.0 -20.0) (svg:scale 2.0 #false) (svg:rotate 45.0 #false #false) (svg:translate 5.0 #false)))
                (it-check-parser/error "rotate(1.2e2, 2)" svg:attr-value*->transform-list null logsrc exn:svg:malformed?)
                (it-check-parser/error "skew(45)" svg:attr-value*->transform-list null logsrc exn:svg:function?))

              (describe "List" #:do
                (it-check-parser "12, 34" svg:attr-value*->integer-pair (cons 12 34))
                (it-check-parser "56" svg:attr-value*->integer-pair 56)
                (it-check-parser/error "7, 8, 9" svg:attr-value*->number-pair (cons 7 8) logsrc exn:svg:malformed?)
                (it-check-parser/error "1 0" svg:attr-value*->number-pair (cons 1 0) logsrc exn:svg:missing-comma?)
                (it-check-parser/error "1em, 2ch, 3.0pt, 4.0in" svg:attr-value*->length-list '((1.0 . em) (2.0 . ch) (3.0 . pt) (4.0 . in)) logsrc exn:svg:unit?))

              (describe "Dimension and Coordinate" #:do
                (it-check-parser "12.56%" svg:attr-value*->dim:length (cons 12.56 '%))
                (it-check-parser "5em" svg:attr-value*->coordinate (cons 5.0 'em))
                (it-check-parser/error "1MHz" svg:attr-value*->dim:frequency #false logsrc exn:svg:unit?)
                (it-check-parser/error "1hz" svg:attr-value*->dim:frequency (cons 1.0 'hz) logsrc exn:svg:unit?))

              (describe "IRI" #:do
                (it-check-parser "https://gyoudmon.org#svg" svg:attr-value*->IRI "https://gyoudmon.org#svg")
                (it-check-parser "url (https://gyoudmon.org#svg)" svg:attr-value*->IRI "https://gyoudmon.org#svg")
                (it-check-parser "url()" svg:attr-value*->IRI "")
                (it-check-parser "#relative" svg:attr-value*->IRI "#relative")
                (it-check-parser/error "uri(https://gyoudmon.org)" svg:attr-value*->IRI #false logsrc exn:svg:function?))))

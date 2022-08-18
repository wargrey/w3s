#lang typed/racket/base

(require digimon/spec)
(require digimon/format)

(require bitmap/color)

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
               [(not expected-datum) (expect-satisfy exn:xml? datum)])))]))

(define-behavior (it-check-parser/error src.svg svg-attr->datum expected-datum logsrc expected-message)
  #:it
  ["should be parsed into ~a, with logging message matching ~a, when fed with `~a`" (tamer-datum expected-datum) (tamer-datum expected-message) src.svg]
  #:when expected-datum
  ["should be parsed into an error matching ~a, when fed with `~a`" (tamer-datum expected-message) src.svg]
  #:do
  (expect-log-message* logsrc expected-message
                       (λ [] (tamer-tokens src.svg svg-attr->datum))
                       (λ [[results : (List* Any Any)]] (do-check-parser (car results) expected-datum))))

(define-behavior (it-check-parser src.svg svg-attr->datum expected-values)
  #:it
  ["should be parsed into ~a, when fed with `~a`" (tamer-datum expected-values) src.svg]
  #:do
  (do-check-parser (tamer-tokens src.svg svg-attr->datum) expected-values))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  (require digimon/dtrace)
  
  (define logsrc : Log-Receiver (make-log-receiver /dev/dtrace 'warning 'exn:svg:syntax))

  (current-logger /dev/dtrace)
  (default-xml-error-topic 'exn:svg:syntax)

  (spec-begin SVG-Data-Type #:do
              (describe "Color" #:do
                (it-check-parser "#ABC" svg:attr-value*->color (hexa #xAABBCC 1.0))
                (it-check-parser "#123456" svg:attr-value*->color (hexa #x123456 1.0))
                (it-check-parser "GhostWhite" svg:attr-value*->color (rgb* 'ghostwhite))
                (it-check-parser "rgb(255, -10, 0)" svg:attr-value*->color (rgb* #xFF0000))
                (it-check-parser "rgb(110%, -10%, 0%)" svg:attr-value*->color (rgb* #xFF0000))
                (it-check-parser/error "#ABCD" svg:attr-value*->color #false logsrc exn:svg:digit?)
                (it-check-parser/error "rgb(110%, -10, 0%)" svg:attr-value*->color #false logsrc exn:svg:malformed?)
                (it-check-parser/error "hsv(30%, 100%, 100%)" svg:attr-value*->color #false logsrc exn:svg:function?)
                (it-check-parser/error "rgb(30%, 100%, 100%, 100%)" svg:attr-value*->color #false logsrc exn:svg:malformed?)
                (it-check-parser/error "rgb(120, 100%, 100%)" svg:attr-value*->color #false logsrc exn:svg:malformed?))

              (describe "Number and Optional Number Pair" #:do
                (it-check-parser "12, 34" svg:attr-value*->integer-pair (cons 12 34))
                (it-check-parser "56" svg:attr-value*->integer-pair 56)
                (it-check-parser/error "7, 8, 9" svg:attr-value*->integer-pair (cons 7 8) logsrc exn:svg:malformed?)
                (it-check-parser/error "1 0" svg:attr-value*->integer-pair #false logsrc exn:svg:malformed?))

              (describe "Dimension and Coordinate" #:do
                (it-check-parser "12.56%" svg:attr-value*->dim:length (cons 12.56 '%))
                (it-check-parser "5em" svg:attr-value*->coordinate (cons 5.0 'em))
                (it-check-parser/error "1MHz" svg:attr-value*->dim:frequency #false logsrc exn:svg:unit?)
                (it-check-parser/error "1hz" svg:attr-value*->dim:frequency (cons 1.0 'hz) logsrc exn:svg:unit?))))

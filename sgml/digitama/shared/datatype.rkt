#lang typed/racket/base

(provide (all-defined-out))

(require racket/string)
(require racket/format)
(require racket/symbol)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type XML-Boolean (U 'true 'false))
(define-type XML-Dimension (Pairof Flonum Symbol))
(define-type XML-Nonnegative-Dimension (Pairof Nonnegative-Flonum Symbol))
(define-type XML-Percentage (Pairof Flonum '%))
(define-type XML-Nonnegative-Percentage (Pairof Nonnegative-Flonum '%))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml:attr-datum->value : (-> Any String)
  (lambda [v]
    (cond [(string? v) v]
          [(symbol? v) (symbol->immutable-string v)]
          [(and (integer? v) (inexact? v)) (number->string (inexact->exact v))]
          [(number? v) (number->string v)]
          [(list? v) (string-join (map xml:attr-datum->value v))]
          [else (~a v)])))

(define xml:attr-hexdecimal->value : (-> Integer String)
  (lambda [v]
    (number->string v 16)))

(define xml:attr-dimension->value : (-> (Pairof Real Symbol) String)
  (lambda [v]
    (string-append (number->string (car v))
                   (symbol->immutable-string (cdr v)))))

(define xml:attr-listof-type->value : (All (T) (->* ((Listof T) (-> T String)) (String) String))
  (lambda [vs datum->value [sep ", "]]
    (string-join (map datum->value vs) sep)))

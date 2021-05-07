#lang typed/racket/base

(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Xexpr-Attribute (Pairof Symbol String))
(define-type Xexpr-Datum (U String Index Symbol Bytes))
(define-type Xexpr-Element (List Symbol (Listof Xexpr-Attribute) (Listof Xexpr)))

(define-type Xexpr (Rec elem (U Xexpr-Datum (List Symbol (Listof Xexpr-Attribute) (Listof elem)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xexpr? : (-> Any Boolean : Xexpr)
  (lambda [v]
    (or (xexpr-datum? v)
        (xexpr-element? v))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xexpr-datum? : (-> Any Boolean : Xexpr-Datum)
  (lambda [v]
    (or (string? v)
        (symbol? v)
        (index? v)
        (bytes? v))))

(define xexpr-attribute? : (-> Any Boolean : Xexpr-Attribute)
  (lambda [e]
    (and (pair? e)
         (symbol? (car e))
         (string? (cdr e)))))

(define xexpr-attributes? : (-> Any Boolean : (Listof Xexpr-Attribute))
  (lambda [a]
    (and (list? a)
         (andmap xexpr-attribute? a))))

(define xexpr-list? : (-> Any Boolean : (Listof Xexpr))
  (lambda [a]
    (and (list? a)
         (andmap xexpr? a))))

(define xexpr-element? : (-> Any Boolean : Xexpr-Element)
  (lambda [e]
    (and (pair? e)
         (symbol? (car e))
         (pair? (cdr e))
         (xexpr-attributes? (cadr e))
         (pair? (cddr e))
         (null? (cdddr e))
         (xexpr-list? (caddr e)))))

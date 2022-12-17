#lang typed/racket/base

(provide (all-defined-out))

(require digimon/symbol)

(require "../tokenizer/port.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type XExpr-Attribute-Value (U String (Boxof String) Symbol (Listof Symbol)))
(define-type XExpr-Attribute (U (Pairof Symbol XExpr-Attribute-Value) (List Symbol XExpr-Attribute-Value)))
(define-type XExpr-AttList (Listof XExpr-Attribute))
(define-type XExpr-Datum (U String Index Symbol Bytes XML-White-Space))
(define-type XExpr-PI (MPairof Symbol (Option String)))

(define-type XExpr-Full-Element (Rec elem (U (List Symbol XExpr-AttList (Listof (U XExpr-PI XExpr-Datum XExpr-Element elem))))))
(define-type XExpr-Children-Element (Rec elem (List Symbol (Listof (U XExpr-PI XExpr-Datum XExpr-Element elem)))))
(define-type XExpr-Empty-Element (U (List Symbol) (List Symbol (Listof XExpr-Attribute))))

(define-type XExpr-Element (U XExpr-Full-Element XExpr-Children-Element XExpr-Empty-Element))
(define-type XExpr (U XExpr-PI XExpr-Datum XExpr-Element))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module unsafe racket/base
  (provide (all-defined-out))

  (define xexpr-&value?
    (lambda [v]
      (and (box? v)
           (string? (unbox v)))))

  (define xexpr-pi?
    (lambda [v]
      (and (mpair? v)
           (symbol? (mcar v))
           (or (not (mcdr v))
               (string? (mcdr v)))))))

(require typed/racket/unsafe)
(unsafe-require/typed
 (submod "." unsafe)
 [xexpr-&value? (-> Any Boolean : (Boxof String))]
 [xexpr-pi? (-> Any Boolean : XExpr-PI)])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xexpr? : (-> Any Boolean : XExpr)
  (lambda [v]
    (or (xexpr-datum? v)
        (xexpr-element? v)
        (xexpr-pi? v))))

(define write-xexpr : (->* (XExpr) (Output-Port #:prolog? Boolean #:newline-at-end? Boolean) Void)
  (lambda [x [/dev/xmlout (current-output-port)] #:prolog? [prolog? #false] #:newline-at-end? [blank? #true]]
    (unless (not prolog?)
      (write-bytes #"<?xml" /dev/xmlout)
      (write-xexpr-attrlist '([version "1.0"] [encoding "UTF-8"] [standalone "yes"]) /dev/xmlout)
      (write-bytes #"?>" /dev/xmlout)
      (newline /dev/xmlout))
    
    (cond [(xexpr-empty-element? x)
           (write-char #\< /dev/xmlout)
           (write (car x) /dev/xmlout)
           (let ([maybe-attrs (cdr x)])
             (when (pair? maybe-attrs)
               (write-xexpr-attrlist (car maybe-attrs) /dev/xmlout)))
           (write-bytes #"/>" /dev/xmlout)]
          [(xexpr-full-element? x)
           (write-xexpr-element (car x) (cadr x) (caddr x) /dev/xmlout)]
          [(xexpr-children-element? x)
           (write-xexpr-element (car x) null (cadr x) /dev/xmlout)]
          [(string? x)
           (write-xexpr-string x /dev/xmlout)]
          [(symbol? x)
           (write-char #\& /dev/xmlout)
           (write x /dev/xmlout)
           (write-char #\; /dev/xmlout)]
          [(index? x)
           (write-bytes #"&#x" /dev/xmlout)
           (write-string (number->string x 16) /dev/xmlout)
           (write-char #\; /dev/xmlout)]
          [(bytes? x)
           (write-bytes #"<![CDATA[" /dev/xmlout)
           (write-bytes x /dev/xmlout)
           (write-bytes #"]]>" /dev/xmlout)]
          [(mpair? x)
           (write-bytes #"<?" /dev/xmlout)
           (write (mcar x) /dev/xmlout)
           (let ([c (mcdr x)])
             (when (string? c)
               (write-char #\space /dev/xmlout)
               (write-string c /dev/xmlout)))
           (write-bytes #"?>" /dev/xmlout)])

    (unless (not blank?)
      (newline /dev/xmlout))))

(define xexpr->bytes : (-> XExpr [#:prolog? Boolean] [#:newline-at-end? Boolean] Bytes)
  (lambda [x #:prolog? [prolog? #false] #:newline-at-end? [blank? #true]]
    (define /dev/xmlout (open-output-bytes '/dev/xmlout))

    (write-xexpr x /dev/xmlout #:prolog? prolog? #:newline-at-end? blank?)
    (get-output-bytes /dev/xmlout)))

(define xexpr->string : (-> XExpr [#:prolog? Boolean] [#:newline-at-end? Boolean] String)
  (lambda [x #:prolog? [prolog? #false] #:newline-at-end? [blank? #true]]
    (bytes->string/utf-8 (xexpr->bytes x #:prolog? prolog? #:newline-at-end? blank?))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define write-xexpr-element : (->* (Symbol XExpr-AttList (Listof XExpr)) (Output-Port) Void)
  (lambda [tagname attrlist children [/dev/xmlout (current-output-port)]]
    (write-char #\< /dev/xmlout)
    (write tagname /dev/xmlout)
    (write-xexpr-attrlist attrlist /dev/xmlout)
    (write-char #\> /dev/xmlout)
    (write-xexpr-children children /dev/xmlout)
    (write-bytes #"</" /dev/xmlout)
    (write tagname /dev/xmlout)
    (write-char #\> /dev/xmlout)))

(define write-xexpr-attrlist : (->* (XExpr-AttList) (Output-Port) Void)
  (lambda [attrlist [/dev/xmlout (current-output-port)]]
    (for ([attr (in-list attrlist)])
      (write-char #\space /dev/xmlout)
      (write (car attr) /dev/xmlout)
      (write-char #\= /dev/xmlout)
      
      (write-char #\" /dev/xmlout)
      (write-xexpr-attribute-value (cdr attr) /dev/xmlout)
      (write-char #\" /dev/xmlout))))

(define write-xexpr-children : (->* ((Listof XExpr)) (Output-Port) Void)
  (lambda [children [/dev/xmlout (current-output-port)]]
    (for ([child (in-list children)])
      (write-xexpr #:prolog? #false #:newline-at-end? #false
                   child /dev/xmlout))))

(define write-xexpr-attribute-value : (->* ((U XExpr-Attribute-Value (List XExpr-Attribute-Value))) (Output-Port) Void)
  (lambda [v [/dev/xmlout (current-output-port)]]
    (cond [(string? v) (write-xexpr-string v /dev/xmlout)]
          [(symbol? v) (write-xexpr-string (symbol->immutable-string v) /dev/xmlout)]
          [(box? v) (write-xexpr-string (unbox v) /dev/xmlout)]
          [(symbol-list? v) (write-xexpr-string (symbol-join v) /dev/xmlout)]
          [else (write-xexpr-attribute-value (car v) /dev/xmlout)])))

(define write-xexpr-string : (->* (String) (Output-Port) Void)
  (lambda [s [/dev/xmlout (current-output-port)]]
    (for ([ch (in-string s)])
      (case ch
        [(#\") (display "&quot;" /dev/xmlout)]
        [(#\') (display "&apos;" /dev/xmlout)]
        [(#\&) (display "&amp;" /dev/xmlout)]
        [(#\<) (display "&lt;" /dev/xmlout)]
        [(#\>) (display "&gt;" /dev/xmlout)]
        [else (write-char ch /dev/xmlout)]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xexpr-datum? : (-> Any Boolean : XExpr-Datum)
  (lambda [v]
    (or (string? v)
        (symbol? v)
        (index? v)
        (bytes? v)
        (xml-white-space? v))))

(define xexpr-attribute-value? : (-> Any Boolean : XExpr-Attribute-Value)
  (lambda [v]
    (or (string? v)
        (symbol? v)
        (xexpr-&value? v)
        (symbol-list? v))))

(define xexpr-attribute? : (-> Any Boolean : XExpr-Attribute)
  (lambda [e]
    (and (pair? e)
         (symbol? (car e))
         (or (xexpr-attribute-value? (cdr e))
             (and (pair? (cdr e))
                  (xexpr-attribute-value? (cadr e))
                  (null? (cddr e)))))))

(define xexpr-attrlist? : (-> Any Boolean : XExpr-AttList)
  (lambda [a]
    (and (list? a)
         (andmap xexpr-attribute? a))))

(define xexpr-list? : (-> Any Boolean : (Listof XExpr))
  (lambda [a]
    (and (list? a)
         (andmap xexpr? a))))

(define xexpr-element? : (-> Any Boolean : XExpr-Element)
  (lambda [e]
    (or (xexpr-empty-element? e)
        (xexpr-children-element? e)
        (xexpr-full-element? e))))

(define xexpr-full-element? : (-> Any Boolean : XExpr-Full-Element)
  (lambda [e]
    (and (pair? e)
         (symbol? (car e))
         (pair? (cdr e))
         (xexpr-attrlist? (cadr e))
         (pair? (cddr e))
         (null? (cdddr e))
         (xexpr-list? (caddr e)))))

(define xexpr-children-element? : (-> Any Boolean : XExpr-Children-Element)
  (lambda [e]
    (and (pair? e)
         (symbol? (car e))
         (pair? (cdr e))
         (null? (cddr e))
         (xexpr-list? (cadr e)))))

(define xexpr-empty-element? : (-> Any Boolean : XExpr-Empty-Element)
  (lambda [e]
    (and (pair? e)
         (symbol? (car e))
         (or (null? (cdr e))
             (and (pair? (cdr e))
                  (xexpr-attrlist? (cadr e))
                  (null? (cddr e)))))))

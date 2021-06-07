#lang typed/racket/base

(provide (all-defined-out))

(require digimon/symbol)

(require "../tokenizer/port.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Xexpr-Attribute-Value (U String (Boxof String) Symbol (Listof Symbol)))
(define-type Xexpr-Attribute (U (Pairof Symbol Xexpr-Attribute-Value) (List Symbol Xexpr-Attribute-Value)))
(define-type Xexpr-AttList (Listof Xexpr-Attribute))
(define-type Xexpr-Datum (U String Index Symbol Bytes XML-White-Space))
(define-type Xexpr-PI (MPairof Symbol (Option String)))

(define-type Xexpr-Full-Element (Rec elem (U (List Symbol Xexpr-AttList (Listof (U Xexpr-PI Xexpr-Datum Xexpr-Element elem))))))
(define-type Xexpr-Children-Element (Rec elem (List Symbol (Listof (U Xexpr-PI Xexpr-Datum Xexpr-Element elem)))))
(define-type Xexpr-Empty-Element (U (List Symbol) (List Symbol (Listof Xexpr-Attribute))))

(define-type Xexpr-Element (U Xexpr-Full-Element Xexpr-Children-Element Xexpr-Empty-Element))
(define-type Xexpr (U Xexpr-PI Xexpr-Datum Xexpr-Element))

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
 [xexpr-pi? (-> Any Boolean : Xexpr-PI)])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xexpr? : (-> Any Boolean : Xexpr)
  (lambda [v]
    (or (xexpr-datum? v)
        (xexpr-element? v)
        (xexpr-pi? v))))

(define write-xexpr : (->* (Xexpr) (Output-Port #:prolog? Boolean #:newline-at-end? Boolean) Void)
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

(define xexpr->bytes : (-> Xexpr [#:prolog? Boolean] [#:newline-at-end? Boolean] Bytes)
  (lambda [x #:prolog? [prolog? #false] #:newline-at-end? [blank? #true]]
    (define /dev/xmlout (open-output-bytes '/dev/xmlout))

    (write-xexpr x /dev/xmlout #:prolog? prolog? #:newline-at-end? blank?)
    (get-output-bytes /dev/xmlout)))

(define xexpr->string : (-> Xexpr [#:prolog? Boolean] [#:newline-at-end? Boolean] String)
  (lambda [x #:prolog? [prolog? #false] #:newline-at-end? [blank? #true]]
    (bytes->string/utf-8 (xexpr->bytes x #:prolog? prolog? #:newline-at-end? blank?))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define write-xexpr-element : (->* (Symbol Xexpr-AttList (Listof Xexpr)) (Output-Port) Void)
  (lambda [tagname attrlist children [/dev/xmlout (current-output-port)]]
    (write-char #\< /dev/xmlout)
    (write tagname /dev/xmlout)
    (write-xexpr-attrlist attrlist /dev/xmlout)
    (write-char #\> /dev/xmlout)
    (write-xexpr-children children /dev/xmlout)
    (write-bytes #"</" /dev/xmlout)
    (write tagname /dev/xmlout)
    (write-char #\> /dev/xmlout)))

(define write-xexpr-attrlist : (->* (Xexpr-AttList) (Output-Port) Void)
  (lambda [attrlist [/dev/xmlout (current-output-port)]]
    (for ([attr (in-list attrlist)])
      (write-char #\space /dev/xmlout)
      (write (car attr) /dev/xmlout)
      (write-char #\= /dev/xmlout)
      
      (write-char #\" /dev/xmlout)
      (write-xexpr-attribute-value (cdr attr) /dev/xmlout)
      (write-char #\" /dev/xmlout))))

(define write-xexpr-children : (->* ((Listof Xexpr)) (Output-Port) Void)
  (lambda [children [/dev/xmlout (current-output-port)]]
    (for ([child (in-list children)])
      (write-xexpr #:prolog? #false #:newline-at-end? #false
                   child /dev/xmlout))))

(define write-xexpr-attribute-value : (->* ((U Xexpr-Attribute-Value (List Xexpr-Attribute-Value))) (Output-Port) Void)
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
(define xexpr-datum? : (-> Any Boolean : Xexpr-Datum)
  (lambda [v]
    (or (string? v)
        (symbol? v)
        (index? v)
        (bytes? v)
        (xml-white-space? v))))

(define xexpr-attribute-value? : (-> Any Boolean : Xexpr-Attribute-Value)
  (lambda [v]
    (or (string? v)
        (symbol? v)
        (xexpr-&value? v)
        (symbol-list? v))))

(define xexpr-attribute? : (-> Any Boolean : Xexpr-Attribute)
  (lambda [e]
    (and (pair? e)
         (symbol? (car e))
         (or (xexpr-attribute-value? (cdr e))
             (and (pair? (cdr e))
                  (xexpr-attribute-value? (cadr e))
                  (null? (cddr e)))))))

(define xexpr-attrlist? : (-> Any Boolean : Xexpr-AttList)
  (lambda [a]
    (and (list? a)
         (andmap xexpr-attribute? a))))

(define xexpr-list? : (-> Any Boolean : (Listof Xexpr))
  (lambda [a]
    (and (list? a)
         (andmap xexpr? a))))

(define xexpr-element? : (-> Any Boolean : Xexpr-Element)
  (lambda [e]
    (or (xexpr-empty-element? e)
        (xexpr-children-element? e)
        (xexpr-full-element? e))))

(define xexpr-full-element? : (-> Any Boolean : Xexpr-Full-Element)
  (lambda [e]
    (and (pair? e)
         (symbol? (car e))
         (pair? (cdr e))
         (xexpr-attrlist? (cadr e))
         (pair? (cddr e))
         (null? (cdddr e))
         (xexpr-list? (caddr e)))))

(define xexpr-children-element? : (-> Any Boolean : Xexpr-Children-Element)
  (lambda [e]
    (and (pair? e)
         (symbol? (car e))
         (pair? (cdr e))
         (null? (cddr e))
         (xexpr-list? (cadr e)))))

(define xexpr-empty-element? : (-> Any Boolean : Xexpr-Empty-Element)
  (lambda [e]
    (and (pair? e)
         (symbol? (car e))
         (or (null? (cdr e))
             (and (pair? (cdr e))
                  (xexpr-attrlist? (cadr e))
                  (null? (cddr e)))))))

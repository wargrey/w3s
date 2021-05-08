#lang typed/racket/base

(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type Xexpr-Attribute (U (Pairof Symbol String) (List Symbol String)))
(define-type Xexpr-Attrlist (Listof Xexpr-Attribute))
(define-type Xexpr-Datum (U String Index Symbol Bytes))

(define-type Xexpr-Full-Element (Rec elem (U (List Symbol Xexpr-Attrlist (Listof (U Xexpr-Datum Xexpr-Element elem))))))
(define-type Xexpr-Children-Element (Rec elem (List Symbol (Listof (U Xexpr-Datum Xexpr-Element elem)))))
(define-type Xexpr-Empty-Element (U (List Symbol) (List Symbol (Listof Xexpr-Attribute))))

(define-type Xexpr-Element (U Xexpr-Full-Element Xexpr-Children-Element Xexpr-Empty-Element))
(define-type Xexpr (U Xexpr-Datum Xexpr-Element))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xexpr? : (-> Any Boolean : Xexpr)
  (lambda [v]
    (or (xexpr-datum? v)
        (xexpr-element? v))))

(define write-xexpr : (->* (Xexpr) (Output-Port #:prolog? Boolean) Void)
  (lambda [x [/dev/xmlout (current-output-port)] #:prolog? [prolog? #false]]
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
           (write-bytes #" \\>" /dev/xmlout)]
          [(xexpr-full-element? x)
           (write-xexpr-element (car x) (cadr x) (caddr x) /dev/xmlout)]
          [(xexpr-children-element? x)
           (write-xexpr-element (car x) null (cadr x) /dev/xmlout)]
          [(string? x)
           (write-string x /dev/xmlout)]
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
           (write-bytes #"]]>" /dev/xmlout)])
    (void)))

(define xexpr->bytes : (-> Xexpr [#:prolog? Boolean] Bytes)
  (lambda [x #:prolog? [prolog? #false]]
    (define /dev/xmlout (open-output-bytes '/dev/xmlout))

    (write-xexpr x /dev/xmlout #:prolog? prolog?)
    (get-output-bytes /dev/xmlout)))

(define xexpr->string : (-> Xexpr [#:prolog? Boolean] String)
  (lambda [x #:prolog? [prolog? #false]]
    (bytes->string/utf-8 (xexpr->bytes x #:prolog? prolog?))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define write-xexpr-element : (->* (Symbol Xexpr-Attrlist (Listof Xexpr)) (Output-Port) Void)
  (lambda [tagname attrlist children [/dev/xmlout (current-output-port)]]
    (write-char #\< /dev/xmlout)
    (write tagname /dev/xmlout)
    (write-xexpr-attrlist attrlist /dev/xmlout)
    (write-char #\> /dev/xmlout)
    (write-xexpr-children children /dev/xmlout)
    (write-bytes #"</" /dev/xmlout)
    (write tagname /dev/xmlout)
    (write-char #\> /dev/xmlout)))

(define write-xexpr-attrlist : (->* (Xexpr-Attrlist) (Output-Port) Void)
  (lambda [attrlist [/dev/xmlout (current-output-port)]]
    (for ([attr (in-list attrlist)])
      (write-char #\space /dev/xmlout)
      (write (car attr) /dev/xmlout)
      (write-char #\= /dev/xmlout)
      (let ([v (cdr attr)])
        (cond [(pair? v) (write (car v) /dev/xmlout)]
              [else (write v /dev/xmlout)])))))

(define write-xexpr-children : (->* ((Listof Xexpr)) (Output-Port) Void)
  (lambda [children [/dev/xmlout (current-output-port)]]
    (for ([child (in-list children)])
      (write-xexpr #:prolog? #false
                   child /dev/xmlout))))

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
         (or (string? (cdr e))
             (and (pair? (cdr e))
                  (string? (cadr e))
                  (null? (cddr e)))))))

(define xexpr-attrlist? : (-> Any Boolean : Xexpr-Attrlist)
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

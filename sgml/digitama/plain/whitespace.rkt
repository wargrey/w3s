#lang typed/racket/base

(provide (all-defined-out))
(provide (all-from-out "../whitespace.rkt"))

(require (for-syntax racket/base))

(require racket/string)
(require racket/symbol)

(require "grammar.rkt")

(require "../misc.rkt")
(require "../space.rkt")
(require "../whitespace.rkt")

(require "../tokenizer/port.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax (xml-white-space-remake stx)
  (syntax-case stx []
    [(_ src type datum)
     (syntax/loc stx (xml-white-space datum))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-xml:space=preserve xml:space=preserve #:=> XML-White-Space
  #:remake-space xml-white-space-remake
  #:space-datum xml-white-space-raw
  #:space-newline? xml-new-line?)

(define-xml-child-cons xml-child-cons (#:-> XML-White-Space (Listof (U XML-Subdatum XML-Element)))
  #:remake-space xml-white-space-remake #:space-datum xml-white-space-raw
  #:spaces-fold xml-whitespaces-fold #:spaces-consolidate xml-whitespaces-consolidate
  #:space=preserve xml:space=preserve #:space-newline? xml-new-line?)

(define xml-whitespaces-consolidate : (-> (Pairof XML-White-Space (Listof XML-White-Space)) XML-White-Space)
  (lambda [secaps]
    xml-whitespace/1))

(define xml-whitespaces-fold : (-> (Pairof XML-White-Space (Listof XML-White-Space)) XML-White-Space)
  (lambda [secaps]
    (define-values (head body) (values (car secaps) (cdr secaps)))
    
    (cond [(null? body) head]
          [else (let ws-fold ([rest : (Listof XML-White-Space) body]
                              [spaces : (Listof String) null]
                              [has-newline? : Boolean (xml-new-line? head)])
                  (if (null? rest)
                      (let ([ws (apply string-append spaces)])
                        (if has-newline? (xml-new-line ws) (xml-white-space ws)))
                      (let ([self (car rest)])
                        (ws-fold (cdr rest)
                                 (cons (xml-white-space-raw self) spaces)
                                 (or has-newline? (xml-new-line? self))))))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml:lang-ref : (case-> [String -> (Option String)]
                               [Any XML-Element-Attribute-Value -> (Option String)])
  (case-lambda
    [(lang)
     (and (> (string-length lang) 0) lang)]
    [(tagname v)
     (and (string? v)
          (xml:lang-ref v))]))

(define xml:space-ref : (-> XML-Element-Attribute-Value Symbol Symbol)
  (lambda [attval inherited-space]
    (define this:space : String
      (cond [(string? attval) attval]
            [(box? attval) (unbox attval)]
            [(symbol? attval) (symbol->immutable-string attval)]
            [else (string-join (map symbol->immutable-string attval))]))
    
    (cond [(string-ci=? this:space "preserve") 'preserve]
          [(string-ci=? this:space "default") (default-xml:space-signal)]
          [else inherited-space])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml-whitespace/0 : XML-White-Space (xml-white-space "")) ; to tell `xml-child-cons` here isn't the head position
(define xml-whitespace/1 : XML-White-Space (xml-white-space " ")) ; consolidated whitespace

(define xml-cdata->datum : (-> (U String XML-White-Space) String)
  (lambda [cdata]
    (cond [(string? cdata) cdata]
          [else (xml-white-space-raw cdata)])))

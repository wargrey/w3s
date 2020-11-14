#lang typed/racket/base

(provide (all-defined-out))
(provide (all-from-out "../whitespace.rkt"))

(require (for-syntax racket/base))

(require racket/string)

(require "grammar.rkt")

(require "../misc.rkt")
(require "../space.rkt")
(require "../whitespace.rkt")

(require "../tokenizer/port.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax (xml-white-space-remake stx)
  (syntax-case stx []
    [(_ src type datum)
     #'(xml-white-space datum)]))

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
  (let ([consolidated-whitespace (xml-white-space " ")])
    (lambda [secaps]
      consolidated-whitespace)))

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
            [(symbol? attval) (symbol->string attval)]
            [else (string-join (map symbol->string attval))]))
    
    (cond [(string-ci=? this:space "preserve") 'preserve]
          [(string-ci=? this:space "default") (default-xml:space-signal)]
          [else inherited-space])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml:space-cons : (-> String Fixnum Char Char (-> Symbol (Option String) Char (Option Char)) (U Zero One) (Option (Listof Char))
                             Symbol (Option String) (Option (Listof Char)))
  (lambda [spaces idx s os filter nl-span secaps0 tagname xml:lang]
    (define ns : (Option Char) (filter tagname xml:lang s))
    (cond [(eq? ns os) (if (list? secaps0) (cons s secaps0) secaps0)]
          [else (let ([secaps (if (list? secaps0) secaps0 (reverse (string->list (substring spaces 0 (- idx nl-span)))))])
                  (if (not ns) secaps (cons ns secaps)))])))

(define xml:space-values : (-> Symbol (Option String) Char (Option Char))
  (lambda [tag xml:lang char]
    char))

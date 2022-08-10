#lang typed/racket/base

(provide (all-defined-out))

(require racket/string)

(require digimon/symbol)
(require digimon/dimension)

(require "grammar.rkt")
(require "digicore.rkt")

;;; NOTE
; Normalizing the attributes' values require DTD or other schema,
; These APIs are designed to manually normalizing,
; Thus, the input tokens are usually XML:String instances.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type XML-Dimension (Pairof Flonum Symbol))
(define-type XML-Nonnegative-Dimension (Pairof Nonnegative-Flonum Symbol))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml-attribute-datum->value : (-> Any String)
  (lambda [v]
    (cond [(string? v) v]
          [(symbol? v) (symbol->immutable-string v)]
          [(number? v) (number->string v)]
          [else (~a v)])))

(define xml-attribute-dimension->value : (-> (Pairof Real Symbol) String)
  (lambda [v]
    (string-append (number->string (car v))
                   (symbol->immutable-string (cdr v)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml-attribute-value*->string : (-> XML-Element-Attribute-Value* String)
  (lambda [v]
    (cond [(xml:string? v) (xml:string-datum v)]
          [(xml:name? v) (symbol->immutable-string (xml:name-datum v))]
          [else (symbol-join (map xml:name-datum v))])))

(define xml-attribute-value*->keyword : (-> XML-Element-Attribute-Value* Keyword)
  (lambda [v]
    (cond [(xml:string? v) (string->keyword (xml:string-datum v))]
          [(xml:name? v) (symbol->keyword (xml:name-datum v))]
          [else (string->keyword (string-join (map symbol->immutable-string (map xml:name-datum v))))])))

(define xml-attribute-value*->symbol : (-> XML-Element-Attribute-Value* Symbol)
  (lambda [v]
    (cond [(xml:string? v) (string->symbol (xml:string-datum v))]
          [(xml:name? v) (symbol->interned-symbol (xml:name-datum v))]
          [else (string->symbol (symbol-join (map xml:name-datum v)))])))

(define xml-attribute-value*->unreadable-symbol : (-> XML-Element-Attribute-Value* Symbol)
  (lambda [v]
    (cond [(xml:string? v) (string->unreadable-symbol (xml:string-datum v))]
          [(xml:name? v) (symbol->unreadable-symbol (xml:name-datum v))]
          [else (string->unreadable-symbol (symbol-join (map xml:name-datum v)))])))

(define xml-attribute-value*->symbol-list : (-> XML-Element-Attribute-Value* (Listof Symbol))
  (lambda [v]
    (cond [(xml:string? v) (map string->symbol (string-split (xml:string-datum v)))]
          [(list? v) (map xml:name-datum v)]
          [(xml:name? v) (list (xml:name-datum v))]
          [else null])))

(define xml-attribute-value*->unreadable-symbol-list : (-> XML-Element-Attribute-Value* (Listof Symbol))
  (lambda [v]
    (cond [(xml:string? v) (map string->unreadable-symbol (string-split (xml:string-datum v)))]
          [(list? v) (map symbol->unreadable-symbol (map xml:name-datum v))]
          [(xml:name? v) (list (symbol->unreadable-symbol (xml:name-datum v)))]
          [else null])))

(define xml-attribute-value*->dimension : (->* (XML-Element-Attribute-Value*) (Symbol) XML-Dimension)
  (lambda [v [canonical-unit '||]]
    (define-values (n unit)
      (cond [(xml:string? v) (string->dimension (xml:string-datum v) canonical-unit #:ci? #false)]
            [(xml:name? v) (string->dimension (xml:name-datum v) canonical-unit #:ci? #false)]
            [else (string->dimension "" canonical-unit)]))

    (cons n unit)))

(define xml-attribute-value*->nonnegative-dimension : (->* (XML-Element-Attribute-Value*) (Symbol) XML-Nonnegative-Dimension)
  (lambda [v [canonical-unit '||]]
    (define-values (n unit)
      (cond [(xml:string? v) (string->dimension (xml:string-datum v) canonical-unit #:ci? #false)]
            [(xml:name? v) (string->dimension (xml:name-datum v) canonical-unit #:ci? #false)]
            [else (string->dimension "" canonical-unit)]))

    (cons (if (>= n 0.0) n +nan.0) unit)))

(define xml-attribute-value*->dimension/ci : (->* (XML-Element-Attribute-Value*) (Symbol) XML-Dimension)
  (lambda [v [canonical-unit '||]]
    (define-values (n unit)
      (cond [(xml:string? v) (string->dimension (xml:string-datum v) canonical-unit #:ci? #true)]
            [(xml:name? v) (string->dimension (xml:name-datum v) canonical-unit #:ci? #true)]
            [else (string->dimension "" canonical-unit)]))

    (cons n unit)))

(define xml-attribute-value*->nonnegative-dimension/ci : (->* (XML-Element-Attribute-Value*) (Symbol) XML-Nonnegative-Dimension)
  (lambda [v [canonical-unit '||]]
    (define-values (n unit)
      (cond [(xml:string? v) (string->dimension (xml:string-datum v) canonical-unit #:ci? #true)]
            [(xml:name? v) (string->dimension (xml:name-datum v) canonical-unit #:ci? #true)]
            [else (string->dimension "" canonical-unit)]))

    (cons (if (>= n 0.0) n +nan.0) unit)))


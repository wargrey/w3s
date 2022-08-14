#lang typed/racket/base

(provide (all-defined-out))
(provide (rename-out [xml-attribute-value*+>integer xml-attribute-value*->natural]))

(require racket/string)

(require digimon/symbol)
(require digimon/dimension)
(require digimon/number)

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
          [(list? v) (string-join (map xml-attribute-datum->value v))]
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

(define xml-attribute-value*->string-list : (->* (XML-Element-Attribute-Value*) ((U String Regexp)) (Listof String))
  (lambda [v [sep #px"\\s+"]]
    (cond [(xml:string? v) (string-split (xml:string-datum v) sep)]
          [(list? v) (for/list : (Listof String) ([n (in-list v)]) (symbol->immutable-string (xml:name-datum n)))]
          [(xml:name? v) (list (symbol->immutable-string (xml:name-datum v)))]
          [else null])))

(define xml-attribute-value*->keyword : (-> XML-Element-Attribute-Value* Keyword)
  (lambda [v]
    (cond [(xml:string? v) (string->keyword (xml:string-datum v))]
          [(xml:name? v) (symbol->keyword (xml:name-datum v))]
          [else (string->keyword (string-join (map symbol->immutable-string (map xml:name-datum v))))])))

(define xml-attribute-value*->integer : (-> XML-Element-Attribute-Value* (Option Integer))
  (lambda [v]
    (cond [(xml:string? v) (string->integer (xml:string-datum v))]
          [(xml:name? v) (string->integer (symbol->immutable-string (xml:name-datum v)))]
          [else #false])))

(define xml-attribute-value*+>integer : (-> XML-Element-Attribute-Value* (Option Natural))
  (lambda [v]
    (cond [(xml:string? v) (string->natural (xml:string-datum v))]
          [(xml:name? v) (string->natural (symbol->immutable-string (xml:name-datum v)))]
          [else #false])))

(define xml-attribute-value*->flonum : (-> XML-Element-Attribute-Value* (Option Flonum))
  (lambda [v]
    (cond [(xml:string? v) (string->flonum (xml:string-datum v))]
          [(xml:name? v) (string->flonum (symbol->immutable-string (xml:name-datum v)))]
          [else #false])))

(define xml-attribute-value*+>flonum : (-> XML-Element-Attribute-Value* (Option Nonnegative-Flonum))
  (lambda [v]
    (cond [(xml:string? v) (string+>flonum (xml:string-datum v))]
          [(xml:name? v) (string+>flonum (symbol->immutable-string (xml:name-datum v)))]
          [else #false])))

(define xml-attribute-value*->number : (-> XML-Element-Attribute-Value* (Option Real))
  (lambda [v]
    (cond [(xml:string? v) (string->real (xml:string-datum v))]
          [(xml:name? v) (string->real (symbol->immutable-string (xml:name-datum v)))]
          [else #false])))

(define xml-attribute-value*+>number : (-> XML-Element-Attribute-Value* (Option Nonnegative-Real))
  (lambda [v]
    (cond [(xml:string? v) (string+>real (xml:string-datum v))]
          [(xml:name? v) (string+>real (symbol->immutable-string (xml:name-datum v)))]
          [else #false])))

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

(define xml-attribute-value*+>dimension : (->* (XML-Element-Attribute-Value*) (Symbol) XML-Nonnegative-Dimension)
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

(define xml-attribute-value*+>dimension/ci : (->* (XML-Element-Attribute-Value*) (Symbol) XML-Nonnegative-Dimension)
  (lambda [v [canonical-unit '||]]
    (define-values (n unit)
      (cond [(xml:string? v) (string->dimension (xml:string-datum v) canonical-unit #:ci? #true)]
            [(xml:name? v) (string->dimension (xml:name-datum v) canonical-unit #:ci? #true)]
            [else (string->dimension "" canonical-unit)]))

    (cons (if (>= n 0.0) n +nan.0) unit)))

(define xml-attribute-value*->type-list : (All (T) (->* (XML-Element-Attribute-Value* (-> XML-Element-Attribute-Value* (Option T)))
                                                        ((U String Regexp))
                                                        (Listof T)))
  (lambda [v ->type-datum [sep #px"\\s*,\\s*"]]
    (cond [(xml:string? v)
           (let parse ([vs : (Listof String) (string-split (xml:string-datum v) sep)]
                       [ts : (Listof T) null])
             (cond [(null? vs) (reverse ts)]
                   [else (let*-values ([(self rest) (values (car vs) (cdr vs))]
                                       [(datum) (->type-datum (syn-remake-token v xml:string self))])
                           (cond [(not datum) (parse rest ts)]
                                 [else (parse rest (cons datum ts))]))]))]
          [(list? v)
           (let parse ([vs : (Listof XML:Name) v]
                       [ts : (Listof T) null])
             (cond [(null? vs) (reverse ts)]
                   [else (let*-values ([(self rest) (values (car vs) (cdr vs))]
                                       [(datum) (->type-datum (syn-remake-token self xml:string (symbol->immutable-string (xml:name-datum self))))])
                           (cond [(not datum) (parse rest ts)]
                                 [else (parse rest (cons datum ts))]))]))]
          [(xml:name? v)
           (let ([datum (->type-datum (syn-remake-token v xml:string (symbol->immutable-string (xml:name-datum v))))])
             (if (not datum) null (list datum)))]
          [else null])))

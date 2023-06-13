#lang typed/racket/base

(provide (all-defined-out))
(provide (rename-out [xml:attr-value+>integer xml:attr-value->natural]))
(provide (all-from-out "../shared/datatype.rkt"))
(provide (all-from-out "../shared/enum.rkt"))

(require racket/string)

(require digimon/symbol)
(require digimon/dimension)
(require digimon/number)

(require "grammar.rkt")
(require "../shared/datatype.rkt")
(require "../shared/enum.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; NOTE
; These APIs are designed to manually normalizing for SAX,
; Thus, the input data are usually plain strings.

(define xml:attr-value->string : (-> XML-Element-Attribute-Value String)
  (lambda [v]
    (cond [(string? v) v]
          [(symbol? v) (symbol->immutable-string v)]
          [(list? v) (symbol-join v)]
          [else (xml:attr-value->string (unbox v))])))

(define xml:attr-value->string/trim : (-> XML-Element-Attribute-Value String)
  (lambda [v]
    (cond [(string? v) (string-trim v)]
          [(symbol? v) (symbol->immutable-string v)]
          [(list? v) (symbol-join v)]
          [else (xml:attr-value->string (unbox v))])))

(define xml:attr-value->string-list : (->* (XML-Element-Attribute-Value) ((U String Regexp)) (Option (Listof String)))
  (lambda [v [sep #px"\\s+"]]
    (cond [(string? v) (string-split (string-trim v) sep)]
          [(list? v) (for/list : (Listof String) ([n (in-list v)]) (symbol->immutable-string n))]
          [(symbol? v) (list (symbol->immutable-string v))]
          [else (xml:attr-value->string-list (unbox v))])))

(define xml:attr-value->boolean : (-> XML-Element-Attribute-Value (Option XML-Boolean))
  (lambda [v]
    (cond [(string? v) (cond [(member v '("true" "1")) 'true] [(member v '("false" "0")) 'false] [else #false])]
          [(symbol? v) (and (or (eq? v 'true) (eq? v 'false)) v)]
          [else #false])))

(define xml:attr-value->integer : (-> XML-Element-Attribute-Value (Option Integer))
  (lambda [v]
    (cond [(string? v) (string->integer (string-trim v))]
          [(symbol? v) (string->integer (symbol->immutable-string v))]
          [else #false])))

(define xml:attr-value->index : (-> XML-Element-Attribute-Value (Option Index))
  (lambda [v]
    (cond [(string? v) (string->index (string-trim v))]
          [(symbol? v) (string->index (symbol->immutable-string v))]
          [else #false])))

(define xml:attr-value+>integer : (-> XML-Element-Attribute-Value (Option Natural))
  (lambda [v]
    (cond [(string? v) (string->natural (string-trim v))]
          [(symbol? v) (string->natural (symbol->immutable-string v))]
          [else #false])))

(define xml:attr-value->flonum : (-> XML-Element-Attribute-Value (Option Flonum))
  (lambda [v]
    (cond [(string? v) (string->flonum (string-trim v))]
          [(symbol? v) (string->flonum (symbol->immutable-string v))]
          [else #false])))

(define xml:attr-value+>flonum : (-> XML-Element-Attribute-Value (Option Nonnegative-Flonum))
  (lambda [v]
    (cond [(string? v) (string+>flonum (string-trim v))]
          [(symbol? v) (string+>flonum (symbol->immutable-string v))]
          [else #false])))

(define xml:attr-value->number : (-> XML-Element-Attribute-Value (Option Real))
  (lambda [v]
    (cond [(string? v) (string->real (string-trim v))]
          [(symbol? v) (string->real (symbol->immutable-string v))]
          [else #false])))

(define xml:attr-value+>number : (-> XML-Element-Attribute-Value (Option Nonnegative-Real))
  (lambda [v]
    (cond [(string? v) (string+>real (string-trim v))]
          [(symbol? v) (string+>real (symbol->immutable-string v))]
          [else #false])))

(define xml:attr-value->symbol : (-> XML-Element-Attribute-Value Symbol)
  (lambda [v]
    (cond [(string? v) (string->symbol (string-trim v))]
          [(symbol? v) (symbol->interned-symbol v)]
          [(list? v) (string->symbol (symbol-join v))]
          [else (xml:attr-value->symbol (unbox v))])))

(define xml:attr-value->unreadable-symbol : (-> XML-Element-Attribute-Value Symbol)
  (lambda [v]
    (cond [(string? v) (string->unreadable-symbol (string-trim v))]
          [(symbol? v) (symbol->unreadable-symbol v)]
          [(list? v) (string->unreadable-symbol (symbol-join v))]
          [else (xml:attr-value->unreadable-symbol (unbox v))])))

(define xml:attr-value->symbol-list : (->* (XML-Element-Attribute-Value) ((U String Regexp)) (Option (Listof Symbol)))
  (lambda [v [sep #px"\\s+"]]
    (cond [(string? v) (map string->symbol (string-split (string-trim v) sep))]
          [(list? v) v]
          [(symbol? v) (list v)]
          [else (xml:attr-value->symbol-list (unbox v) sep)])))

(define xml:attr-value->unreadable-symbol-list : (->* (XML-Element-Attribute-Value) ((U String Regexp)) (Option (Listof Symbol)))
  (lambda [v [sep #px"\\s+"]]
    (cond [(string? v) (map string->unreadable-symbol (string-split v sep))]
          [(list? v) (map symbol->unreadable-symbol v)]
          [(symbol? v) (list (symbol->unreadable-symbol v))]
          [else (xml:attr-value->unreadable-symbol-list (unbox v) sep)])))

(define xml:attr-value->keyword : (-> XML-Element-Attribute-Value Keyword)
  (lambda [v]
    (cond [(string? v) (string->keyword (string-trim v))]
          [(symbol? v) (symbol->keyword v)]
          [(list? v) (string->keyword (string-join (map symbol->immutable-string v)))]
          [else (xml:attr-value->keyword (unbox v))])))

(define xml:attr-value->dimension : (->* (XML-Element-Attribute-Value) (Symbol) (Option XML-Dimension))
  (lambda [v [canonical-unit '||]]
    (define-values (n unit)
      (cond [(string? v) (string->dimension v canonical-unit #:ci? #false)]
            [(symbol? v) (string->dimension v canonical-unit #:ci? #false)]
            [else (values #false canonical-unit)]))

    (and n (cons n unit))))

(define xml:attr-value+>dimension : (->* (XML-Element-Attribute-Value) (Symbol) (Option XML-Nonnegative-Dimension))
  (lambda [v [canonical-unit '||]]
    (define-values (n unit)
      (cond [(string? v) (string->dimension v canonical-unit #:ci? #false)]
            [(symbol? v) (string->dimension v canonical-unit #:ci? #false)]
            [else (values #false canonical-unit)]))

    (and n (>= n 0.0) (cons n unit))))

(define xml:attr-value->dimension/ci : (->* (XML-Element-Attribute-Value) (Symbol) (Option XML-Dimension))
  (lambda [v [canonical-unit '||]]
    (define-values (n unit)
      (cond [(string? v) (string->dimension v canonical-unit #:ci? #true)]
            [(symbol? v) (string->dimension v canonical-unit #:ci? #true)]
            [else (values #false canonical-unit)]))

    (and n (cons n unit))))

(define xml:attr-value+>dimension/ci : (->* (XML-Element-Attribute-Value) (Symbol) (Option XML-Nonnegative-Dimension))
  (lambda [v [canonical-unit '||]]
    (define-values (n unit)
      (cond [(string? v) (string->dimension v canonical-unit #:ci? #true)]
            [(symbol? v) (string->dimension v canonical-unit #:ci? #true)]
            [else (values #false canonical-unit)]))

    (and n (>= n 0.0) (cons n unit))))

(define xml:attr-value->listof-type : (All (T) (->* (XML-Element-Attribute-Value (-> XML-Element-Attribute-Value (Option T)))
                                                    ((U String Regexp (-> String (Listof String))))
                                                    (Listof T)))
  (lambda [v string->datum [sep #px"\\s+"]]
    (cond [(string? v)
           (let parse ([vs : (Listof String) (if (procedure? sep) (sep v) (string-split v sep))]
                       [ts : (Listof T) null])
             (cond [(null? vs) (reverse ts)]
                   [else (let*-values ([(self rest) (values (car vs) (cdr vs))]
                                       [(datum) (string->datum self)])
                           (cond [(not datum) (parse rest ts)]
                                 [else (parse rest (cons datum ts))]))]))]
          [(list? v)
           (let parse ([vs : (Listof Symbol) v]
                       [ts : (Listof T) null])
             (cond [(null? vs) (reverse ts)]
                   [else (let*-values ([(self rest) (values (car vs) (cdr vs))]
                                       [(self) (symbol->immutable-string self)]
                                       [(datum) (string->datum self)])
                           (cond [(not datum) (parse rest ts)]
                                 [else (parse rest (cons datum ts))]))]))]
          [(symbol? v)
           (let ([datum (string->datum (symbol->immutable-string v))])
             (cond [(not datum) null]
                   [else (list datum)]))]
          [else null])))

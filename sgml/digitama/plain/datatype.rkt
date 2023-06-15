#lang typed/racket/base

(provide (all-defined-out))
(provide (rename-out [xml:attr-value->natural xml:attr-value+>integer]))
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
(define-type (XML-Attribute-Value->Datum T) (-> XML-Element-Attribute-Value T))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; NOTE
; These APIs are designed to manually normalizing for SAX,
; Thus, the input data are usually plain strings.

(define xml:attr-value->string
  : (case-> [XML-Element-Attribute-Value -> String]
            [XML-Element-Attribute-Value (U String Bytes Regexp Byte-Regexp) -> (Option String)])
  (case-lambda
    [(v)
     (cond [(string? v) v]
           [(symbol? v) (symbol->immutable-string v)]
           [(list? v) (symbol-join v)]
           [else (xml:attr-value->string (unbox v))])]
    [(v pattern)
     (let ([s (xml:attr-value->string v)])
       (and (regexp-match? pattern s)
            s))]))

(define xml:attr-value->string/trim
  : (case-> [XML-Element-Attribute-Value -> String]
            [XML-Element-Attribute-Value (U String Bytes Regexp Byte-Regexp) -> (Option String)])
  (case-lambda
    [(v)
     (cond [(string? v) (string-trim v)]
           [(symbol? v) (symbol->immutable-string v)]
           [(list? v) (symbol-join v)]
           [else (xml:attr-value->string (unbox v))])]
    [(v pattern)
     (let ([s (xml:attr-value->string/trim v)])
       (and (regexp-match? pattern s)
            s))]))

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

(define xml:attr-value->integer : (case-> [XML-Element-Attribute-Value -> (Option Integer)]
                                          [XML-Element-Attribute-Value (Option Integer) -> (Option Integer)]
                                          [XML-Element-Attribute-Value (Option Integer) (Option Integer) -> (Option Integer)])
  (case-lambda
    [(v)
     (cond [(string? v) (string->integer (string-trim v))]
           [(symbol? v) (string->integer (symbol->immutable-string v))]
           [else #false])]
    [(v min)
     (let ([n (xml:attr-value->integer v)])
       (and n
            (cond [(and min) (and (<= min n) n)]
                  [else n])))]
    [(v min max)
     (let ([n (xml:attr-value->integer v)])
       (and n
            (cond [(and min max) (and (<= min n max) n)]
                  [(and min) (and (<= min n) n)]
                  [(and max) (and (<= n max) n)]
                  [else n])))]))

(define xml:attr-value->fixnum : (case-> [XML-Element-Attribute-Value -> (Option Fixnum)]
                                         [XML-Element-Attribute-Value (Option Fixnum) -> (Option Fixnum)]
                                         [XML-Element-Attribute-Value (Option Fixnum) (Option Fixnum) -> (Option Fixnum)])
  (case-lambda
    [(v)
     (cond [(string? v) (string->fixnum (string-trim v))]
           [(symbol? v) (string->fixnum (symbol->immutable-string v))]
           [else #false])]
    [(v min)
     (let ([n (xml:attr-value->fixnum v)])
       (and n
            (cond [(and min) (and (<= min n) n)]
                  [else n])))]
    [(v min max)
     (let ([n (xml:attr-value->fixnum v)])
       (and n
            (cond [(and min max) (and (<= min n max) n)]
                  [(and min) (and (<= min n) n)]
                  [(and max) (and (<= n max) n)]
                  [else n])))]))

(define xml:attr-value+>fixnum : (case-> [XML-Element-Attribute-Value -> (Option Nonnegative-Fixnum)]
                                         [XML-Element-Attribute-Value (Option Nonnegative-Fixnum) -> (Option Nonnegative-Fixnum)]
                                         [XML-Element-Attribute-Value (Option Nonnegative-Fixnum) (Option Nonnegative-Fixnum) -> (Option Nonnegative-Fixnum)])
  (case-lambda
    [(v)
     (cond [(string? v) (string+>fixnum (string-trim v))]
           [(symbol? v) (string+>fixnum (symbol->immutable-string v))]
           [else #false])]
    [(v min)
     (let ([n (xml:attr-value->fixnum v)])
       (and n
            (cond [(and min) (and (<= min n) n)]
                  [else (and (<= 0 n) n)])))]
    [(v min max)
     (let ([n (xml:attr-value->fixnum v)])
       (and n
            (cond [(and min max) (and (<= min n) (<= n max) n)]
                  [(and min) (and (<= min n) n)]
                  [(and max) (and (<= 0 n) (<= n max) n)]
                  [else (and (<= 0 n) n)])))]))

(define xml:attr-value->index : (case-> [XML-Element-Attribute-Value -> (Option Index)]
                                        [XML-Element-Attribute-Value (Option Index) -> (Option Index)]
                                        [XML-Element-Attribute-Value (Option Index) (Option Index) -> (Option Index)])
  (case-lambda
    [(v)
     (cond [(string? v) (string->index (string-trim v))]
           [(symbol? v) (string->index (symbol->immutable-string v))]
           [else #false])]
    [(v min)
     (let ([nat (xml:attr-value->integer v)])
       (and nat
            (cond [(and min) (and (<= min nat) (index? nat) nat)]
                  [else (and (index? nat) nat)])))]
    [(v min max)
     (let ([nat (xml:attr-value->integer v)])
       (and nat
            (cond [(and min max) (and (<= min nat) (<= nat max) nat)]
                  [(and min) (and (<= min nat) (index? nat) nat)]
                  [(and max) (and (<= 0 nat) (<= nat max) nat)]
                  [else (and (index? nat) nat)])))]))

(define xml:attr-value->natural : (case-> [XML-Element-Attribute-Value -> (Option Natural)]
                                                 [XML-Element-Attribute-Value (Option Natural) -> (Option Natural)]
                                                 [XML-Element-Attribute-Value (Option Natural) (Option Natural) -> (Option Natural)])
  (case-lambda
    [(v)
     (cond [(string? v) (string->natural (string-trim v))]
           [(symbol? v) (string->natural (symbol->immutable-string v))]
           [else #false])]
    [(v min)
     (let ([nat (xml:attr-value->integer v)])
       (and nat
            (cond [(and min) (and (<= min nat) nat)]
                  [else (and (<= 0 nat) nat)])))]
    [(v min max)
     (let ([nat (xml:attr-value->natural v)])
       (and nat
            (cond [(and min max) (and (<= min nat) (<= nat max) nat)]
                  [(and min) (and (<= min nat) nat)]
                  [(and max) (and (<= 0 nat) (<= nat max) nat)]
                  [else nat])))]))

(define xml:attr-value->flonum : (case-> [XML-Element-Attribute-Value -> (Option Flonum)]
                                         [XML-Element-Attribute-Value (Option Flonum) -> (Option Flonum)]
                                         [XML-Element-Attribute-Value (Option Flonum) (Option Flonum) -> (Option Flonum)])
  (case-lambda
    [(v)
     (cond [(string? v) (string->flonum (string-trim v))]
           [(symbol? v) (string->flonum (symbol->immutable-string v))]
           [else #false])]
    [(v min)
     (let ([n (xml:attr-value->flonum v)])
       (and n
            (cond [(and min) (and (<= min n) n)]
                  [else n])))]
    [(v min max)
     (let ([n (xml:attr-value->flonum v)])
       (and n
            (cond [(and min max) (and (<= min n) (<= n max) n)]
                  [(and min) (and (<= min n) n)]
                  [(and max) (and (<= n max) n)]
                  [else n])))]))

(define xml:attr-value+>flonum : (case-> [XML-Element-Attribute-Value -> (Option Nonnegative-Flonum)]
                                         [XML-Element-Attribute-Value (Option Nonnegative-Flonum) -> (Option Nonnegative-Flonum)]
                                         [XML-Element-Attribute-Value (Option Nonnegative-Flonum) (Option Nonnegative-Flonum) -> (Option Nonnegative-Flonum)])
  (case-lambda
    [(v)
     (cond [(string? v) (string+>flonum (string-trim v))]
           [(symbol? v) (string+>flonum (symbol->immutable-string v))]
           [else #false])]
    [(v min)
     (let ([n (xml:attr-value->flonum v)])
       (and n
            (cond [(and min) (and (<= min n) n)]
                  [else (and (<= 0 n) n)])))]
    [(v min max)
     (let ([n (xml:attr-value->flonum v)])
       (and n
            (cond [(and min max) (and (<= min n) (<= n max) n)]
                  [(and min) (and (<= min n) n)]
                  [(and max) (and (>= n 0.0) (<= n max) n)]
                  [else (and (>= n 0.0) n)])))]))

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

(define xml:attr-value->percentage : (->* (XML-Element-Attribute-Value) (Symbol) (Option XML-Percentage))
  (lambda [v [canonical-unit '||]]
    (define-values (n unit)
      (cond [(string? v) (string->dimension v canonical-unit #:ci? #false)]
            [(symbol? v) (string->dimension v canonical-unit #:ci? #false)]
            [else (values #false canonical-unit)]))

    (and n (eq? unit '%) (cons n unit))))

(define xml:attr-value+>percentage : (->* (XML-Element-Attribute-Value) (Symbol) (Option XML-Nonnegative-Dimension))
  (lambda [v [canonical-unit '||]]
    (define-values (n unit)
      (cond [(string? v) (string->dimension v canonical-unit #:ci? #false)]
            [(symbol? v) (string->dimension v canonical-unit #:ci? #false)]
            [else (values #false canonical-unit)]))

    (and n (>= n 0.0) (eq? unit '%) (cons n unit))))

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

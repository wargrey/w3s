#lang typed/racket/base

(provide (all-defined-out))
(provide (all-from-out "shared/datatype.rkt"))
(provide (rename-out [xml:attr-value*->natural xml:attr-value*+>integer]
                     [xml:attr-value*->boolean xml:attr-value*->on-off]
                     [xml:attr-value*->symbol xml:attr-value*->name]))

(require digimon/symbol)
(require digimon/string)
(require digimon/dimension)
(require digimon/number)

(require "grammar.rkt")
(require "digicore.rkt")

(require "shared/enum.rkt")
(require "shared/datatype.rkt")

(require (for-syntax racket/base))
(require (for-syntax racket/syntax))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; NOTE
; Normalizing the attributes' values require DTD or other schema,
; These APIs are designed to manually normalizing,
; Thus, the input tokens are usually XML:String instances.

(define-type (XML-Attribute-Value*->Datum T) (-> XML-Element-Attribute-Value* T))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax (define-xml-enumeration* stx)
  (syntax-case stx [:]
    [(_ id : TypeU [enum ...])
     (with-syntax ([xml:attr->id (format-id #'id "xml:attr-value->~a" #'id)]
                   [xml:attr*->id (format-id #'id "xml:attr-value*->~a" #'id)])
       (syntax/loc stx
         (begin (define-xml-enumeration id : TypeU [enum ...])
                
                (define xml:attr*->id : (-> XML-Element-Attribute-Value* (Option TypeU))
                  (lambda [v]
                    (cond [(xml:string? v) (xml:attr->id (xml:string-datum v))]
                          [(xml:name? v) (xml:attr->id (xml:name-datum v))]
                          [else #false]))))))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml:attr-value*->string
  : (case-> [XML-Element-Attribute-Value* -> String]
            [XML-Element-Attribute-Value* (U String Bytes Regexp Byte-Regexp (-> String Boolean)) -> (Option String)])
  (case-lambda
    [(v)
     (cond [(xml:string? v) (xml:string-datum v)]
           [(xml:name? v) (symbol->immutable-string (xml:name-datum v))]
           [else (symbol-join (map xml:name-datum v))])]
    [(v pattern)
     (let ([s (xml:attr-value*->string v)])
       (cond [(procedure? pattern) (and (pattern s) s)]
             [else (and (regexp-match? pattern s) s)]))]))

(define xml:attr-value*->string/trim
  : (case-> [XML-Element-Attribute-Value* -> String]
            [XML-Element-Attribute-Value* (U String Bytes Regexp Byte-Regexp (-> String Boolean)) -> (Option String)])
  (case-lambda
    [(v)
     (cond [(xml:string? v) (string-trim (xml:string-datum v))]
           [(xml:name? v) (symbol->immutable-string (xml:name-datum v))]
           [else (symbol-join (map xml:name-datum v))])]
    [(v pattern)
     (let ([s (xml:attr-value*->string/trim v)])
       (cond [(procedure? pattern) (and (pattern s) s)]
             [else (and (regexp-match? pattern s) s)]))]))

(define xml:attr-value*->string-list : (->* (XML-Element-Attribute-Value*) ((U String Regexp)) (Option (Listof String)))
  (lambda [v [sep #px"\\s+"]]
    (cond [(xml:string? v) (string-split (string-trim (xml:string-datum v)) sep)]
          [(list? v) (for/list : (Listof String) ([n (in-list v)]) (symbol->immutable-string (xml:name-datum n)))]
          [(xml:name? v) (list (symbol->immutable-string (xml:name-datum v)))]
          [else #false])))

(define xml:attr-value*->boolean : (-> XML-Element-Attribute-Value* (Option XML-Boolean))
  (lambda [v]
    (cond [(xml:string? v) (let ([datum (xml:string-datum v)]) (cond [(member datum '("true" "1")) 'true] [(member datum '("false" "0")) 'false] [else #false]))]
          [(xml:name? v) (let ([datum (xml:name-datum v)]) (and (or (eq? v 'true) (eq? v 'false)) datum))]
          [else #false])))

(define xml:attr-value*->integer : (case-> [XML-Element-Attribute-Value* -> (Option Integer)]
                                           [XML-Element-Attribute-Value* (Option Integer) -> (Option Integer)]
                                           [XML-Element-Attribute-Value* (Option Integer) (Option Integer) -> (Option Integer)])
  (case-lambda
    [(v)
     (cond [(xml:string? v) (string->integer (string-trim (xml:string-datum v)))]
           [(xml:name? v) (string->integer (symbol->immutable-string (xml:name-datum v)))]
           [else #false])]
    [(v min)
     (let ([n (xml:attr-value*->integer v)])
       (and n
            (cond [(and min) (and (<= min n) n)]
                  [else n])))]
    [(v min max)
     (let ([n (xml:attr-value*->integer v)])
       (and n
            (cond [(and min max) (and (<= min n max) n)]
                  [(and min) (and (<= min n) n)]
                  [(and max) (and (<= n max) n)]
                  [else n])))]))

(define xml:attr-value*->fixnum : (case-> [XML-Element-Attribute-Value* -> (Option Fixnum)]
                                          [XML-Element-Attribute-Value* (Option Fixnum) -> (Option Fixnum)]
                                          [XML-Element-Attribute-Value* (Option Fixnum) (Option Fixnum) -> (Option Fixnum)])
  (case-lambda
    [(v)
     (cond [(xml:string? v) (string->fixnum (string-trim (xml:string-datum v)))]
           [(xml:name? v) (string->fixnum (symbol->immutable-string (xml:name-datum v)))]
           [else #false])]
    [(v min)
     (let ([n (xml:attr-value*->fixnum v)])
       (and n
            (cond [(and min) (and (<= min n) n)]
                  [else n])))]
    [(v min max)
     (let ([n (xml:attr-value*->fixnum v)])
       (and n
            (cond [(and min max) (and (<= min n max) n)]
                  [(and min) (and (<= min n) n)]
                  [(and max) (and (<= n max) n)]
                  [else n])))]))

(define xml:attr-value*+>fixnum : (case-> [XML-Element-Attribute-Value* -> (Option Nonnegative-Fixnum)]
                                          [XML-Element-Attribute-Value* (Option Nonnegative-Fixnum) -> (Option Nonnegative-Fixnum)]
                                          [XML-Element-Attribute-Value* (Option Nonnegative-Fixnum) (Option Nonnegative-Fixnum) -> (Option Nonnegative-Fixnum)])
  (case-lambda
    [(v)
     (cond [(xml:string? v) (string+>fixnum (string-trim (xml:string-datum v)))]
           [(xml:name? v) (string+>fixnum (symbol->immutable-string (xml:name-datum v)))]
           [else #false])]
    [(v min)
     (let ([n (xml:attr-value*->fixnum v)])
       (and n
            (cond [(and min) (and (<= min n) n)]
                  [else (and (<= 0 n) n)])))]
    [(v min max)
     (let ([n (xml:attr-value*->fixnum v)])
       (and n
            (cond [(and min max) (and (<= min n) (<= n max) n)]
                  [(and min) (and (<= min n) n)]
                  [(and max) (and (<= 0 n) (<= n max) n)]
                  [else (and (<= 0 n) n)])))]))

(define xml:attr-value*->index : (case-> [XML-Element-Attribute-Value* -> (Option Index)]
                                         [XML-Element-Attribute-Value* (Option Index) -> (Option Index)]
                                         [XML-Element-Attribute-Value* (Option Index) (Option Index) -> (Option Index)])
  (case-lambda
    [(v)
     (cond [(xml:string? v) (string->index (string-trim (xml:string-datum v)))]
           [(xml:name? v) (string->index (symbol->immutable-string (xml:name-datum v)))]
           [else #false])]
    [(v min)
     (let ([nat (xml:attr-value*->integer v)])
       (and nat
            (cond [(and min) (and (<= min nat) (index? nat) nat)]
                  [else (and (index? nat) nat)])))]
    [(v min max)
     (let ([nat (xml:attr-value*->integer v)])
       (and nat
            (cond [(and min max) (and (<= min nat) (<= nat max) nat)]
                  [(and min) (and (<= min nat) (index? nat) nat)]
                  [(and max) (and (<= 0 nat) (<= nat max) nat)]
                  [else (and (index? nat) nat)])))]))

(define xml:attr-value*->hexdecimal : (case-> [XML-Element-Attribute-Value* -> (Option Index)]
                                              [XML-Element-Attribute-Value* Positive-Byte -> (Option Index)])
  (case-lambda
    [(v)
     (cond [(xml:string? v) (string->index (string-trim (xml:string-datum v)) 16)]
           [(xml:name? v) (string->index (symbol->immutable-string (xml:name-datum v)) 16)]
           [else #false])]
    [(v len)
     (let ([hex (xml:attr-value*->string v)])
       (and (= (string-length hex) len)
            (string->index hex 16)))]))

(define xml:attr-value*->natural : (case-> [XML-Element-Attribute-Value* -> (Option Natural)]
                                           [XML-Element-Attribute-Value* (Option Natural) -> (Option Natural)]
                                           [XML-Element-Attribute-Value* (Option Natural) (Option Natural) -> (Option Natural)])
  (case-lambda
    [(v)
     (cond [(xml:string? v) (string->natural (string-trim (xml:string-datum v)))]
           [(xml:name? v) (string->natural (symbol->immutable-string (xml:name-datum v)))]
           [else #false])]
    [(v min)
     (let ([nat (xml:attr-value*->integer v)])
       (and nat
            (cond [(and min) (and (<= min nat) nat)]
                  [else (and (<= 0 nat) nat)])))]
    [(v min max)
     (let ([nat (xml:attr-value*->natural v)])
       (and nat
            (cond [(and min max) (and (<= min nat) (<= nat max) nat)]
                  [(and min) (and (<= min nat) nat)]
                  [(and max) (and (<= 0 nat) (<= nat max) nat)]
                  [else nat])))]))

(define xml:attr-value*->flonum : (case-> [XML-Element-Attribute-Value* -> (Option Flonum)]
                                         [XML-Element-Attribute-Value* (Option Flonum) -> (Option Flonum)]
                                         [XML-Element-Attribute-Value* (Option Flonum) (Option Flonum) -> (Option Flonum)])
  (case-lambda
    [(v)
     (cond [(xml:string? v) (string->flonum (string-trim (xml:string-datum v)))]
           [(xml:name? v) (string->flonum (symbol->immutable-string (xml:name-datum v)))]
           [else #false])]
    [(v min)
     (let ([n (xml:attr-value*->flonum v)])
       (and n
            (cond [(and min) (and (<= min n) n)]
                  [else n])))]
    [(v min max)
     (let ([n (xml:attr-value*->flonum v)])
       (and n
            (cond [(and min max) (and (<= min n) (<= n max) n)]
                  [(and min) (and (<= min n) n)]
                  [(and max) (and (<= n max) n)]
                  [else n])))]))

(define xml:attr-value*+>flonum : (case-> [XML-Element-Attribute-Value* -> (Option Nonnegative-Flonum)]
                                          [XML-Element-Attribute-Value* (Option Nonnegative-Flonum) -> (Option Nonnegative-Flonum)]
                                          [XML-Element-Attribute-Value* (Option Nonnegative-Flonum) (Option Nonnegative-Flonum) -> (Option Nonnegative-Flonum)])
  (case-lambda
    [(v)
     (cond [(xml:string? v) (string+>flonum (string-trim (xml:string-datum v)))]
           [(xml:name? v) (string+>flonum (symbol->immutable-string (xml:name-datum v)))]
           [else #false])]
    [(v min)
     (let ([n (xml:attr-value*->flonum v)])
       (and n
            (cond [(and min) (and (<= min n) n)]
                  [else (and (<= 0 n) n)])))]
    [(v min max)
     (let ([n (xml:attr-value*->flonum v)])
       (and n
            (cond [(and min max) (and (<= min n) (<= n max) n)]
                  [(and min) (and (<= min n) n)]
                  [(and max) (and (>= n 0.0) (<= n max) n)]
                  [else (and (>= n 0.0) n)])))]))

(define xml:attr-value*->number : (-> XML-Element-Attribute-Value* (Option Real))
  (lambda [v]
    (cond [(xml:string? v) (string->real (string-trim (xml:string-datum v)))]
          [(xml:name? v) (string->real (symbol->immutable-string (xml:name-datum v)))]
          [else #false])))

(define xml:attr-value*+>number : (-> XML-Element-Attribute-Value* (Option Nonnegative-Real))
  (lambda [v]
    (cond [(xml:string? v) (string+>real (string-trim (xml:string-datum v)))]
          [(xml:name? v) (string+>real (symbol->immutable-string (xml:name-datum v)))]
          [else #false])))

(define xml:attr-value*->symbol : (-> XML-Element-Attribute-Value* Symbol)
  (lambda [v]
    (cond [(xml:string? v) (string->symbol (string-trim (xml:string-datum v)))]
          [(xml:name? v) (symbol->interned-symbol (xml:name-datum v))]
          [else (string->symbol (symbol-join (map xml:name-datum v)))])))

(define xml:attr-value*->unreadable-symbol : (-> XML-Element-Attribute-Value* Symbol)
  (lambda [v]
    (cond [(xml:string? v) (string->unreadable-symbol (string-trim (xml:string-datum v)))]
          [(xml:name? v) (symbol->unreadable-symbol (xml:name-datum v))]
          [else (string->unreadable-symbol (symbol-join (map xml:name-datum v)))])))

(define xml:attr-value*->symbol-list : (->* (XML-Element-Attribute-Value*) ((U String Regexp)) (Option (Listof Symbol)))
  (lambda [v [sep #px"\\s+"]]
    (cond [(xml:string? v) (map string->symbol (string-split (string-trim (xml:string-datum v)) sep))]
          [(list? v) (map xml:name-datum v)]
          [(xml:name? v) (list (xml:name-datum v))]
          [else #false])))

(define xml:attr-value*->unreadable-symbol-list : (->* (XML-Element-Attribute-Value*) ((U String Regexp)) (Option (Listof Symbol)))
  (lambda [v [sep #px"\\s+"]]
    (cond [(xml:string? v) (map string->unreadable-symbol (string-split (xml:string-datum v) sep))]
          [(list? v) (map symbol->unreadable-symbol (map xml:name-datum v))]
          [(xml:name? v) (list (symbol->unreadable-symbol (xml:name-datum v)))]
          [else #false])))

(define xml:attr-value*->keyword : (-> XML-Element-Attribute-Value* Keyword)
  (lambda [v]
    (cond [(xml:string? v) (string->keyword (string-trim (xml:string-datum v)))]
          [(xml:name? v) (symbol->keyword (xml:name-datum v))]
          [else (string->keyword (string-join (map symbol->immutable-string (map xml:name-datum v))))])))

(define xml:attr-value*->dimension : (->* (XML-Element-Attribute-Value*) (Symbol) (Option XML-Dimension))
  (lambda [v [canonical-unit '||]]
    (define-values (n unit)
      (cond [(xml:string? v) (string->dimension (xml:string-datum v) canonical-unit #:ci? #false)]
            [(xml:name? v) (string->dimension (xml:name-datum v) canonical-unit #:ci? #false)]
            [else (values #false canonical-unit)]))

    (and n (cons n unit))))

(define xml:attr-value*+>dimension : (->* (XML-Element-Attribute-Value*) (Symbol) (Option XML-Nonnegative-Dimension))
  (lambda [v [canonical-unit '||]]
    (define-values (n unit)
      (cond [(xml:string? v) (string->dimension (xml:string-datum v) canonical-unit #:ci? #false)]
            [(xml:name? v) (string->dimension (xml:name-datum v) canonical-unit #:ci? #false)]
            [else (values #false canonical-unit)]))

    (and n (>= n 0.0) (cons n unit))))

(define xml:attr-value*->percentage : (->* (XML-Element-Attribute-Value*) (Symbol) (Option XML-Percentage))
  (lambda [v [canonical-unit '||]]
    (define-values (n unit)
      (cond [(xml:string? v) (string->dimension (xml:string-datum v) canonical-unit #:ci? #false)]
            [(xml:name? v) (string->dimension (xml:name-datum v) canonical-unit #:ci? #false)]
            [else (values #false canonical-unit)]))

    (and n (eq? unit '%) (cons n unit))))

(define xml:attr-value*+>percentage : (->* (XML-Element-Attribute-Value*) (Symbol) (Option XML-Nonnegative-Dimension))
  (lambda [v [canonical-unit '||]]
    (define-values (n unit)
      (cond [(xml:string? v) (string->dimension (xml:string-datum v) canonical-unit #:ci? #false)]
            [(xml:name? v) (string->dimension (xml:name-datum v) canonical-unit #:ci? #false)]
            [else (values #false canonical-unit)]))

    (and n (>= n 0.0) (eq? unit '%) (cons n unit))))

(define xml:attr-value*->dimension/ci : (->* (XML-Element-Attribute-Value*) (Symbol) (Option XML-Dimension))
  (lambda [v [canonical-unit '||]]
    (define-values (n unit)
      (cond [(xml:string? v) (string->dimension (xml:string-datum v) canonical-unit #:ci? #true)]
            [(xml:name? v) (string->dimension (xml:name-datum v) canonical-unit #:ci? #true)]
            [else (values #false canonical-unit)]))

    (and n (cons n unit))))

(define xml:attr-value*+>dimension/ci : (->* (XML-Element-Attribute-Value*) (Symbol) (Option XML-Nonnegative-Dimension))
  (lambda [v [canonical-unit '||]]
    (define-values (n unit)
      (cond [(xml:string? v) (string->dimension (xml:string-datum v) canonical-unit #:ci? #true)]
            [(xml:name? v) (string->dimension (xml:name-datum v) canonical-unit #:ci? #true)]
            [else (values #false canonical-unit)]))

    (and n (>= n 0.0) (cons n unit))))

(define xml:attr-value*->listof-type : (All (T) (->* (XML-Element-Attribute-Value*
                                                      (-> XML-Element-Attribute-Value* (XML-Option T))
                                                      (->* (XML-Element-Attribute-Value*) ((Option XML-Token) (Option Log-Level)) exn))
                                                     ((U String Regexp (-> XML-Token String (Listof String))))
                                                     (Listof T)))
  (lambda [v string->datum make-exn:range [sep #px"\\s+"]]
    (cond [(xml:string? v)
           (let parse ([vs : (Listof String) (if (procedure? sep) (sep v (xml:string-datum v)) (string-split (xml:string-datum v) sep))]
                       [ts : (Listof T) null])
             (cond [(null? vs) (reverse ts)]
                   [else (let*-values ([(self rest) (values (syn-remake-token v xml:string (car vs)) (cdr vs))]
                                       [(datum) (string->datum self)])
                           (cond [(not datum) (make-exn:range self) (parse rest ts)]
                                 [(exn? datum) (parse rest ts)]
                                 [else (parse rest (cons datum ts))]))]))]
          [(list? v)
           (let parse ([vs : (Listof XML:Name) v]
                       [ts : (Listof T) null])
             (cond [(null? vs) (reverse ts)]
                   [else (let*-values ([(self rest) (values (car vs) (cdr vs))]
                                       [(self) (syn-remake-token self xml:string (symbol->immutable-string (xml:name-datum self)))]
                                       [(datum) (string->datum self)])
                           (cond [(not datum) (make-exn:range self) (parse rest ts)]
                                 [(exn? datum) (parse rest ts)]
                                 [else (parse rest (cons datum ts))]))]))]
          [(xml:name? v)
           (let ([datum (string->datum (syn-remake-token v xml:string (symbol->immutable-string (xml:name-datum v))))])
             (cond [(not datum) (make-exn:range v) null]
                   [(exn:xml? datum) null]
                   [else (list datum)]))]
          [else null])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml:attr-value*->guid-uri : (-> XML-Element-Attribute-Value* (Option String))
  (lambda [v]
    (xml:attr-value*->string/trim v string-uri?)))

(define xml:attr-value*->guid-string : (-> XML-Element-Attribute-Value* (Option String))
  (lambda [v]
    (xml:attr-value*->string/trim v string-guid?)))

(define xml:attr-value*->panose-string : (-> XML-Element-Attribute-Value* (Option String))
  (lambda [v]
    (xml:attr-value*->string/trim v string-panose?)))

(define xml:attr-value*->panose : (-> XML-Element-Attribute-Value* (Option Index))
  (lambda [v]
    (define s (xml:attr-value*->panose-string v))
    (and s (string->index s 16))))

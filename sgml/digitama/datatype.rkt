#lang typed/racket/base

(provide (all-defined-out))
(provide (rename-out [xml:attr-value*->natural xml:attr-value*+>integer]
                     [xml:attr-value*->boolean xml:attr-value*->on-off]
                     [xml:attr-value*->symbol xml:attr-value*->name]
                     [xml:attr-value*+>flonum xml:attr-value*->nonnegative-flonum]
                     [xml:attr-value*+>fixnum xml:attr-value*->nonnegative-fixnum]))

(require digimon/symbol)
(require digimon/string)
(require digimon/number)

(require "grammar.rkt")
(require "digicore.rkt")

(require "shared/enum.rkt")
(require "shared/datatype.rkt")

(require (for-syntax racket/base))
(require (for-syntax racket/syntax))
(require (for-syntax syntax/parse))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; NOTE
; Normalizing the attributes' values require DTD or other schema,
; These APIs are designed to manually normalizing,
; Thus, the input tokens are usually XML:String instances.

(define-type (XML-Attribute-Value*->Datum T) (-> XML-Element-Attribute-Value* T))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax (define-xml-enumeration* stx)
  (syntax-parse stx #:literals [:]
    [(_ id : TypeU (~optional (~seq #:for prefix) #:defaults ([prefix #'xml])) [enum ...])
     (with-syntax ([xml:attr->id (format-id #'id "xml:attr-value->~a" #'id)]
                   [xml:attr*->id (format-id #'id "xml:attr-value*->~a" #'id)])
       (syntax/loc stx
         (begin (define-xml-enumeration id : TypeU #:for prefix [enum ...])
                
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
           [(xml:name? v) (xml:attr-symbol->string (xml:name-datum v))]
           [else (xml:attr-list->string (map xml:name-datum v))])]
    [(v pattern) (xml:attr-string-filter (xml:attr-value*->string v) pattern)]))

(define xml:attr-value*->token
  : (case-> [XML-Element-Attribute-Value* -> String]
            [XML-Element-Attribute-Value* (U String Bytes Regexp Byte-Regexp (-> String Boolean)) -> (Option String)])
  (case-lambda
    [(v)
     (cond [(xml:string? v) (xml:attr-string->token (xml:string-datum v))]
           [(xml:name? v) (xml:attr-symbol->token (xml:name-datum v))]
           [else (xml:attr-list->token (map xml:name-datum v))])]
    [(v pattern) (xml:attr-string-filter (xml:attr-value*->token v) pattern)]))

(define xml:attr-value*->string-list : (->* (XML-Element-Attribute-Value*) ((U String Regexp)) (Listof String))
  (lambda [v [sep #px"\\s+"]]
    (cond [(xml:string? v) ((inst xml:attr-string->list String) (xml:string-datum v) values sep)]
          [(list? v) ((inst xml:attr-list->list String) (map xml:name-datum v) values)]
          [else ((inst xml:attr-symbol->list String) (xml:name-datum v) values)])))

(define xml:attr-value*->boolean : (-> XML-Element-Attribute-Value* (Option XML-Boolean))
  (lambda [v]
    (cond [(xml:string? v) (xml:attr-string->boolean (xml:string-datum v))]
          [(xml:name? v) (xml:attr-symbol->boolean (xml:name-datum v))]
          [else #false])))

(define xml:attr-value*->integer : (case-> [XML-Element-Attribute-Value* -> (Option Integer)]
                                           [XML-Element-Attribute-Value* (Option Integer) -> (Option Integer)]
                                           [XML-Element-Attribute-Value* (Option Integer) (Option Integer) -> (Option Integer)])
  (case-lambda
    [(v)
     (cond [(xml:string? v) (xml:attr-string->real (xml:string-datum v) string->integer)]
           [(xml:name? v) (xml:attr-symbol->real (xml:name-datum v) string->integer)]
           [else #false])]
    [(v min) (xml:attr-real-filter (xml:attr-value*->integer v) min)]
    [(v min max) (xml:attr-real-filter (xml:attr-value*->integer v) min max)]))

(define xml:attr-value*->fixnum : (case-> [XML-Element-Attribute-Value* -> (Option Fixnum)]
                                          [XML-Element-Attribute-Value* (Option Fixnum) -> (Option Fixnum)]
                                          [XML-Element-Attribute-Value* (Option Fixnum) (Option Fixnum) -> (Option Fixnum)])
  (case-lambda
    [(v)
     (cond [(xml:string? v) (xml:attr-string->real (xml:string-datum v) string->fixnum)]
           [(xml:name? v) (xml:attr-symbol->real (xml:name-datum v) string->fixnum)]
           [else #false])]
    [(v min) (xml:attr-real-filter (xml:attr-value*->fixnum v) min)]
    [(v min max) (xml:attr-real-filter (xml:attr-value*->fixnum v) min max)]))

(define xml:attr-value*+>fixnum : (case-> [XML-Element-Attribute-Value* -> (Option Nonnegative-Fixnum)]
                                          [XML-Element-Attribute-Value* (Option Nonnegative-Fixnum) -> (Option Nonnegative-Fixnum)]
                                          [XML-Element-Attribute-Value* (Option Nonnegative-Fixnum) (Option Nonnegative-Fixnum) -> (Option Nonnegative-Fixnum)])
  (case-lambda
    [(v)
     (cond [(xml:string? v) (xml:attr-string->real (xml:string-datum v) string+>fixnum)]
           [(xml:name? v) (xml:attr-symbol->real (xml:name-datum v) string+>fixnum)]
           [else #false])]
    [(v min) (xml:attr-real-filter (xml:attr-value*+>fixnum v) min)]
    [(v min max) (xml:attr-real-filter (xml:attr-value*+>fixnum v) min max)]))

(define xml:attr-value*->byte : (case-> [XML-Element-Attribute-Value* -> (Option Byte)]
                                        [XML-Element-Attribute-Value* (Option Byte) -> (Option Byte)]
                                        [XML-Element-Attribute-Value* (Option Byte) (Option Byte) -> (Option Byte)])
  (case-lambda
    [(v)
     (cond [(xml:string? v) (xml:attr-string->real (xml:string-datum v) string->byte)]
           [(xml:name? v) (xml:attr-symbol->real (xml:name-datum v) string->byte)]
           [else #false])]
    [(v min) (xml:attr-real-filter (xml:attr-value*->byte v) min)]
    [(v min max) (xml:attr-real-filter (xml:attr-value*->byte v) min max)]))

(define xml:attr-value*->index : (case-> [XML-Element-Attribute-Value* -> (Option Index)]
                                         [XML-Element-Attribute-Value* (Option Index) -> (Option Index)]
                                         [XML-Element-Attribute-Value* (Option Index) (Option Index) -> (Option Index)])
  (case-lambda
    [(v)
     (cond [(xml:string? v) (xml:attr-string->real (xml:string-datum v) string->index)]
           [(xml:name? v) (xml:attr-symbol->real (xml:name-datum v) string->index)]
           [else #false])]
    [(v min) (xml:attr-real-filter (xml:attr-value*->index v) min)]
    [(v min max) (xml:attr-real-filter (xml:attr-value*->index v) min max)]))

(define xml:attr-value*->natural : (case-> [XML-Element-Attribute-Value* -> (Option Natural)]
                                           [XML-Element-Attribute-Value* (Option Natural) -> (Option Natural)]
                                           [XML-Element-Attribute-Value* (Option Natural) (Option Natural) -> (Option Natural)])
  (case-lambda
    [(v)
     (cond [(xml:string? v) (xml:attr-string->real (xml:string-datum v) string->natural)]
           [(xml:name? v) (xml:attr-symbol->real (xml:name-datum v) string->natural)]
           [else #false])]
    [(v min) (xml:attr-real-filter (xml:attr-value*->natural v) min)]
    [(v min max) (xml:attr-real-filter (xml:attr-value*->natural v) min max)]))

(define xml:attr-value*->hexadecimal : (case-> [XML-Element-Attribute-Value* -> (Option Index)]
                                               [XML-Element-Attribute-Value* Positive-Byte -> (Option Index)])
  (case-lambda
    [(v)
     (cond [(xml:string? v) (xml:attr-string->real (xml:string-datum v) string->index 16)]
           [(xml:name? v) (xml:attr-symbol->real (xml:name-datum v) string->index 16)]
           [else #false])]
    [(v len) (xml:attr-hexadecimal-filter (xml:attr-value*->string v) len)]))

(define xml:attr-value*->flonum : (case-> [XML-Element-Attribute-Value* -> (Option Flonum)]
                                          [XML-Element-Attribute-Value* (Option Flonum) -> (Option Flonum)]
                                          [XML-Element-Attribute-Value* (Option Flonum) (Option Flonum) -> (Option Flonum)])
  (case-lambda
    [(v)
     (cond [(xml:string? v) (xml:attr-string->real (xml:string-datum v) string->flonum)]
           [(xml:name? v) (xml:attr-symbol->real (xml:name-datum v) string->flonum)]
           [else #false])]
    [(v min) (xml:attr-real-filter (xml:attr-value*->flonum v) min)]
    [(v min max) (xml:attr-real-filter (xml:attr-value*->flonum v) min max)]))

(define xml:attr-value*+>flonum : (case-> [XML-Element-Attribute-Value* -> (Option Nonnegative-Flonum)]
                                          [XML-Element-Attribute-Value* (Option Nonnegative-Flonum) -> (Option Nonnegative-Flonum)]
                                          [XML-Element-Attribute-Value* (Option Nonnegative-Flonum) (Option Nonnegative-Flonum) -> (Option Nonnegative-Flonum)])
  (case-lambda
    [(v)
     (cond [(xml:string? v) (xml:attr-string->real (xml:string-datum v) string+>flonum)]
           [(xml:name? v) (xml:attr-symbol->real (xml:name-datum v) string+>flonum)]
           [else #false])]
    [(v min) (xml:attr-real-filter (xml:attr-value*+>flonum v) min)]
    [(v min max) (xml:attr-real-filter (xml:attr-value*+>flonum v) min max)]))

(define #:forall (N) xml:attr-value*->number
  : (case-> [XML-Element-Attribute-Value* -> (Option Real)]
            [XML-Element-Attribute-Value* (-> Any Boolean : (∩ Real N)) -> (Option N)]
            [XML-Element-Attribute-Value* (-> Any Boolean : (∩ Real N)) (Option (∩ Real N)) (Option (∩ Real N)) -> (Option N)])
  (case-lambda
    [(v)
     (cond [(xml:string? v) (xml:attr-string->real (xml:string-datum v) string->real)]
           [(xml:name? v) (xml:attr-symbol->real (xml:name-datum v) string->real)]
           [else #false])]
    [(v number?) (let ([r (xml:attr-value*->number v)]) (and r (number? r) r))]
    [(v number? min max) ((inst xml:attr-real-filter N) (xml:attr-value*->number v) number? min max)]))

(define #:forall (N) xml:attr-value*+>number
  : (case-> [XML-Element-Attribute-Value* -> (Option Nonnegative-Real)]
            [XML-Element-Attribute-Value* (-> Any Boolean : (∩ Real N)) -> (Option N)]
            [XML-Element-Attribute-Value* (-> Any Boolean : (∩ Real N)) (Option (∩ Real N)) (Option (∩ Real N)) -> (Option N)])
  (case-lambda
    [(v)
     (cond [(xml:string? v) (xml:attr-string->real (xml:string-datum v) string+>real)]
           [(xml:name? v) (xml:attr-symbol->real (xml:name-datum v) string+>real)]
           [else #false])]
    [(v number?) (let ([r (xml:attr-value*+>number v)]) (and r (number? r) r))]
    [(v number? min max) ((inst xml:attr-real-filter N) (xml:attr-value*+>number v) number? min max)]))

(define xml:attr-value*->symbol : (-> XML-Element-Attribute-Value* Symbol)
  (lambda [v]
    (cond [(xml:string? v) (xml:attr-string->symbol (xml:string-datum v) string->symbol)]
          [(xml:name? v) (xml:attr-symbol->symbol (xml:name-datum v))]
          [else (xml:attr-list->symbol (map xml:name-datum v) string->symbol)])))

(define xml:attr-value*->unreadable-symbol : (-> XML-Element-Attribute-Value* Symbol)
  (lambda [v]
    (cond [(xml:string? v) (xml:attr-string->symbol (xml:string-datum v) string->unreadable-symbol)]
          [(xml:name? v) (xml:attr-symbol->unreadable-symbol (xml:name-datum v))]
          [else (xml:attr-list->symbol (map xml:name-datum v) string->unreadable-symbol)])))

(define xml:attr-value*->symbol-list : (->* (XML-Element-Attribute-Value*) ((U String Regexp)) (Listof Symbol))
  (lambda [v [sep #px"\\s+"]]
    (cond [(xml:string? v) (xml:attr-string->list (xml:string-datum v) string->symbol sep)]
          [(list? v) (xml:attr-list->list (map xml:name-datum v) string->symbol)]
          [else (xml:attr-symbol->list (xml:name-datum v) string->symbol)])))

(define xml:attr-value*->unreadable-symbol-list : (->* (XML-Element-Attribute-Value*) ((U String Regexp)) (Listof Symbol))
  (lambda [v [sep #px"\\s+"]]
    (cond [(xml:string? v) (xml:attr-string->list (xml:string-datum v) string->unreadable-symbol sep)]
          [(list? v) (xml:attr-list->list (map xml:name-datum v) string->unreadable-symbol)]
          [else (xml:attr-symbol->list (xml:name-datum v) string->unreadable-symbol)])))

(define xml:attr-value*->keyword : (-> XML-Element-Attribute-Value* Keyword)
  (lambda [v]
    (cond [(xml:string? v) (xml:attr-string->symbol (xml:string-datum v) string->keyword)]
          [(xml:name? v) (xml:attr-symbol->keyword (xml:name-datum v))]
          [else (xml:attr-list->symbol (map xml:name-datum v) string->keyword)])))

(define xml:attr-value*->dimension : (->* (XML-Element-Attribute-Value*) (Symbol) (Option XML-Dimension))
  (lambda [v [canonical-unit '||]]
    (cond [(xml:string? v) (xml:attr-string->dimension (xml:string-datum v) canonical-unit #false #false)]
          [(xml:name? v) (xml:attr-symbol->dimension (xml:name-datum v) canonical-unit #false #false)]
          [else #false])))

(define xml:attr-value*+>dimension : (->* (XML-Element-Attribute-Value*) (Symbol) (Option XML-Nonnegative-Dimension))
  (lambda [v [canonical-unit '||]]
    (cond [(xml:string? v) (xml:attr-string->dimension (xml:string-datum v) canonical-unit #false nonnegative-flonum?)]
          [(xml:name? v) (xml:attr-symbol->dimension (xml:name-datum v) canonical-unit #false nonnegative-flonum?)]
          [else #false])))

(define xml:attr-value*->dimension/ci : (->* (XML-Element-Attribute-Value*) (Symbol) (Option XML-Dimension))
  (lambda [v [canonical-unit '||]]
    (cond [(xml:string? v) (xml:attr-string->dimension (xml:string-datum v) canonical-unit #true #false)]
          [(xml:name? v) (xml:attr-symbol->dimension (xml:name-datum v) canonical-unit #true #false)]
          [else #false])))

(define xml:attr-value*+>dimension/ci : (->* (XML-Element-Attribute-Value*) (Symbol) (Option XML-Nonnegative-Dimension))
  (lambda [v [canonical-unit '||]]
    (cond [(xml:string? v) (xml:attr-string->dimension (xml:string-datum v) canonical-unit #true nonnegative-flonum?)]
          [(xml:name? v) (xml:attr-symbol->dimension (xml:name-datum v) canonical-unit #true nonnegative-flonum?)]
          [else #false])))

(define xml:attr-value*->percentage : (->* (XML-Element-Attribute-Value*) (Symbol) (Option XML-Percentage))
  (lambda [v [canonical-unit '||]]
    (cond [(xml:string? v) (xml:attr-string->percentage (xml:string-datum v) canonical-unit #false)]
          [(xml:name? v) (xml:attr-symbol->percentage (xml:name-datum v) canonical-unit #false)]
          [else #false])))

(define xml:attr-value*+>percentage : (->* (XML-Element-Attribute-Value*) (Symbol) (Option XML-Nonnegative-Percentage))
  (lambda [v [canonical-unit '||]]
    (cond [(xml:string? v) (xml:attr-string->percentage (xml:string-datum v) canonical-unit nonnegative-flonum?)]
          [(xml:name? v) (xml:attr-symbol->percentage (xml:name-datum v) canonical-unit nonnegative-flonum?)]
          [else #false])))

(define xml:attr-value*->fixed-percentage : (->* (XML-Element-Attribute-Value*) (Symbol) (Option XML-Percentage))
  (lambda [v [canonical-unit '||]]
    (cond [(xml:string? v) (xml:attr-string->percentage (xml:string-datum v) canonical-unit fixed-percentage?)]
          [(xml:name? v) (xml:attr-symbol->percentage (xml:name-datum v) canonical-unit fixed-percentage?)]
          [else #false])))

(define xml:attr-value*+>fixed-percentage : (->* (XML-Element-Attribute-Value*) (Symbol) (Option XML-Nonnegative-Percentage))
  (lambda [v [canonical-unit '||]]
    (cond [(xml:string? v) (xml:attr-string->percentage (xml:string-datum v) canonical-unit nonnegative-fixed-percentage?)]
          [(xml:name? v) (xml:attr-symbol->percentage (xml:name-datum v) canonical-unit nonnegative-fixed-percentage?)]
          [else #false])))

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
(define xml:attr-value*->uri-string : (-> XML-Element-Attribute-Value* (Option String))
  (lambda [v]
    (xml:attr-value*->token v string-uri?)))

(define xml:attr-value*->guid-string : (-> XML-Element-Attribute-Value* (Option String))
  (lambda [v]
    (xml:attr-value*->token v string-guid?)))

(define xml:attr-value*->panose-string : (-> XML-Element-Attribute-Value* (Option String))
  (lambda [v]
    (xml:attr-value*->token v string-panose?)))

(define xml:attr-value*->panose : (-> XML-Element-Attribute-Value* (Option Index))
  (lambda [v]
    (define s (xml:attr-value*->panose-string v))
    (and s (string->index s 16))))

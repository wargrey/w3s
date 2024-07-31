#lang typed/racket/base

(provide (all-defined-out))
(provide (all-from-out "../shared/enum.rkt"))
(provide (rename-out [xml:attr-value->natural xml:attr-value+>integer]
                     [xml:attr-value->boolean xml:attr-value->on-off]
                     [xml:attr-value->symbol xml:attr-value->name]
                     [xml:attr-value+>flonum xml:attr-value->nonnegative-flonum]
                     [xml:attr-value+>fixnum xml:attr-value->nonnegative-fixnum]))

(require digimon/string)
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
            [XML-Element-Attribute-Value (U String Bytes Regexp Byte-Regexp (-> String Boolean)) -> (Option String)])
  (case-lambda
    [(v)
     (cond [(string? v) v]
           [(symbol? v) (xml:attr-symbol->string v)]
           [(list? v) (xml:attr-list->string v)]
           [else (xml:attr-value->string (unbox v))])]
    [(v pattern) (xml:attr-string-filter (xml:attr-value->string v) pattern)]))

(define xml:attr-value->token
  : (case-> [XML-Element-Attribute-Value -> String]
            [XML-Element-Attribute-Value (U String Bytes Regexp Byte-Regexp (-> String Boolean)) -> (Option String)])
  (case-lambda
    [(v)
     (cond [(string? v) (xml:attr-string->token v)]
           [(symbol? v) (xml:attr-symbol->token v)]
           [(list? v) (xml:attr-list->token v)]
           [else (xml:attr-value->token (unbox v))])]
    [(v pattern) (xml:attr-string-filter (xml:attr-value->token v) pattern)]))

(define xml:attr-value->string-list : (->* (XML-Element-Attribute-Value) ((U String Regexp (-> String (Listof String)))) (Listof String))
  (lambda [v [sep #px"\\s+"]]
    (cond [(string? v) ((inst xml:attr-string->list String) v values sep)]
          [(list? v) ((inst xml:attr-list->list String) v values)]
          [(symbol? v) ((inst xml:attr-symbol->list String) v values)]
          [else (xml:attr-value->string-list (unbox v))])))

(define xml:attr-value->boolean : (-> XML-Element-Attribute-Value (Option XML-Boolean))
  (lambda [v]
    (cond [(string? v) (xml:attr-string->boolean v)]
          [(symbol? v) (xml:attr-symbol->boolean v)]
          [else #false])))

(define xml:attr-value->integer : (case-> [XML-Element-Attribute-Value -> (Option Integer)]
                                          [XML-Element-Attribute-Value (Option Integer) -> (Option Integer)]
                                          [XML-Element-Attribute-Value (Option Integer) (Option Integer) -> (Option Integer)])
  (case-lambda
    [(v)
     (cond [(string? v) (xml:attr-string->real v string->integer)]
           [(symbol? v) (xml:attr-symbol->real v string->integer)]
           [else #false])]
    [(v min) (xml:attr-real-filter (xml:attr-value->integer v) min)]
    [(v min max) (xml:attr-real-filter (xml:attr-value->integer v) min max)]))

(define xml:attr-value->fixnum : (case-> [XML-Element-Attribute-Value -> (Option Fixnum)]
                                         [XML-Element-Attribute-Value (Option Fixnum) -> (Option Fixnum)]
                                         [XML-Element-Attribute-Value (Option Fixnum) (Option Fixnum) -> (Option Fixnum)])
  (case-lambda
    [(v)
     (cond [(string? v) (xml:attr-string->real v string->fixnum)]
           [(symbol? v) (xml:attr-symbol->real v string->fixnum)]
           [else #false])]
    [(v min) (xml:attr-real-filter (xml:attr-value->fixnum v) min)]
    [(v min max) (xml:attr-real-filter (xml:attr-value->fixnum v) min max)]))

(define xml:attr-value+>fixnum : (case-> [XML-Element-Attribute-Value -> (Option Nonnegative-Fixnum)]
                                         [XML-Element-Attribute-Value (Option Nonnegative-Fixnum) -> (Option Nonnegative-Fixnum)]
                                         [XML-Element-Attribute-Value (Option Nonnegative-Fixnum) (Option Nonnegative-Fixnum) -> (Option Nonnegative-Fixnum)])
  (case-lambda
    [(v)
     (cond [(string? v) (xml:attr-string->real v string+>fixnum)]
           [(symbol? v) (xml:attr-symbol->real v string+>fixnum)]
           [else #false])]
    [(v min) (xml:attr-real-filter (xml:attr-value+>fixnum v) min)]
    [(v min max) (xml:attr-real-filter (xml:attr-value+>fixnum v) min max)]))

(define xml:attr-value->byte : (case-> [XML-Element-Attribute-Value -> (Option Byte)]
                                       [XML-Element-Attribute-Value (Option Byte) -> (Option Byte)]
                                       [XML-Element-Attribute-Value (Option Byte) (Option Byte) -> (Option Byte)])
  (case-lambda
    [(v)
     (cond [(string? v) (xml:attr-string->real v string->byte)]
           [(symbol? v) (xml:attr-symbol->real v string->byte)]
           [else #false])]
    [(v min) (xml:attr-real-filter (xml:attr-value->byte v) min)]
    [(v min max) (xml:attr-real-filter (xml:attr-value->byte v) min max)]))

(define xml:attr-value->index : (case-> [XML-Element-Attribute-Value -> (Option Index)]
                                        [XML-Element-Attribute-Value (Option Index) -> (Option Index)]
                                        [XML-Element-Attribute-Value (Option Index) (Option Index) -> (Option Index)])
  (case-lambda
    [(v)
     (cond [(string? v) (xml:attr-string->real v string->index)]
           [(symbol? v) (xml:attr-symbol->real v string->index)]
           [else #false])]
    [(v min) (xml:attr-real-filter (xml:attr-value->index v) min)]
    [(v min max) (xml:attr-real-filter (xml:attr-value->index v) min max)]))

(define xml:attr-value->natural : (case-> [XML-Element-Attribute-Value -> (Option Natural)]
                                          [XML-Element-Attribute-Value (Option Natural) -> (Option Natural)]
                                          [XML-Element-Attribute-Value (Option Natural) (Option Natural) -> (Option Natural)])
  (case-lambda
    [(v)
     (cond [(string? v) (xml:attr-string->real v string->natural)]
           [(symbol? v) (xml:attr-symbol->real v string->natural)]
           [else #false])]
    [(v min) (xml:attr-real-filter (xml:attr-value->natural v) min)]
    [(v min max) (xml:attr-real-filter (xml:attr-value->natural v) min max)]))

(define xml:attr-value->hexadecimal : (case-> [XML-Element-Attribute-Value -> (Option Index)]
                                              [XML-Element-Attribute-Value Positive-Byte -> (Option Index)])
  (case-lambda
    [(v)
     (cond [(string? v) (xml:attr-string->real v string->index 16)]
           [(symbol? v) (xml:attr-symbol->real v string->index 16)]
           [else #false])]
    [(v len) (xml:attr-hexadecimal-filter (xml:attr-value->string v) len)]))

(define xml:attr-value->flonum : (case-> [XML-Element-Attribute-Value -> (Option Flonum)]
                                         [XML-Element-Attribute-Value (Option Flonum) -> (Option Flonum)]
                                         [XML-Element-Attribute-Value (Option Flonum) (Option Flonum) -> (Option Flonum)])
  (case-lambda
    [(v)
     (cond [(string? v) (xml:attr-string->real v string->flonum)]
           [(symbol? v) (xml:attr-symbol->real v string->flonum)]
           [else #false])]
    [(v min) (xml:attr-real-filter (xml:attr-value->flonum v) min)]
    [(v min max) (xml:attr-real-filter (xml:attr-value->flonum v) min max)]))

(define xml:attr-value+>flonum : (case-> [XML-Element-Attribute-Value -> (Option Nonnegative-Flonum)]
                                         [XML-Element-Attribute-Value (Option Nonnegative-Flonum) -> (Option Nonnegative-Flonum)]
                                         [XML-Element-Attribute-Value (Option Nonnegative-Flonum) (Option Nonnegative-Flonum) -> (Option Nonnegative-Flonum)])
  (case-lambda
    [(v)
     (cond [(string? v) (xml:attr-string->real v string+>flonum)]
           [(symbol? v) (xml:attr-symbol->real v string+>flonum)]
           [else #false])]
    [(v min) (xml:attr-real-filter (xml:attr-value+>flonum v) min)]
    [(v min max) (xml:attr-real-filter (xml:attr-value+>flonum v) min max)]))

(define #:forall (N) xml:attr-value->number
  : (case-> [XML-Element-Attribute-Value -> (Option Real)]
            [XML-Element-Attribute-Value (-> Any Boolean : (∩ Real N)) -> (Option N)]
            [XML-Element-Attribute-Value (-> Any Boolean : (∩ Real N)) (Option (∩ Real N)) (Option (∩ Real N)) -> (Option N)])
  (case-lambda
    [(v)
     (cond [(string? v) (xml:attr-string->real v string->real)]
           [(symbol? v) (xml:attr-symbol->real v string->real)]
           [else #false])]
    [(v number?) (let ([r (xml:attr-value->number v)]) (and r (number? r) r))]
    [(v number? min max) ((inst xml:attr-real-filter N) (xml:attr-value->number v) number? min max)]))

(define #:forall (N) xml:attr-value+>number
  : (case-> [XML-Element-Attribute-Value -> (Option Nonnegative-Real)]
            [XML-Element-Attribute-Value (-> Any Boolean : (∩ Real N)) -> (Option N)]
            [XML-Element-Attribute-Value (-> Any Boolean : (∩ Real N)) (Option (∩ Real N)) (Option (∩ Real N)) -> (Option N)])
  (case-lambda
    [(v)
     (cond [(string? v) (xml:attr-string->real v string+>real)]
           [(symbol? v) (xml:attr-symbol->real v string+>real)]
           [else #false])]
    [(v number?) (let ([r (xml:attr-value+>number v)]) (and r (number? r) r))]
    [(v number? min max) ((inst xml:attr-real-filter N) (xml:attr-value+>number v) number? min max)]))

(define xml:attr-value->symbol : (-> XML-Element-Attribute-Value Symbol)
  (lambda [v]
    (cond [(string? v) (xml:attr-string->symbol v string->symbol)]
          [(symbol? v) (xml:attr-symbol->symbol v)]
          [(list? v) (xml:attr-list->symbol v string->symbol)]
          [else (xml:attr-value->symbol (unbox v))])))

(define xml:attr-value->unreadable-symbol : (-> XML-Element-Attribute-Value Symbol)
  (lambda [v]
    (cond [(string? v) (xml:attr-string->symbol v string->unreadable-symbol)]
          [(symbol? v) (xml:attr-symbol->unreadable-symbol v)]
          [(list? v) (xml:attr-list->symbol v string->unreadable-symbol)]
          [else (xml:attr-value->unreadable-symbol (unbox v))])))

(define xml:attr-value->symbol-list : (->* (XML-Element-Attribute-Value) ((U String Regexp (-> String (Listof String)))) (Listof Symbol))
  (lambda [v [sep #px"\\s+"]]
    (cond [(string? v) (xml:attr-string->list v string->symbol sep)]
          [(list? v) (xml:attr-list->list v string->symbol)]
          [(symbol? v) (xml:attr-symbol->list v string->symbol)]
          [else (xml:attr-value->symbol-list (unbox v) sep)])))

(define xml:attr-value->unreadable-symbol-list : (->* (XML-Element-Attribute-Value) ((U String Regexp (-> String (Listof String)))) (Listof Symbol))
  (lambda [v [sep #px"\\s+"]]
    (cond [(string? v) (xml:attr-string->list v string->unreadable-symbol sep)]
          [(list? v) (xml:attr-list->list v string->unreadable-symbol)]
          [(symbol? v) (xml:attr-symbol->list v string->unreadable-symbol)]
          [else (xml:attr-value->unreadable-symbol-list (unbox v) sep)])))

(define xml:attr-value->keyword : (-> XML-Element-Attribute-Value Keyword)
  (lambda [v]
    (cond [(string? v) (xml:attr-string->symbol v string->keyword)]
          [(symbol? v) (xml:attr-symbol->keyword v)]
          [(list? v) (xml:attr-list->symbol v string->keyword)]
          [else (xml:attr-value->keyword (unbox v))])))

(define xml:attr-value->dimension : (->* (XML-Element-Attribute-Value) (Symbol) (Option XML-Dimension))
  (lambda [v [canonical-unit '||]]
    (cond [(string? v) (xml:attr-string->dimension v canonical-unit #false #false)]
          [(symbol? v) (xml:attr-symbol->dimension v canonical-unit #false #false)]
          [else #false])))

(define xml:attr-value+>dimension : (->* (XML-Element-Attribute-Value) (Symbol) (Option XML-Nonnegative-Dimension))
  (lambda [v [canonical-unit '||]]
    (cond [(string? v) (xml:attr-string->dimension v canonical-unit #false nonnegative-flonum?)]
          [(symbol? v) (xml:attr-symbol->dimension v canonical-unit #false nonnegative-flonum?)]
          [else #false])))

(define xml:attr-value->dimension/ci : (->* (XML-Element-Attribute-Value) (Symbol) (Option XML-Dimension))
  (lambda [v [canonical-unit '||]]
    (cond [(string? v) (xml:attr-string->dimension v canonical-unit #true #false)]
          [(symbol? v) (xml:attr-symbol->dimension v canonical-unit #true #false)]
          [else #false])))

(define xml:attr-value+>dimension/ci : (->* (XML-Element-Attribute-Value) (Symbol) (Option XML-Nonnegative-Dimension))
  (lambda [v [canonical-unit '||]]
    (cond [(string? v) (xml:attr-string->dimension v canonical-unit #true nonnegative-flonum?)]
          [(symbol? v) (xml:attr-symbol->dimension v canonical-unit #true nonnegative-flonum?)]
          [else #false])))

(define xml:attr-value->percentage : (->* (XML-Element-Attribute-Value) (Symbol) (Option XML-Percentage))
  (lambda [v [canonical-unit '||]]
    (cond [(string? v) (xml:attr-string->percentage v canonical-unit #false)]
          [(symbol? v) (xml:attr-symbol->percentage v canonical-unit #false)]
          [else #false])))

(define xml:attr-value+>percentage : (->* (XML-Element-Attribute-Value) (Symbol) (Option XML-Nonnegative-Percentage))
  (lambda [v [canonical-unit '||]]
    (cond [(string? v) (xml:attr-string->percentage v canonical-unit nonnegative-flonum?)]
          [(symbol? v) (xml:attr-symbol->percentage v canonical-unit nonnegative-flonum?)]
          [else #false])))

(define xml:attr-value->fixed-percentage : (->* (XML-Element-Attribute-Value) (Symbol) (Option XML-Percentage))
  (lambda [v [canonical-unit '||]]
    (cond [(string? v) (xml:attr-string->percentage v canonical-unit fixed-percentage?)]
          [(symbol? v) (xml:attr-symbol->percentage v canonical-unit fixed-percentage?)]
          [else #false])))

(define xml:attr-value+>fixed-percentage : (->* (XML-Element-Attribute-Value) (Symbol) (Option XML-Nonnegative-Percentage))
  (lambda [v [canonical-unit '||]]
    (cond [(string? v) (xml:attr-string->percentage v canonical-unit nonnegative-fixed-percentage?)]
          [(symbol? v) (xml:attr-symbol->percentage v canonical-unit nonnegative-fixed-percentage?)]
          [else #false])))

(define xml:attr-value->listof-type : (All (T) (->* (XML-Element-Attribute-Value (-> XML-Element-Attribute-Value (Option T)))
                                                    ((U String Regexp (-> String (Listof String))))
                                                    (Listof T)))
  (lambda [v string->datum [sep #px"\\s+"]]
    (cond [(string? v) (xml:attr-string->filtered-list v string->datum sep)]
          [(list? v) (xml:attr-list->filtered-list v string->datum)]
          [(symbol? v) (xml:attr-symbol->filtered-list v string->datum)]
          [else null])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml:attr-value->uri-string : (-> XML-Element-Attribute-Value (Option String))
  (lambda [v]
    (xml:attr-value->token v string-uri?)))

(define xml:attr-value->guid-string : (-> XML-Element-Attribute-Value (Option String))
  (lambda [v]
    (xml:attr-value->token v string-guid?)))

(define xml:attr-value->panose-string : (-> XML-Element-Attribute-Value (Option String))
  (lambda [v]
    (xml:attr-value->token v string-panose?)))

(define xml:attr-value->panose : (-> XML-Element-Attribute-Value (Option Index))
  (lambda [v]
    (define s (xml:attr-value->panose-string v))
    (and s (string->index s 16))))

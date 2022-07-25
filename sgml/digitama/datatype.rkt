#lang typed/racket/base

(provide (all-defined-out))

(require racket/string)

(require digimon/symbol)

(require "grammar.rkt")
(require "digicore.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; NOTE
; Normalizing the attributes' values require DTD or other schema,
; These APIs are designed to manually normalizing,
; Thus, the input tokens are usually XML:String instances.

(define xml-attribute-value->string : (-> XML-Element-Attribute-Value* String)
  (lambda [v]
    (cond [(xml:string? v) (xml:string-datum v)]
          [(xml:name? v) (symbol->immutable-string (xml:name-datum v))]
          [else (symbol-join (map xml:name-datum v))])))

(define xml-attribute-value->symbol : (-> XML-Element-Attribute-Value* Symbol)
  (lambda [v]
    (cond [(xml:string? v) (string->unreadable-symbol (xml:string-datum v))]
          [(xml:name? v) (xml:name-datum v)]
          [else (string->unreadable-symbol (symbol-join (map xml:name-datum v)))])))

(define xml-attribute-value->symbols : (-> XML-Element-Attribute-Value* (Listof Symbol))
  (lambda [v]
    (cond [(xml:string? v) (map string->unreadable-symbol (string-split (xml:string-datum v)))]
          [(list? v) (map xml:name-datum v)]
          [(xml:name? v) (list (xml:name-datum v))]
          [else null])))

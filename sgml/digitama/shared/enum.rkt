#lang typed/racket/base

(provide (all-defined-out))

(require "../xexpr/grammar.rkt")

(require (for-syntax racket/base))
(require (for-syntax racket/list))
(require (for-syntax racket/symbol))
(require (for-syntax racket/syntax))
(require (for-syntax syntax/parse))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax (define-xml-enumeration stx)
  (syntax-parse stx #:literals [:]
    [(_ id : TypeU
        (~optional (~seq #:for prefix) #:defaults ([prefix #'xml]))
        [enum ...])
     (with-syntax ([(senum ...) (let* ([enums (syntax->list #'[enum ...])]
                                       [<enum> (check-duplicates enums eq? #:key syntax-e)])
                                  (when (syntax? <enum>)
                                    (raise-syntax-error 'define-xml-enumeration "duplicate name" <enum> #false
                                                        (filter (λ [<e>] (and (eq? (syntax-e <e>) (syntax-e <enum>))
                                                                              (not (eq? <e> <enum>))))
                                                                enums)))
                                  (for/list ([<e> (in-list enums)])
                                    (datum->syntax <e> (symbol->immutable-string (syntax-e <e>)))))]
                   [id? (format-id #'id "~a?" #'id)]
                   [xml:attr->id (format-id #'id "~a:attr-value->~a" #'prefix #'id)])
       (syntax/loc stx
         (begin (define-type TypeU (U 'enum ...))
                (define id? : (-> Any Boolean : TypeU) (λ [v] (or (eq? v 'enum) ...)))

                (define xml:attr->id : (-> XML-Element-Attribute-Value (Option TypeU))
                  (lambda [v]
                    (cond [(string? v) (cond [(string=? v senum) 'enum] ... [else #false])]
                          [(symbol? v) (and (id? v) v)]
                          [(box? v) (xml:attr->id (unbox v))]
                          [else #false]))))))]))

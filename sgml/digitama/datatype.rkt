#lang typed/racket/base

(provide (all-defined-out))
(provide (rename-out [xml:attr-value*+>integer xml:attr-value*->natural]))
(provide (all-from-out "shared/datatype.rkt"))

(require racket/string)

(require digimon/symbol)
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
(define xml:attr-value*->string : (-> XML-Element-Attribute-Value* String)
  (lambda [v]
    (cond [(xml:string? v) (xml:string-datum v)]
          [(xml:name? v) (symbol->immutable-string (xml:name-datum v))]
          [else (symbol-join (map xml:name-datum v))])))

(define xml:attr-value*->string/trim : (-> XML-Element-Attribute-Value* String)
  (lambda [v]
    (cond [(xml:string? v) (string-trim (xml:string-datum v))]
          [(xml:name? v) (symbol->immutable-string (xml:name-datum v))]
          [else (symbol-join (map xml:name-datum v))])))

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

(define xml:attr-value*->integer : (-> XML-Element-Attribute-Value* (Option Integer))
  (lambda [v]
    (cond [(xml:string? v) (string->integer (string-trim (xml:string-datum v)))]
          [(xml:name? v) (string->integer (symbol->immutable-string (xml:name-datum v)))]
          [else #false])))

(define xml:attr-value*->index : (-> XML-Element-Attribute-Value* (Option Index))
  (lambda [v]
    (cond [(xml:string? v) (string->index (string-trim (xml:string-datum v)))]
          [(xml:name? v) (string->index (symbol->immutable-string (xml:name-datum v)))]
          [else #false])))

(define xml:attr-value*+>integer : (-> XML-Element-Attribute-Value* (Option Natural))
  (lambda [v]
    (cond [(xml:string? v) (string->natural (string-trim (xml:string-datum v)))]
          [(xml:name? v) (string->natural (symbol->immutable-string (xml:name-datum v)))]
          [else #false])))

(define xml:attr-value*->flonum : (-> XML-Element-Attribute-Value* (Option Flonum))
  (lambda [v]
    (cond [(xml:string? v) (string->flonum (string-trim (xml:string-datum v)))]
          [(xml:name? v) (string->flonum (symbol->immutable-string (xml:name-datum v)))]
          [else #false])))

(define xml:attr-value*+>flonum : (-> XML-Element-Attribute-Value* (Option Nonnegative-Flonum))
  (lambda [v]
    (cond [(xml:string? v) (string+>flonum (string-trim (xml:string-datum v)))]
          [(xml:name? v) (string+>flonum (symbol->immutable-string (xml:name-datum v)))]
          [else #false])))

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

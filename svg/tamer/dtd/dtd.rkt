#lang typed/racket/base

(provide (all-defined-out) svg11*.dtd)

(require sgml/digitama/schema)
(require sgml/digitama/digicore)

(require "../../village/svglang/svg11.tdtd")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define attrlists : (HashTable Symbol Keyword) (make-hasheq))
(define attribs : (HashTable Keyword (Listof Symbol)) (make-hasheq))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (dtd-setup!) : Void
  (for ([(e-name entity) (in-hash (xml-schema-entities svg11*.dtd))])
    (when (and (keyword? e-name) (xsch-token-entity? entity))
      (let ([body (xsch-token-entity-body entity)])
        (when (and (pair? body) (xml:name? (car body)))
          (for ([token (in-list (or (xsch-token-entity-body entity) null))])
            (when (xml:name? token)
              (define a-name (xml:name-datum token))
              (when (pair? (dtd-element-list a-name))
                (hash-set! attrlists a-name e-name)
                (hash-set! attribs e-name (cons a-name (hash-ref attribs e-name (inst list Symbol))))))))))))

(define (dtd-attribute-list [tag-name : Symbol]) : (Listof Symbol)
  (hash-keys
   (hash-ref (xml-schema-attributes svg11*.dtd) tag-name
             (inst make-hasheq Symbol XSch-Attribute))))

(define (dtd-element-list [attr-name : Symbol]) : (Listof Symbol)
  (for/list ([(e as) (xml-schema-attributes svg11*.dtd)]
             #:when (hash-has-key? as attr-name))
    e))

(define (dtd-children [tag-name : Symbol]) : (U False Keyword (Listof Symbol) (Pairof Schema-Element-Children Char))
  (define e (hash-ref (xml-schema-elements svg11*.dtd) tag-name (λ [] #false)))

  (cond [(xsch-mixed-element? e) (xsch-mixed-element-children e)]
        [(xsch-element+children? e) (xsch-element+children-content e)]
        [(xsch-element? e) '#:empty]
        [else #false]))

(define (dtd-attr-group-displayln [attrs : (Listof Symbol)] [indent : String ""]) : Void
  (define-values (groups extra)
    (for/fold ([groups : (Listof Keyword) null] [extra : (Listof Symbol) null])
              ([attr (in-list attrs)])
      (define group (hash-ref attrlists attr (λ [] #false)))
      (cond [(not group) (values groups (cons attr extra))]
            [(memq group groups) (values groups extra)]
            [else (values (cons group groups) extra)])))
  
    (for ([group (in-list groups)])
      (printf "~a~a: ~a~n" indent group (reverse (hash-ref attribs group list))))
    (printf "~aREST: ~a~n" indent extra))

(define (dtd-info-displayln [argv : (Listof Symbol)] [type : Symbol] [svg-list : (-> Symbol (Listof Symbol))]) : Void
    (define lists : (Listof (Pairof (Listof Symbol) Symbol))
      (for/list ([tag (in-list argv)])
        (cons (svg-list tag) tag)))

    (printf "=================== ~a ===================" type)
    
    (for ([ls (in-list lists)])
      (when (pair? (car ls))
        (printf "~n~a[~a, ~a]: ~a~n"
                (cdr ls) type (length (car ls)) (car ls))))

    (let ([dict : (HashTable Symbol Natural) (make-hasheq)]
          [n (length argv)])
      (for ([ls (in-list lists)])
        (for ([t (in-list (car ls))])
          (hash-set! dict t (+ (hash-ref dict t (λ [] 0)) 1))))

      (when (> n 1)
        (printf "~ncommon:")
        (for ([(t c) (in-hash dict)])
          (when (= c n)
            (printf " ~a" t)))
        (newline))))

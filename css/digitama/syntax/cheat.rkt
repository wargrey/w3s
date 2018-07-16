#lang typed/racket/base

(provide (all-defined-out))

(require typed/racket/class)
(require typed/racket/unsafe)

(require (for-syntax racket/base))
(require (for-syntax racket/syntax))

(module cheat racket/base
  (provide (all-defined-out))

  (require racket/class)

  (define (make-cheat-opaque? ? fname)
    (procedure-rename (λ [v] (and (? v) #true)) fname))

  (define (make-cheat-is-a? % fname)
    (procedure-rename (λ [v] (is-a? v %)) fname))

  (define (make-cheat-procedure? arity fname kw?)
    (procedure-rename (λ [v] (and (procedure? v)
                                  (procedure-arity-includes? v arity kw?)))
                      fname)))

(unsafe-require/typed/provide
 (submod "." cheat)
 [make-cheat-opaque? (All (OT) (-> (-> Any Boolean) Symbol (-> Any Boolean : #:+ OT)))]
 [make-cheat-is-a? (All (%) (-> Any Symbol (-> Any Boolean : #:+ (Instance %))))]
 [make-cheat-procedure? (All (PT) (-> Byte Symbol Boolean (-> Any Boolean : #:+ PT)))])

(define-syntax (define-cheat-opaque stx)
  (syntax-case stx []
    [(_ id #:=> PT arg kw?)
     #'(define id : (-> Any Boolean : #:+ PT)
         ((inst make-cheat-procedure? PT) arg 'id kw?))]
    [(_ id #:is-a? % arg%)
     #'(define id : (-> Any Boolean : #:+ (Instance %))
         ((inst make-cheat-is-a? %) arg% 'id))]))

(define-syntax (define/make-is-a? stx)
  (syntax-case stx [:]
    [(_ % : Type% class-definition)
     (with-syntax ([%? (format-id #'% "~a?" (syntax-e #'%))])
       #'(begin (define % : Type% class-definition)
                (define-cheat-opaque %? #:is-a? Type% %)))]))

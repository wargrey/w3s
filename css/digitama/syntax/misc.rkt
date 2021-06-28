#lang typed/racket/base

(provide (all-defined-out))

(require racket/promise)

(require "w3s.rkt")

(require (for-syntax racket/base))
(require (for-syntax racket/syntax))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; For performance, mutating racket parameters costs too much ...
(define-syntax (define-css-parameter stx)
  (syntax-case stx [:]
    [(_ id : Type #:= defval)
     (syntax/loc stx
       (define id : (case-> [(U Type (Promise (-> Type))) -> Void] [-> Type])
         (let ([&storage : (Boxof (-> Type)) (box (λ [] defval))])
           (case-lambda
             [(v) (set-box! &storage (if (promise? v) (force v) (λ [] v)))]
             [() ((unbox &storage))]))))]
    [(_ id : Type #:guard [guard : TypeIn] #:= defval)
     (syntax/loc stx
       (define id : (case-> [TypeIn -> Void] [-> Type])
         (let ([&storage : (Boxof Type) (box (guard defval))])
           (case-lambda
             [(v) (set-box! &storage (guard v))]
             [() (unbox &storage)]))))]))

(define-syntax (define-css-parameters stx)
  (syntax-case stx [:]
    [(_ parameters [name ...] : Type #:= defval)
     (with-syntax ([(p-name ...) (for/list ([<u> (in-list (syntax->list #'(name ...)))]) (format-id <u> "css-~a" (syntax-e <u>)))])
       (syntax/loc stx
         (begin (define-css-parameter p-name : Type #:= defval) ... 
                (define parameters : (case-> [-> (Listof (Pairof Symbol (U Type (Promise (-> Type)))))]
                                             [(Listof (Pairof Symbol Type)) -> Void])
                  (case-lambda
                    [(db) (let ([pv (assq 'name db)]) (when pv (p-name (cdr pv)))) ...]
                    [() (list (cons 'name (p-name)) ...)])))))]))

(define-type (Listof+ css) (Pairof css (Listof css)))
(define-type (ListofU+ css) (U css (Listof+ css)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
(define css-log-read-error : (->* ((U exn String)) (Any Log-Level) Void)
  (lambda [errobj [src #false] [level 'debug]]
    (w3s-log-exn errobj 'exn:css:read src level)))

(define css-log-eval-error : (->* ((U exn String)) (Any Log-Level) Void)
  (lambda [errobj [src #false] [level 'debug]]
    (w3s-log-exn errobj 'exn:css:eval src level)))

(define hash-set++ : (-> (HashTable Symbol Any) (Listof+ Symbol) Any (HashTable Symbol Any))
  (lambda [data++ tags v]
    (define rest : (Listof Symbol) (cdr tags))
    (cond [(null? rest) (hash-set data++ (car tags) v)]
          [else (hash-set++ (hash-set data++ (car tags) v) rest v)])))

(define hash-update++ : (-> (HashTable Symbol Any) (Listof+ Symbol) Any (-> Symbol Any Any Any) (HashTable Symbol Any))
  (lambda [data++ tags datum updater]
    (define rest : (Listof Symbol) (cdr tags))
    (cond [(null? rest) (hash-update data++ (car tags) (λ [[v : Any]] (updater (car tags) v datum)) (λ [] #false))]
          [else (hash-update++ (hash-update data++ (car tags) (λ [[v : Any]] (updater (car tags) v datum)) (λ [] #false))
                               rest datum updater)])))

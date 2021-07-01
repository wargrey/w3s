#lang typed/racket/base

(provide (all-defined-out))

(require racket/promise)

(require "w3s.rkt")

(require (for-syntax racket/base))
(require (for-syntax racket/syntax))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type (Listof+ css) (Pairof css (Listof css)))
(define-type (ListofU+ css) (U css (Listof+ css)))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
(define css-log-read-error : (->* ((U exn String)) (Any Log-Level) Void)
  (lambda [errobj [src #false] [level 'debug]]
    (w3s-log-exn errobj 'exn:css:read src level)))

(define css-log-eval-error : (->* ((U exn String)) (Any Log-Level) Void)
  (lambda [errobj [src #false] [level 'debug]]
    (w3s-log-exn errobj 'exn:css:eval src level)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type CSS-Shorthand-Datum (Listof (Pairof Symbol Any)))
(define-type CSS-Longhand-Update (-> Symbol (Option Any) Any Any))

(define css-shorthand-ref : (->* (CSS-Shorthand-Datum Symbol) ((Option (-> Any))) Any)
  (lambda [data tag [mkdefault #false]]
    (define maybe-datum (assq tag data))

    (cond [(pair? maybe-datum) (cdr maybe-datum)]
          [(not mkdefault) (raise-user-error 'css-shorthand-ref "no such property: ~a" tag)]
          [else (mkdefault)])))

(define css-shorthand-set : (case-> [CSS-Shorthand-Datum Symbol Any -> CSS-Shorthand-Datum]
                                    [CSS-Shorthand-Datum Symbol Any CSS-Longhand-Update -> CSS-Shorthand-Datum])
  (case-lambda
    [(base tag datum)
     (let try-set ([data : CSS-Shorthand-Datum base]
                   [atad : CSS-Shorthand-Datum null])
       (cond [(null? data) (cons (cons tag datum) atad)]
             [else (let-values ([(datum rest) (values (car data) (cdr data))])
                     (cond [(not (eq? (car datum) tag)) (try-set (cdr data) (cons datum atad))]
                           [(null? rest) (cons (cons tag datum) atad)]
                           [else (cons (cons tag datum) (append rest atad))]))]))]
    [(base tag datum updater)
     (let try-update ([data : CSS-Shorthand-Datum base]
                      [atad : CSS-Shorthand-Datum null])
       (cond [(null? data) (cons (cons tag (updater tag #false datum)) atad)]
             [else (let-values ([(self rest) (values (car data) (cdr data))])
                     (cond [(not (eq? (car self) tag)) (try-update (cdr data) (cons self atad))]
                           [(null? rest) (cons (cons tag (updater tag (cdr self) datum)) atad)]
                           [else (cons (cons tag (updater tag (cdr self) datum)) (append rest atad))]))]))]))

(define css-shorthand-set* : (case-> [CSS-Shorthand-Datum (Listof+ Symbol) Any -> CSS-Shorthand-Datum]
                                     [CSS-Shorthand-Datum (Listof+ Symbol) Any CSS-Longhand-Update -> CSS-Shorthand-Datum])
  (case-lambda
    [(data tags v)
     (define rest : (Listof Symbol) (cdr tags))
     (cond [(null? rest) (css-shorthand-set data (car tags) v)]
           [else (css-shorthand-set* (css-shorthand-set data (car tags) v) rest v)])]
    [(data tags v updater)
     (define rest : (Listof Symbol) (cdr tags))
     (cond [(null? rest) (css-shorthand-set data (car tags) v updater)]
           [else (css-shorthand-set* (css-shorthand-set data (car tags) v updater)
                                     rest v updater)])]))

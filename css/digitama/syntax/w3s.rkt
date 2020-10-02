#lang typed/racket/base

(provide (all-defined-out))

(require (for-syntax racket/base))
(require (for-syntax racket/syntax))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax (define-syntax-error stx)
  (syntax-case stx []
    [(_ exn:w3s #:as Syntax-Error #:for Token #:with [make-syntax-error log-syntax-error]
        [subexn #:-> parent] ...)
     (with-syntax ([([make-exn make+exn throw-exn] ...)
                    (for/list ([<exn> (in-list (syntax->list #'(subexn ...)))])
                      (list (format-id <exn> "make-~a" (syntax-e <exn>))
                            (format-id <exn> "make+~a" (syntax-e <exn>))
                            (format-id <exn> "throw-~a" (syntax-e <exn>))))])
       #'(begin (define-type Syntax-Error exn:w3s)
                (struct exn:w3s exn:fail:syntax ())
                (struct subexn parent ()) ...

                (define make-exn : (-> (U EOF Token (Listof Token)) subexn)
                  (lambda [v]
                    (make-syntax-error subexn v)))
                ...

                (define make+exn : (->* ((U EOF Token (Listof Token))) ((Option Token) (Option Log-Level)) subexn)
                  (lambda [v [property #false] [level #false]]
                    (define errobj : subexn (make-syntax-error subexn v))
                    (log-syntax-error errobj property level)
                    errobj))
                ...

                (define throw-exn : (->* ((U EOF Token (Listof Token))) ((Option Token) Log-Level) Nothing)
                  (lambda [v [property #false] [level 'warning]]
                    (raise (make+exn v property level))))
                ...))]))

(define-syntax (w3s-remake-token stx)
  (syntax-case stx []
    [(_ [start-token end-token] make-w3s:token datum extra ...)
     #'(make-w3s:token (w3s-token-source start-token) (w3s-token-line start-token)
                       (w3s-token-column start-token) (w3s-token-start start-token)
                       (w3s-token-end end-token) datum extra ...)]
    [(_ here-token make-w3s:token datum ...)
     #'(w3s-remake-token [here-token here-token] make-w3s:token datum ...)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type W3S-Token-Source (U String Symbol))
(define-type W3S-Datum (Rec datum (U String Symbol Keyword Integer Flonum Char Boolean (Pairof datum datum))))
(define-type W3S-Token-Constructor (All (T W3S) (-> W3S-Token-Source Positive-Integer Natural Positive-Integer Positive-Integer T W3S)))

(struct w3s-token
  ([source : W3S-Token-Source]
   [line : Positive-Integer]
   [column : Natural]
   [start : Positive-Integer] ; `start` and `end` (instead of `position` and `span`) are required by color lexer.
   [end : Positive-Integer])
  #:type-name W3S-Token
  #:transparent)

(struct (T) w3s-token-datum ; for #lang expander
  ([typeid : Symbol]
   [source : W3S-Token-Source]
   [line : Positive-Integer]
   [column : Natural]
   [start : Positive-Integer]
   [end : Positive-Integer]
   [payload : T])
  #:prefab
  #:type-name W3S-Token-Datumof)

(define w3s-token->syntax-location : (-> W3S-Token (Vector Any (Option Integer) (Option Integer) (Option Integer) (Option Integer)))
  (lambda [instance]
    (vector (w3s-token-source instance) (w3s-token-line instance) (w3s-token-column instance)
            (w3s-token-start instance) (- (w3s-token-end instance) (w3s-token-start instance)))))

(define w3s-token-location-string : (-> W3S-Token String)
  (lambda [instance]
    (format "~a:~a:~a" (w3s-token-source instance) (w3s-token-line instance) (add1 (w3s-token-column instance)))))

(define make-w3s-location+datum : (All (T) (-> T (W3S-Token-Datumof T)))
  (lambda [datum]
    (w3s-token-datum 'DEADCODE '/dev/ddcin
                      1 0 1 1
                      datum)))

(define w3s-token->location+datum : (All (W3S T) (case-> [(∩ W3S W3S-Token) (∩ T W3S-Datum) -> (W3S-Token-Datumof T)]
                                                         [(∩ W3S W3S-Token) (-> W3S (∩ T W3S-Datum)) -> (W3S-Token-Datumof T)]))
  (lambda [instance datum]
    (w3s-token-datum (assert (object-name instance) symbol?) (w3s-token-source instance)
                     (w3s-token-line instance) (w3s-token-column instance)
                     (w3s-token-start instance) (w3s-token-end instance)
                     (if (procedure? datum) (datum instance) datum))))

(define w3s-token->location+datum* : (All (W3S T) (-> (Option W3S) (-> W3S (∩ T W3S-Datum)) (Option (W3S-Token-Datumof T))))
  (lambda [instance datum]
    (and (w3s-token? instance)
         ((inst w3s-token->location+datum (∩ W3S W3S-Token) T)
          instance datum))))

(define w3s-location+datum->token : (All (T W3S) (case-> [(W3S-Token-Constructor T W3S) (W3S-Token-Datumof T) -> W3S]
                                                         [(W3S-Token-Constructor T W3S) (W3S-Token-Datumof Any) T -> W3S]))
  (case-lambda
    [(make-w3s:token datum)
     (make-w3s:token (w3s-token-datum-source datum)
                     (w3s-token-datum-line datum) (w3s-token-datum-column datum)
                     (w3s-token-datum-start datum) (w3s-token-datum-end datum)
                     (w3s-token-datum-payload datum))]
    [(make-w3s:token datum payload)
     (make-w3s:token (w3s-token-datum-source datum)
                     (w3s-token-datum-line datum) (w3s-token-datum-column datum)
                     (w3s-token-datum-start datum) (w3s-token-datum-end datum)
                     payload)]))

(define w3s-location+datum->token* : (All (T W3S) (case-> [(W3S-Token-Constructor T W3S) (Option (W3S-Token-Datumof T)) -> (Option W3S)]
                                                          [(W3S-Token-Constructor T W3S) (W3S-Token-Datumof Any) (-> Any Boolean : T) -> (Option W3S)]))
  (case-lambda
    [(make-w3s:token datum)
     (and datum
          (w3s-token-datum-typeof? datum make-w3s:token)
          (w3s-location+datum->token make-w3s:token datum))]
    [(make-w3s:token datum type?)
     (let ([payload (w3s-token-datum-payload datum)])
       (and (type? payload)
            (w3s-token-datum-typeof? datum make-w3s:token)
            (w3s-location+datum->token make-w3s:token datum payload)))]))

(define w3s-token-datum-typeof? : (All (T) (-> (W3S-Token-Datumof T) (U Struct-TypeTop Procedure) Boolean))
  (lambda [datum struct:type]
    (eq? (w3s-token-datum-typeid datum)
         (object-name struct:type))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define w3s-empty-stack : Continuation-Mark-Set (continuation-marks #false))

(define w3s-log-exn : (->* ((U exn String) Symbol) (Any Log-Level) Void)
  (lambda [errobj topic [src #false] [level 'debug]]
    (define message : String
      (cond [(string? errobj) errobj]
            [else (format "@~s: ~a: ~a" src
                    (object-name errobj) (exn-message errobj))]))
    
    (log-message (current-logger) level topic message errobj)))

(define #:forall (T Error) w3s-token->exn
  : (case-> [(-> String Continuation-Mark-Set (Listof Syntax) Error) (->* (T) ((Option Any) (Option Any)) String) (-> T Syntax) T -> Error]
            [(-> String Continuation-Mark-Set (Listof Syntax) Error) (->* (T) ((Option Any) (Option Any)) String) (-> T Syntax) (-> T String) T (Listof T) -> Error])
  (case-lambda
    [(exn:xml token->string token->syntax main)
     (exn:xml (token->string main exn:xml) w3s-empty-stack (list (token->syntax main)))]
    [(exn:xml token->string token->syntax token-datum->string head others)
     (exn:xml (format "~a ~a" (token->string head exn:xml) (map token-datum->string others))
              w3s-empty-stack (map token->syntax (cons head others)))]))

(define #:forall (T Error) w3s-log-syntax-error : (->* (Symbol (->* (T) ((Option Any) (Option Any)) String) (-> T Any) Error) ((Option T) Log-Level) Void)
  (lambda [topic token->string token->datum errobj [property #false] [level 'warning]]
    (define logger : Logger (current-logger))
    (define msg : String (exn-message (assert errobj exn?)))
    (define <eof>? : Boolean (regexp-match? #px"#<eof>" msg))
    (cond [(not property) (log-message logger level topic msg errobj)]
          [(w3s-token? property)
           (let ([property-msg (format "<~a:~a:~a>" (token->datum property) (w3s-token-line property) (add1 (w3s-token-column property)))])
             (cond [(not <eof>?) (log-message logger level topic (format "~a @~a" msg property-msg) errobj)]
                   [else (let ([eof-msg (token->string property errobj eof)])
                           (log-message logger level topic (format "~a @~a" eof-msg property-msg) errobj))]))])))

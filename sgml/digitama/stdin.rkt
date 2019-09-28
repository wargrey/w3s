#lang typed/racket/base

(provide (all-defined-out))
(provide xml-read-syntax xml-peek-syntax)

(require racket/path)
(require racket/port)
(require racket/unsafe/ops)

(require "digicore.rkt")
(require "tokenizer.rkt")

(require typed/racket/unsafe)

(unsafe-require/typed
 racket/base ; the output is gauranteed by the caller, hence the explicitly requiring.
 [[read-byte-or-special xml-read-syntax] (->* (Input-Port) (U XML-Token EOF))]
 [[peek-byte-or-special xml-peek-syntax] (->* (Input-Port Natural) (U XML-Token EOF))])

(require (for-syntax racket/base))

(define-type XML-StdIn (U Input-Port Path-String Bytes))

(define-syntax (define-xml-parser-entry stx)
  (syntax-case stx [: lambda]
    [(_ id #:-> ->T (lambda [/dev/xmlin [args : T defval ...] ...] body ...))
     #'(define (id [/dev/stdin : XML-StdIn (current-input-port)] [args : T defval ...] ...) : ->T
         (define /dev/xmlin : Input-Port (xml-open-input-port /dev/stdin))
         (dynamic-wind (λ [] '(css-open-input-port has already enabled line counting))
                       (λ [] ((λ [[/dev/xmlin : Input-Port] [args : T defval ...] ...] : ->T body ...) /dev/xmlin args ...))
                       (λ [] (close-input-port /dev/xmlin))))]))

(define xml-open-input-port : (-> XML-StdIn Input-Port)
  (lambda [/dev/stdin]
    (define /dev/rawin : Input-Port
      (cond [(port? /dev/stdin) /dev/stdin]
            [(path? /dev/stdin) (open-input-file /dev/stdin)]
            [(regexp-match? #px"\\.xml$" /dev/stdin) (open-input-file (~a /dev/stdin))]
            [(string? /dev/stdin) (open-input-string /dev/stdin '/dev/xmlin/string)]
            [(bytes? /dev/stdin) (open-input-bytes /dev/stdin '/dev/xmlin/bytes)]
            [else (open-input-string (~s /dev/stdin) '/dev/xmlin/error)]))
    (define /dev/xmlin : Input-Port (xml-fallback-encode-input-port /dev/rawin))
    (define peek-pool : (Listof XML-Syntax-Any) null)
    (define portname : (U String Symbol)
      (let ([src (object-name /dev/xmlin)])
        (cond [(path? src) (path->string (simple-form-path src))]
              [else (string->symbol (~a src))])))
    (make-input-port portname
                     (λ [[buf : Bytes]]
                       (λ _ (cond [(null? peek-pool) (xml-consume-token /dev/xmlin portname)]
                                  [else (let-values ([(rest peeked) (split-at-right peek-pool 1)])
                                          (set! peek-pool rest)
                                          (car peeked))])))
                     (λ [[buf : Bytes] [skip : Nonnegative-Integer] [evt : Any]]
                       ; NOTE: It seems that optimize this code to always throw the last peeked token
                       ;        does not improve the performance.
                       (λ _ (and (for ([idx (in-range (length peek-pool) (add1 skip))])
                                   (set! peek-pool (cons (xml-consume-token /dev/xmlin portname) peek-pool)))
                                 (list-ref peek-pool (- (length peek-pool) skip 1)))))
                     (λ [] (unless (eq? /dev/rawin /dev/xmlin) (close-input-port /dev/xmlin))
                       (unless (eq? /dev/rawin /dev/stdin) (close-input-port /dev/rawin)))
                     #false #false
                     (λ [] (port-next-location /dev/xmlin))
                     (λ [] (void (list xml-fallback-encode-input-port '|has already set it|))))))

(define xml-read-syntax/skip-whitespace : (-> Input-Port XML-Syntax-Any)
  (lambda [xml]
    (define token (xml-read-syntax xml))
    (cond [(not (xml:whitespace? token)) token]
          [else (xml-read-syntax/skip-whitespace xml)])))

(define xml-peek-syntax/skip-whitespace : (-> Input-Port XML-Syntax-Any)
  (lambda [xml]
    (let peek/skip-whitespace : XML-Syntax-Any ([skip : Nonnegative-Fixnum 0])
      (define token (xml-peek-syntax xml skip))
      (cond [(not (xml:whitespace? token)) token]
            [else (peek/skip-whitespace (unsafe-fx+ skip 1))]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define px:xml-declaration #px"^<[?][Xx][Mm][Ll][^?]+encoding\\s*=\\s*\"(.*?)\"[^?]*[?]>")

(define xml-fallback-charset : (-> Bytes String)
  (lambda [from]
    (define CHARSET : String (string-upcase (bytes->string/utf-8 from)))
    (cond [(member CHARSET '("UTF-16BE" "UTF-16LE")) "UTF-8"]
          [else CHARSET])))
  
(define xml-fallback-encode-input-port : (-> Input-Port Input-Port)
  ;;; https://www.w3.org/TR/xml11/#charencoding
  ;;; https://www.w3.org/TR/xml11/#sec-TextDecl
  (lambda [/dev/rawin]
    (unless (port-counts-lines? /dev/rawin) (port-count-lines! /dev/rawin))
    (when (regexp-match-peek #px"^#(lang|!)" /dev/rawin)
      ;; skip racket `#lang` line and blanks.
      (read-line /dev/rawin)
      (regexp-match #px"^\\s*" /dev/rawin))
    (define magic : (Option (Pairof Bytes (Listof (Option Bytes)))) (regexp-match-peek px:xml-declaration /dev/rawin))
    (define charset : (Option Bytes) (and magic (let ([name (cdr magic)]) (and (pair? name) (car name)))))
    (define CHARSET : (Option String) (and charset (xml-fallback-charset charset)))
    (cond [(or (false? CHARSET) (string-ci=? CHARSET "UTF-8")) /dev/rawin]
          [else (with-handlers ([exn? (λ _ /dev/rawin)])
                  (reencode-input-port /dev/rawin CHARSET (car (assert magic pair?))
                                       #false (object-name /dev/rawin) #true
                                       (λ [[msg : String] [port : Input-Port]] : Nothing
                                         (error 'xml-fallback-encode-input-port
                                                (string-append msg ": ~e") port))))])))

#lang typed/racket/base

(provide (all-defined-out))
(provide css-read-syntax css-peek-syntax)

(require racket/path)
(require racket/port)

(require "digicore.rkt")
(require "tokenizer.rkt")

(require typed/racket/unsafe)

(unsafe-require/typed
 racket/base ; the output is gauranteed by the caller, hence the explicitly requiring.
 [[read-byte-or-special css-read-syntax] (->* (Input-Port) (U CSS-Token EOF))]
 [[peek-byte-or-special css-peek-syntax] (->* (Input-Port Natural) (U CSS-Token EOF))])

(define-type CSS-StdIn (U Input-Port Path-String Bytes (Listof CSS-Token)))

(define css-open-input-port : (-> CSS-StdIn Input-Port)
  ;;; https://drafts.csswg.org/css-syntax/#parser-entry-points
  (lambda [/dev/stdin]
    (if (list? /dev/stdin)
        (let ([total : Index (length /dev/stdin)]
              [cursor : Integer 0])
          (make-input-port (if (pair? /dev/stdin) (css-token-source (car /dev/stdin)) '/dev/cssin/null)
                           (λ [[buf : Bytes]]
                             (λ _ (cond [(>= cursor total) eof]
                                        [(set! cursor (add1 cursor))
                                         => (λ _ (list-ref /dev/stdin (sub1 cursor)))])))
                           (λ [[buf : Bytes] [skip : Nonnegative-Integer] [evt : Any]]
                             (λ _ (cond [(>= (+ skip cursor) total) eof]
                                        [else (list-ref /dev/stdin (+ skip cursor))])))
                           void))
        (let* ([/dev/rawin (cond [(port? /dev/stdin) /dev/stdin]
                                 [(path? /dev/stdin) (open-input-file /dev/stdin)]
                                 [(regexp-match? #px"\\.css$" /dev/stdin) (open-input-file (~a /dev/stdin))]
                                 [(string? /dev/stdin) (open-input-string /dev/stdin '/dev/cssin/string)]
                                 [(bytes? /dev/stdin) (open-input-bytes /dev/stdin '/dev/cssin/bytes)]
                                 [else (open-input-string (~s /dev/stdin) '/dev/cssin/error)])]
               [/dev/cssin : Input-Port (css-fallback-encode-input-port /dev/rawin)]
               [peek-pool : (Listof CSS-Syntax-Any) null])
          (define portname : (U String Symbol)
            (let ([src (object-name /dev/cssin)])
              (cond [(path? src) (path->string (simple-form-path src))]
                    [else (string->symbol (~a src))])))
          (make-input-port portname
                           (λ [[buf : Bytes]]
                             (λ _ (cond [(null? peek-pool) (css-consume-token /dev/cssin portname)]
                                        [else (let-values ([(rest peeked) (split-at-right peek-pool 1)])
                                                (set! peek-pool rest)
                                                (car peeked))])))
                           (λ [[buf : Bytes] [skip : Nonnegative-Integer] [evt : Any]]
                             ; NOTE: It seems that optimize this code to always throw the last peeked token
                             ;        does not improve the performance.
                             (λ _ (and (for ([idx (in-range (length peek-pool) (add1 skip))])
                                         (set! peek-pool (cons (css-consume-token /dev/cssin portname) peek-pool)))
                                       (list-ref peek-pool (- (length peek-pool) skip 1)))))
                           (λ [] (unless (eq? /dev/rawin /dev/cssin) (close-input-port /dev/cssin))
                                  (unless (eq? /dev/rawin /dev/stdin) (close-input-port /dev/rawin)))
                           #false #false
                           (λ [] (port-next-location /dev/cssin))
                           (λ [] (void (list css-fallback-encode-input-port '|has already set it|))))))))
    
(define css-read-syntax/skip-whitespace : (-> Input-Port CSS-Syntax-Any)
  (lambda [css]
    (define token (css-read-syntax css))
    (cond [(not (css:whitespace? token)) token]
          [else (css-read-syntax/skip-whitespace css)])))

(define css-peek-syntax/skip-whitespace : (-> Input-Port CSS-Syntax-Any)
  (lambda [css]
    (let peek/skip-whitespace : CSS-Syntax-Any ([skip : Nonnegative-Fixnum 0])
      (define token (css-peek-syntax css skip))
      (cond [(not (css:whitespace? token)) token]
            [else (peek/skip-whitespace (fx+ skip 1))]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define px.charset #px"^@charset \"(.*?)\";")

(define css-fallback-charset : (-> Bytes String)
  (lambda [from]
    (define CHARSET : String (string-upcase (bytes->string/utf-8 from)))
    (cond [(member CHARSET '("UTF-16BE" "UTF-16LE")) "UTF-8"]
          [else CHARSET])))
  
(define css-fallback-encode-input-port : (-> Input-Port Input-Port)
  ;;; https://drafts.csswg.org/css-syntax/#input-byte-stream
  ;;; https://drafts.csswg.org/css-syntax/#charset-rule
  (lambda [/dev/rawin]
    (unless (port-counts-lines? /dev/rawin) (port-count-lines! /dev/rawin))
    (when (regexp-match-peek #px"^#(lang|!)" /dev/rawin)
      ;; skip racket `#lang` line and blanks.
      (read-line /dev/rawin)
      (regexp-match #px"^\\s*" /dev/rawin))
    (define magic : (Option (Pairof Bytes (Listof (Option Bytes)))) (regexp-match-peek px.charset /dev/rawin))
    (define charset : (Option Bytes) (and magic (let ([name (cdr magic)]) (and (pair? name) (car name)))))
    (define CHARSET : (Option String) (and charset (css-fallback-charset charset)))
    (cond [(or (false? CHARSET) (string-ci=? CHARSET "UTF-8")) /dev/rawin]
          [else (with-handlers ([exn? (λ _ /dev/rawin)])
                  (reencode-input-port /dev/rawin CHARSET (car (assert magic pair?))
                                       #false (object-name /dev/rawin) #true
                                       (λ [[msg : String] [port : Input-Port]] : Nothing
                                         (error 'css-fallback-encode-input-port
                                                (string-append msg ": ~e") port))))])))

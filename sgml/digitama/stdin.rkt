#lang typed/racket/base

(provide (all-defined-out))
(provide xml-read-syntax xml-peek-syntax)

(require racket/path)
(require racket/port)
(require racket/unsafe/ops)

(require "digicore.rkt")

(require typed/racket/unsafe)

(unsafe-require/typed
 racket/base ; the output is gauranteed by the caller, hence the explicitly requiring.
 [[read-byte-or-special xml-read-syntax] (->* (Input-Port) (U XML-Token EOF))]
 [[peek-byte-or-special xml-peek-syntax] (->* (Input-Port Natural) (U XML-Token EOF))])

(require (for-syntax racket/base))

(define-type XML-StdIn (U Input-Port Path-String Bytes))
  
(define xml-open-input-port : (->* (XML-StdIn) (Boolean) (Values Input-Port (Option Nonnegative-Flonum) (Option String) Boolean))
  ;;; https://www.w3.org/TR/xml11/#sec-prolog-dtd
  ;;; https://www.w3.org/TR/xml11/#NT-EncodingDecl
  ;;; https://www.w3.org/TR/xml11/#sec-TextDecl
  (lambda [/dev/stdin [enable-line-counting? #false]]
    (define /dev/rawin : Input-Port
      (cond [(port? /dev/stdin) /dev/stdin]
            [(path? /dev/stdin) (open-input-file /dev/stdin)]
            [(regexp-match? #px"\\.xml$" /dev/stdin) (open-input-file (~a /dev/stdin))]
            [(string? /dev/stdin) (open-input-string /dev/stdin '/dev/xmlin/string)]
            [(bytes? /dev/stdin) (open-input-bytes /dev/stdin '/dev/xmlin/bytes)]
            [else (open-input-string (~s /dev/stdin) '/dev/xmlin/error)]))

    (when (and enable-line-counting? (not (port-counts-lines? /dev/rawin)))
      (port-count-lines! /dev/rawin))
    
    ;; skip racket `#lang` line
    (when (regexp-match-peek #px"^#(lang|!)" /dev/rawin)
      (read-line /dev/rawin))
    
    (let-values ([(version encoding standalone?) (xml-read-declaration /dev/rawin)])
      (values (cond [(or (not encoding) (string-ci=? encoding "UTF-8")) /dev/rawin]
                    [else (with-handlers ([exn? (λ _ /dev/rawin)])
                            (define-values (line column position) (port-next-location /dev/rawin))
                            (define /dev/xmlin : Input-Port
                              (reencode-input-port /dev/rawin encoding #false #true (object-name /dev/rawin) #true
                                                   (λ [[msg : String] [port : Input-Port]] : Nothing
                                                     (error 'xml-input-port (string-append encoding ": ~e: " msg) port))))
                            (when (and line column position)
                              (port-count-lines! /dev/xmlin)
                              (set-port-next-location! /dev/xmlin line column position))
                            /dev/xmlin)])
              version encoding standalone?))))

(define xml-port-name : (-> Input-Port (U String Symbol))
  (lambda [/dev/xmlin]
    (let ([src (object-name /dev/xmlin)])
      (cond [(path? src) (path->string (simple-form-path src))]
            [(symbol? src) src]
            [(string? src) (string->symbol src)]
            [else (string->symbol (~a src))]))))

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
(define xml-read-declaration : (-> Input-Port (Values (Option Nonnegative-Flonum) (Option String) Boolean))
  (lambda [/dev/xmlin]
    (cond [(not (regexp-try-match #px"^\\s*<[?][Xx][Mm][Ll]\\s+" /dev/xmlin)) (values 1.0 "UTF-8" #true)]
          [else (let read-property ([version : (Option Number) #false]
                                    [encoding : (Option String) #false]
                                    [standalone? : Boolean #true])
                  (define name=value (regexp-try-match #px"^([-a-zA-Z0-9]+)\\s*=\\s*[\"']([^\"'>]+)[\"']\\s*" /dev/xmlin))
                  (define name.value (and name=value (filter bytes? (cdr name=value))))
                  (if (not name.value)
                      (let ([?> (regexp-match #px"[^>]*[?]>" /dev/xmlin)])
                        (values (and (real? version) (positive? version) (real->double-flonum version)) encoding standalone?))
                      (let ([name (unsafe-car name.value)]
                            [value (unsafe-car (unsafe-cdr name.value))])
                        (cond [(bytes=? name #"version") (read-property (string->number (bytes->string/utf-8 value)) encoding standalone?)]
                              [(bytes=? name #"encoding") (read-property version (xml-fallback-charset value) standalone?)]
                              [(bytes=? name #"standalone") (read-property version encoding (regexp-match? #px#"^[Yy][Ee][Ss]$" value))]
                              [else (read-property version encoding standalone?)]))))])))

(define xml-fallback-charset : (-> Bytes String)
  (lambda [from]
    (define CHARSET : String (string-upcase (bytes->string/utf-8 from)))
    (cond [(member CHARSET '("UTF-16BE" "UTF-16LE")) "UTF-8"]
          [else CHARSET])))

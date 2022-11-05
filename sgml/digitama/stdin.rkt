#lang typed/racket/base

(provide (all-defined-out))
(provide (rename-out [Syn-Token-StdIn SGML-StdIn]
                     [syn-token-port-name sgml-port-name]))

(require racket/port)
(require racket/unsafe/ops)

(require digimon/token)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml-open-input-port : (->* (Syn-Token-StdIn) (Boolean) (Values Input-Port Nonnegative-Flonum (Option String) Boolean))
  ;;; https://www.w3.org/TR/xml/#sec-prolog-dtd
  ;;; https://www.w3.org/TR/xml/#NT-EncodingDecl
  ;;; https://www.w3.org/TR/xml/#sec-TextDecl
  (lambda [/dev/stdin [enable-line-counting? #false]]
    (define /dev/rawin : Input-Port (syn-token-stdin->port /dev/stdin #px"\\.t?xml$" 'xmlin))

    (when (and enable-line-counting? (not (port-counts-lines? /dev/rawin)))
      (port-count-lines! /dev/rawin))

    (syn-token-port-skip-lang-line /dev/rawin)
    
    (let-values ([(version encoding standalone?) (xml-read-declaration /dev/rawin)])
      (values (xml-reencode-port /dev/rawin encoding) version encoding standalone?))))

(define dtd-open-input-port : (->* (Syn-Token-StdIn) (Boolean (U String Symbol False)) Input-Port)
  (lambda [/dev/stdin [enable-line-counting? #false] [port-name #false]]
    (define /dev/rawin : Input-Port (syn-token-stdin->port /dev/stdin #px"\\.t?dtd$" 'dtdin port-name))

    (when (and enable-line-counting? (not (port-counts-lines? /dev/rawin)))
      (port-count-lines! /dev/rawin))
    
    (syn-token-port-skip-lang-line /dev/rawin)
    
    (let-values ([(version encoding standalone?) (xml-read-declaration /dev/rawin)])
      (xml-reencode-port /dev/rawin encoding))))

(define rnc-open-input-port : (->* (Syn-Token-StdIn) (Boolean (U String Symbol False)) Input-Port)
  (lambda [/dev/stdin [enable-line-counting? #false] [port-name #false]]
    (define /dev/rawin : Input-Port (syn-token-stdin->port /dev/stdin #px"\\.t?rnc$" 'rncin port-name))

    (when (and enable-line-counting? (not (port-counts-lines? /dev/rawin)))
      (port-count-lines! /dev/rawin))
    
    (syn-token-port-skip-lang-line /dev/rawin)
    
    (let-values ([(version encoding standalone?) (xml-read-declaration /dev/rawin)])
      (xml-reencode-port /dev/rawin encoding))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml-read-declaration : (-> Input-Port (Values Nonnegative-Flonum (Option String) Boolean))
  (lambda [/dev/xmlin]
    (cond [(not (regexp-try-match #px"^\\s*<[?][Xx][Mm][Ll]\\s+" /dev/xmlin)) (values 1.0 "UTF-8" #true)]
          [else (let read-property ([version : (Option Number) #false]
                                    [encoding : (Option String) #false]
                                    [standalone? : Boolean #false #| https://www.w3.org/TR/xml/#sec-rmd |#])
                  (define name=value (regexp-try-match #px"^([-a-zA-Z0-9]+)\\s*=\\s*[\"']([^\"'>]+)[\"']\\s*" /dev/xmlin))
                  (define name.value (and name=value (filter bytes? (cdr name=value))))
                  (if (not name.value)
                      (let ([?> (regexp-match #px"[^>]*[?]>\\s*" /dev/xmlin)])
                        (values (or (and (real? version) (positive? version) (real->double-flonum version)) 0.0) encoding standalone?))
                      (let ([name (unsafe-car name.value)]
                            [value (unsafe-car (unsafe-cdr name.value))])
                        (cond [(bytes=? name #"version") (read-property (string->number (bytes->string/utf-8 value)) encoding standalone?)]
                              [(bytes=? name #"encoding") (read-property version (syn-token-fallback-charset value) standalone?)]
                              [(bytes=? name #"standalone") (read-property version encoding (regexp-match? #px#"^[Yy][Ee][Ss]$" value))]
                              [else (read-property version encoding standalone?)]))))])))

(define xml-reencode-port : (-> Input-Port (Option String) Input-Port)
  (lambda [/dev/rawin encoding]
    (cond [(or (not encoding) (string-ci=? encoding "UTF-8")) /dev/rawin]
          [else (with-handlers ([exn? (λ _ /dev/rawin)])
                  (define-values (line column position) (port-next-location /dev/rawin))
                  (define /dev/xmlin : Input-Port
                    (reencode-input-port /dev/rawin encoding #false #true (object-name /dev/rawin) #true
                                         (λ [[msg : String] [port : Input-Port]] : Nothing
                                           (error 'xml-input-port (string-append encoding ": ~e: " msg) port))))
                  (when (and line column position)
                    (port-count-lines! /dev/xmlin)
                    (set-port-next-location! /dev/xmlin line column position))
                  /dev/xmlin)])))

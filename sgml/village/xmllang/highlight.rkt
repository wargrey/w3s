#lang racket/base

(provide xml-lexer)

(require sgml/digitama/digicore)
(require sgml/digitama/tokenizer)

(define xml-lexer ;: (-> Input-Port Natural XML-Parser-Mode (Values (U String EOF) Symbol (Option Symbol) (Option Integer) (Option Integer) Natural XML-Parser-Mode))
  (lambda [/dev/drin offset mode]
    (log-message (current-logger) 'debug 'exn:ssh (~a (list offset mode (peek-bytes 8 0 /dev/drin))))
    (define-values (t next-mode) (xml-consume-token* /dev/drin '/dev/drin mode))
    (cond [(eof-object? t) (values eof 'eof #false #false #false 0 next-mode)]
          [(xml:whitespace? t) (xml-hlvalues t (if (string? (xml:whitespace-datum t)) 'comment 'white-space) #false next-mode)]
          [(xml:name? t) (xml-hlvalues t (xml-id->drtype (xml:name-datum t)) #false next-mode)]
          [(xml:open? t) (xml-hlvalues t 'parenthesis (string->symbol (string (xml:delim-datum t))) next-mode)]
          [(xml:close? t) (xml-hlvalues t 'parenthesis (string->symbol (string (xml:delim-datum t))) next-mode)]
          [(xml:delim? t) (xml-hlvalues t (xml-char->drtype (xml:delim-datum t)) #false next-mode)]
          [else (xml-hlvalues t (xml-other->drtype t) #false next-mode)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml-char->drtype ;: (-> Char Symbol)
  (lambda [delim]
    (case delim
      [(#\: #\, #\;) 'sexp-comment]
      [(#\+ #\- #\* #\/) 'symbol]
      [else 'constant])))
  
(define xml-id->drtype ;: (-> Symbol Symbol)
  (lambda [id]
    (cond [(symbol-unreadable? id) 'no-color]
          [else 'symbol])))
  
(define xml-other->drtype ;: (-> XML-Token Symbol)
  (lambda [token]
    (cond [(xml:string? token) 'string]
          [(xml:bad? token) 'error]
          [else 'other])))

(define xml-hlvalues ;: (-> XML-Token Symbol (Option Symbol) XML-Parser-Mode (Values String Symbol (Option Symbol) (Option Integer) (Option Integer) Natural XML-Parser-Mode))
  (lambda [t type subtype mode]
    (values "" type subtype (xml-token-start t) (xml-token-end t) 0 mode)))

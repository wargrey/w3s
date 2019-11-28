#lang racket/base

(provide xml-lexer)

(require sgml/digitama/digicore)
(require sgml/digitama/tokenizer)

(define xml-lexer ;: (-> Input-Port Natural XML-Parser-Mode (Values (U String EOF) Symbol (Option Symbol) (Option Integer) (Option Integer) Natural XML-Parser-Mode))
  (lambda [/dev/drin offset mode]
    (define-values (t next-mode) (xml-consume-token* /dev/drin '/dev/drin mode))
    (cond [(eof-object? t) (values eof 'eof #false #false #false 0 next-mode)]
          [(xml:whitespace? t) (xml-hlvalues t 'white-space #false next-mode)]
          [(xml:name? t) (xml-hlvalues t 'symbol #false next-mode)]
          [(xml:string? t) (xml-hlvalues t 'string #false next-mode)]
          [(xml:open? t) (xml-hlvalues t 'parenthesis '|(| next-mode)]
          [(xml:close? t) (xml-hlvalues t 'parenthesis '|)| next-mode)]
          [(xml:eq? t) (xml-hlvalues t 'constant #false next-mode)]
          [(xml:char? t) (xml-hlvalues t 'constant #false next-mode)]
          [(xml:entity? t) (xml-hlvalues t 'constant #false next-mode)]
          [(xml:keyword? t) (xml-hlvalues t 'keyword #false next-mode)]
          [(xml:bad? t) (xml-hlvalues t 'error #false next-mode)]
          [else (xml-hlvalues t 'other #false next-mode)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml-hlvalues ;: (-> XML-Token Symbol (Option Symbol) XML-Parser-Mode (Values String Symbol (Option Symbol) (Option Integer) (Option Integer) Natural XML-Parser-Mode))
  (lambda [t type subtype mode]
    (values "" type subtype (xml-token-start t) (xml-token-end t) 0 mode)))

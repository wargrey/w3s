#lang racket/base

(provide xml-lexer)

(require sgml/digitama/digicore)
(require sgml/digitama/tokenizer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml-lexer ;: (-> Input-Port Natural XML-Parser-Mode (Values (U String EOF) Symbol (Option Symbol) (Option Integer) (Option Integer) Natural XML-Parser-Mode))
  (lambda [/dev/drin offset mode]
    (define-values (t next-mode) (xml-consume-token* /dev/drin '/dev/drin mode))
    
    (cond [(eof-object? t) (values eof 'eof #false #false #false 0 next-mode)]
          [(xml:comment? t) (xml-hlvalues t 'comment #false offset next-mode)]
          [(xml:whitespace? t) (xml-hlvalues t 'white-space #false offset next-mode)]
          [(xml:name? t) (xml-hlvalues t 'symbol #false offset next-mode)]
          [(xml:string? t) (xml-hlvalues t 'string #false offset next-mode)]
          [(xml:open? t) (xml-hlvalues t 'parenthesis (xml-open-type t) offset next-mode)]
          [(xml:close? t) (xml-hlvalues t 'parenthesis (xml-close-type t) offset next-mode)]
          [(xml:delim? t) (xml-hlvalues t 'constant #false offset next-mode)]
          [(xml:entity? t) (xml-hlvalues t 'constant #false offset next-mode)]
          [(xml:keyword? t) (xml-hlvalues t 'keyword #false offset next-mode)]
          [(xml:bad? t) (xml-hlvalues t 'error #false offset next-mode)]
          [else (xml-hlvalues t 'other #false offset next-mode)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml-hlvalues ;: (-> XML-Token Symbol (Option Symbol) Index XML-Parser-Mode (Values String Symbol (Option Symbol) (Option Integer) (Option Integer) Natural XML-Parser-Mode))
  (lambda [t type subtype offset mode]
    (define pos (xml-token-start t))
    
    (values "" type subtype pos (xml-token-end t)
            (+ pos offset) mode)))

(define xml-open-type ;: (-> XML:Open Symbol)
  (lambda [t]
    (define datum (xml-token->datum t))

    (cond [(eq? datum #\() '|(|]
          [(eq? datum #\[) '|]|]
          [else '|{|])))

(define xml-close-type ;: (-> XML:Close Symbol)
  (lambda [t]
    (define datum (xml-token->datum t))

    (cond [(eq? datum #\)) '|)|]
          [(eq? datum #\]) '|]|]
          [else '|}|])))

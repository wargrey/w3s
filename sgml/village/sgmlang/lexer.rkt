#lang racket/base

(provide xml-lexer)
(provide (rename-out [xml-lexer dtd-lexer]
                     [xml-lexer rnc-lexer]))

(require sgml/digitama/digicore)
(require sgml/digitama/tokenizer)
(require sgml/digitama/tokenizer/port)
(require sgml/digitama/tokenizer/delimiter)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml-lexer
  (lambda [/dev/drin offset mode]
    (define-values (t next-mode) (xml-consume-token* /dev/drin '/dev/drin mode))
    
    (cond [(eof-object? t) (values eof 'eof #false #false #false 0 next-mode)]
          [(xml:comment? t) (xml-hlvalues t 'comment #false offset next-mode)]
          [(xml:whitespace? t) (xml-hlvalues t 'white-space #false offset next-mode)]
          [(xml:open? t) (xml-hlvalues t 'parenthesis (xml-open-type t) offset next-mode)]
          [(xml:close? t) (xml-hlvalues t 'parenthesis (xml-close-type t) offset next-mode)]
          [(xml:name? t) (xml-hlvalues t 'symbol #false offset next-mode)]
          [(xml:&string? t) (xml-hlvalues t 'constant #false offset next-mode)]
          [(xml:string? t) (xml-hlvalues t 'string #false offset next-mode)]
          [(xml:delim? t) (xml-hlvalues t 'constant #false offset next-mode)]
          [(xml:char? t) (xml-hlvalues t 'constant #false offset next-mode)]
          [(xml:reference? t) (xml-hlvalues t 'constant #false offset next-mode)]
          [(xml:pereference? t) (xml-hlvalues t 'keyword #false offset next-mode)]
          [(xml:bad? t) (xml-hlvalues t 'error #false offset next-mode)]
          [else (xml-hlvalues t 'other #false offset next-mode)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml-hlvalues
  (lambda [t type subtype offset mode]
    (values "" type subtype

            (w3s-token-start t) (w3s-token-end t)
            
            ; the `backup distance` is nothing but an alternative to the `bad token`
            0

            ; NOTE: Any mode would dramatically slow down the progress, and in proportion to the size of source.
            ; NOTE: DrRacket may do tokenizing at any starting position, thus, prev position must be dropped.
            (let ([scope (xml-parser-env-scope mode)])
              (cond [(eq? scope xml-default-scope) #false]
                    [else (cons (xml-parser-env-consume mode) scope)])))))

(define xml-open-type ;: (-> XML:Open Symbol)
  (lambda [t]
    (define datum (xml-token->datum t))

    (cond [(eq? datum #\() '|(|]
          [(or (eq? datum #\[) (eq? datum <!&CDATA&) (eq? datum <!&)) '|[|]
          [else '|{|])))

(define xml-close-type ;: (-> XML:Close Symbol)
  (lambda [t]
    (define datum (xml-token->datum t))

    (cond [(eq? datum #\)) '|)|]
          [(or (eq? datum #\]) (eq? datum $$>)) '|]|]
          [else '|}|])))

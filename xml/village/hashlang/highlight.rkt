#lang racket/base

(provide xml-lexer)

(require xml/digitama/digicore)
(require xml/digitama/tokenizer)

(define xml-lexer ;: (-> Input-Port (Values (U String EOF) Symbol (Option Symbol) (Option Integer) (Option Integer)))
  (lambda [/dev/drin offset mode]
    (define t #|: CSS-Syntax-Any|# (xml-consume-token /dev/drin '/dev/drin))
    (cond [(eof-object? t) (values eof 'eof #false #false #false 0 (not mode))]
          [(xml:whitespace? t) (xml-hlvalues t (if (string? (xml:whitespace-datum t)) 'comment 'white-space) #false mode)]
          [(xml:ident? t) (xml-hlvalues t (xml-id->drtype (xml:ident-norm t) #false) #false mode)]
          [(xml:function? t) (xml-hlvalues t (xml-id->drtype (xml:function-norm t) #true) '|(| mode)]
          [(xml:open? t) (xml-hlvalues t 'parenthesis (string->symbol (string (xml:delim-datum t))) mode)]
          [(xml:close? t) (xml-hlvalues t 'parenthesis (string->symbol (string (xml:close-datum t))) mode)]
          [(xml:delim? t) (xml-hlvalues t (xml-char->drtype (xml:delim-datum t)) #false mode)]
          [(xml-numeric? t) (xml-hlvalues t (if (xml-nan? t) 'error 'constant) #false mode)]
          [(xml:url? t) (xml-hlvalues t 'parenthesis '|(| mode)]
          [else (xml-hlvalues t (xml-other->drtype t) #false mode)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml-char->drtype ;: (-> Char Symbol)
  (lambda [delim]
    (case delim
      [(#\: #\, #\;) 'sexp-comment]
      [(#\+ #\- #\* #\/) 'symbol]
      [else 'constant])))
  
(define xml-id->drtype ;: (-> Symbol Boolean Symbol)
  (lambda [id func?]
    (case id
      [(inherit important true false) 'constant]
      [(initial unset revert) 'sexp-comment]
      [(only not and or) 'no-color]
      [else (cond [(and func?) 'parenthesis]
                  [(symbol-unreadable? id) 'no-color]
                  [else 'symbol])])))
  
(define xml-other->drtype ;: (-> CSS-Token Symbol)
  (lambda [token]
    (cond [(xml:string? token) 'string]
          [(xml:hash? token) 'hash-colon-keyword]
          [(xml:@keyword? token) 'hash-colon-keyword]
          [(xml:urange? token) 'constant]
          [(xml:bad? token) 'error]
          [else 'other])))

(define xml-hlvalues ;: (-> CSS-Token Symbol (Option Symbol) (Values String Symbol (Option Symbol) (Option Integer) (Option Integer)))
  (lambda [t type subtype mode]
    (values "" type subtype (xml-token-start t) (xml-token-end t) 0 (not mode))))

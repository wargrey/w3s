#lang racket/base

(provide css-lexer)

(require css/digitama/syntax/digicore)
(require css/digitama/syntax/tokenizer)

(define css-lexer ;: (-> Input-Port Natural Any (Values (U String EOF) Symbol (Option Symbol) (Option Integer) (Option Integer) Natural Any))
  (lambda [/dev/drin offset mode]
    (define t #|: CSS-Syntax-Any|# (css-consume-token /dev/drin '/dev/drin))
    (cond [(eof-object? t) (values eof 'eof #false #false #false 0 (not mode))]
          [(css:whitespace? t) (css-hlvalues t (if (string? (css:whitespace-datum t)) 'comment 'white-space) #false mode)]
          [(css:ident? t) (css-hlvalues t (css-id->drtype (css:ident-norm t) #false) #false mode)]
          [(css:function? t) (css-hlvalues t (css-id->drtype (css:function-norm t) #true) '|(| mode)]
          [(css:open? t) (css-hlvalues t 'parenthesis (string->symbol (string (css:delim-datum t))) mode)]
          [(css:close? t) (css-hlvalues t 'parenthesis (string->symbol (string (css:close-datum t))) mode)]
          [(css:delim? t) (css-hlvalues t (css-char->drtype (css:delim-datum t)) #false mode)]
          [(css-numeric? t) (css-hlvalues t (if (css-nan? t) 'error 'constant) #false mode)]
          [(css:url? t) (css-hlvalues t 'parenthesis '|(| mode)]
          [else (css-hlvalues t (css-other->drtype t) #false mode)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define css-char->drtype ;: (-> Char Symbol)
  (lambda [delim]
    (case delim
      [(#\: #\, #\;) 'sexp-comment]
      [(#\+ #\- #\* #\/) 'symbol]
      [else 'constant])))
  
(define css-id->drtype ;: (-> Symbol Boolean Symbol)
  (lambda [id func?]
    (case id
      [(inherit important true false) 'constant]
      [(initial unset revert) 'sexp-comment]
      [(only not and or) 'no-color]
      [else (cond [(and func?) 'parenthesis]
                  [(symbol-unreadable? id) 'no-color]
                  [else 'symbol])])))
  
(define css-other->drtype ;: (-> CSS-Token Symbol)
  (lambda [token]
    (cond [(css:string? token) 'string]
          [(css:hash? token) 'hash-colon-keyword]
          [(css:@keyword? token) 'hash-colon-keyword]
          [(css:urange? token) 'constant]
          [(css:bad? token) 'error]
          [else 'other])))

(define css-hlvalues ;: (-> CSS-Token Symbol (Option Symbol) Any (Values String Symbol (Option Symbol) (Option Integer) (Option Integer) Natural Any))
  (lambda [t type subtype mode]
    (values "" type subtype (w3s-token-start t) (w3s-token-end t) 0 (not mode))))

#lang typed/racket/base

;;; https://drafts.xmlwg.org/xml-syntax/#tokenization

(provide (all-defined-out))

(require "digicore.rkt")
(require "delimiter.rkt")
(require "stdin.rkt")
(require "misc.rkt")

(require "tokenizer/port.rkt")

(require typed/racket/unsafe)

(require (for-syntax racket/base))

(unsafe-require/typed
 racket/base ; the line is gauranteed to count, hence the explicitly requiring.
 [port-next-location (-> Port (Values Positive-Integer Natural Positive-Integer))])

(define-syntax (xml-make-token stx)
  (syntax-case stx []
    [(_ source prev-mode end make-xml:token datum ...)
     #'(make-xml:token source (xml-parser-mode-line prev-mode) (xml-parser-mode-column prev-mode) (xml-parser-mode-position prev-mode) end datum ...)]))
  
(define-syntax (xml-make-bad-token stx)
  (syntax-case stx []
    [(_ source prev-mode end xml:bad:sub token datum)
     #'(let ([bad (xml-make-token source prev-mode end xml:bad:sub (~s (cons (object-name token) datum)))])
         (xml-log-read-error (xml-token->string bad))
         bad)]))

(struct xml-parser-mode
  ([scope : XML-Scope]
   [open-type : (Option Char)]
   [line : Positive-Integer]
   [column : Natural]
   [position : Positive-Integer])
  #:type-name XML-Parser-Mode
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define read-xml-tokens* : (-> Input-Port (Listof XML-Token))
  (lambda [/dev/xmlin]
    (define source : (U String Symbol) (xml-port-name /dev/xmlin))
    (let read-xml ([snekot : (Listof XML-Token) null]
                   [mode : (Option XML-Parser-Mode) #false])
      (define-values (token mode++) (xml-consume-token* /dev/xmlin source mode))
      (cond [(eof-object? token) (reverse snekot)]
            [else (read-xml (cons token snekot) mode++)]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml-consume-token* : (-> Input-Port (U String Symbol) (Option XML-Parser-Mode) (Values (U XML-Token EOF) XML-Parser-Mode))
  (lambda [/dev/xmlin source mode]
    (define prev-mode : XML-Parser-Mode
      (cond [(xml-parser-mode? mode) mode]
            [else (let-values ([(line column position) (port-next-location /dev/xmlin)])
                    (xml-parser-mode 'TopLevel #false line column position))]))
    (define prev-scope : XML-Scope (xml-parser-mode-scope prev-mode))
    (define self-open : (Option Char) (xml-parser-mode-open-type prev-mode))
    (define-values (datum next-scope) (xml-consume-token /dev/xmlin prev-scope))
    (define-values (line column end) (port-next-location /dev/xmlin))

    (values (cond [(xml-white-space? datum)
                   (if (xml-comment? datum)
                       (xml-make-token source prev-mode end xml:comment (xml-white-space-raw datum))
                       (xml-make-token source prev-mode end xml:whitespace (xml-white-space-raw datum)))]
                  [(symbol? datum)
                   (cond [(symbol-interned? datum) (xml-make-token source prev-mode end xml:name datum)]
                         [(or (eq? datum <_) (eq? datum </) (eq? datum <!) (eq? datum <?)) (xml-make-token source prev-mode end xml:open datum)]
                         [(or (eq? datum _>) (eq? datum />) (eq? datum ?>)) (xml-make-token source prev-mode end xml:close datum)]
                         [(eq? datum :=) (xml-make-token source prev-mode end xml:eq datum)]
                         [else (xml-make-token source prev-mode end xml:entity datum)])]
                  [(string? datum) (xml-make-token source prev-mode end xml:string datum)]
                  [(char? datum) (xml-make-token source prev-mode end xml:char datum)]
                  [(keyword? datum) (xml-make-token source prev-mode end xml:keyword datum)]
                  [(eof-object? datum) eof]
                  [else (xml-make-bad-token source prev-mode end xml:bad:stdin xml:string (list->string datum))])
            (xml-parser-mode next-scope self-open line column end))))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax (xml-make-token stx)
  (syntax-case stx []
    [(_ source prev-env end make-xml:token datum ...)
     #'(make-xml:token source (xml-parser-env-line prev-env) (xml-parser-env-column prev-env) (xml-parser-env-position prev-env) end datum ...)]))
  
(define-syntax (xml-make-bad-token stx)
  (syntax-case stx []
    [(_ source prev-env end xml:bad:sub datum)
     #'(let ([bad (xml-make-token source prev-env end xml:bad:sub (~s datum))])
         (xml-log-read-error (xml-token->string bad))
         bad)]))

(struct xml-parser-env
  ([consume : XML-Token-Consumer]
   [scope : XML-Scope]
   [line : Positive-Integer]
   [column : Natural]
   [position : Positive-Integer])
  #:type-name XML-Parser-ENV
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define read-xml-tokens* : (-> Input-Port (Listof XML-Token))
  (lambda [/dev/xmlin]
    (define source : (U String Symbol) (xml-port-name /dev/xmlin))
    (let read-xml ([snekot : (Listof XML-Token) null]
                   [env : (Option XML-Parser-ENV) #false])
      (define-values (token env++) (xml-consume-token* /dev/xmlin source env))
      (cond [(eof-object? token) (reverse snekot)]
            [else (read-xml (cons token snekot) env++)]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml-consume-token* : (-> Input-Port (U String Symbol) (U False XML-Parser-ENV XML-Token-Consumer) (Values (U XML-Token EOF) XML-Parser-ENV))
  (lambda [/dev/xmlin source env]
    (define prev-env : XML-Parser-ENV
      (cond [(xml-parser-env? env) env]
            [else (let-values ([(line column position) (port-next-location /dev/xmlin)])
                    (xml-parser-env (or env xml-consume-token:*) xml-initial-scope line column position))]))
    (define-values (datum next-consume next-scope)
      (xml-consume-token /dev/xmlin (xml-parser-env-consume prev-env) (xml-parser-env-scope prev-env)))
    (define-values (line column end) (port-next-location /dev/xmlin))

    (values (cond [(xml-white-space? datum)
                   (if (xml-comment? datum)
                       (xml-make-token source prev-env end xml:comment (xml-white-space-raw datum))
                       (xml-make-token source prev-env end xml:whitespace (xml-white-space-raw datum)))]
                  [(char? datum)
                   (cond [(eq? datum #\<) (xml-make-token source prev-env end xml:open datum)]
                         [(eq? datum #\>) (xml-make-token source prev-env end xml:close datum)]
                         [(eq? datum #\=) (xml-make-token source prev-env end xml:eq datum)]
                         [(memq datum '(#\( #\[)) (xml-make-token source prev-env end xml:open datum)]
                         [(memq datum '(#\) #\])) (xml-make-token source prev-env end xml:close datum)]
                         [else (xml-make-token source prev-env end xml:delim datum)])]
                  [(symbol? datum)
                   (cond [(symbol-interned? datum) (xml-make-token source prev-env end xml:name datum)]
                         [(eq? datum </) (xml-make-token source prev-env end xml:open datum)]
                         [(eq? datum />) (xml-make-token source prev-env end xml:close datum)]
                         [(eq? datum <?) (xml-make-token source prev-env end xml:pi datum)]
                         [(memq datum (list <! <!$ <!$CDATA$)) (xml-make-token source prev-env end xml:open datum)]
                         [(memq datum (list ?> $$>)) (xml-make-token source prev-env end xml:close datum)]
                         [else (xml-make-token source prev-env end xml:entity datum)])]
                  [(string? datum) (xml-make-token source prev-env end xml:string datum)]
                  [(index? datum) (xml-make-token source prev-env end xml:entity datum)]
                  [(keyword? datum) (xml-make-token source prev-env end xml:keyword datum)]
                  [(eof-object? datum) eof]
                  [else (xml-make-bad-token source prev-env end xml:bad (list->string datum))])
            (xml-parser-env next-consume next-scope line column end))))

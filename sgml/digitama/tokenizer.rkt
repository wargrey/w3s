#lang typed/racket/base

;;; https://drafts.xmlwg.org/xml-syntax/#tokenization

(provide (all-defined-out))

(require "digicore.rkt")
(require "misc.rkt")

(require "tokenizer/port.rkt")

(require racket/fixnum)

(require typed/racket/unsafe)

(require (for-syntax racket/base))

(unsafe-require/typed
 racket/base ; the line is gauranteed to count, hence the explicitly requiring.
 [port-next-location (-> Port (Values Positive-Integer Natural Positive-Integer))])

(struct xml-srcloc
  ([in : Input-Port]
   [source : (U String Symbol)]
   [line : Positive-Integer]
   [column : Natural]
   [position : Positive-Integer])
  #:type-name XML-Srcloc)

(define-syntax (xml-make-token stx)
  (syntax-case stx []
    [(_ source line column start end make-xml:token datum ...)
     #'(make-xml:token source line column start end datum ...)]))
  
(define-syntax (xml-make-bad-token stx)
  (syntax-case stx []
    [(_ source line column start end xml:bad:sub token datum)
     #'(let ([bad (xml-make-token source line column start end xml:bad:sub (~s (cons (object-name token) datum)))])
         (xml-log-read-error (xml-token->string bad))
         bad)]))

(struct xml-parser-mode
  ([leader-char : (U Char EOF)]
   [literal-type : XML-Literal])
  #:type-name XML-Parser-Mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml-consume-token* : (-> Input-Port Symbol (Option XML-Parser-Mode) (Values (U XML-Token EOF) XML-Parser-Mode))
  (lambda [/dev/xmlin source mode]
    (define prev-mode : XML-Parser-Mode (or mode (xml-parser-mode (read-char /dev/xmlin) 'Attribute)))
    (define leader-char : (U Char EOF) (xml-parser-mode-leader-char prev-mode))
    (define prev-type : XML-Literal (xml-parser-mode-literal-type prev-mode))
    (if (eof-object? leader-char)
        (values eof prev-mode)
        (let-values ([(line column start) (port-next-location /dev/xmlin)])
          (define-values (datum maybe-leader) (xml-consume-token /dev/xmlin leader-char prev-type))
          (define-values (_line _column end) (port-next-location /dev/xmlin))
          (values (cond [(symbol? datum) (xml-make-token source line column start end xml:name datum)]
                        [(string? datum) (xml-make-token source line column start end xml:string datum)]
                        [else (xml-make-bad-token source line column start end xml:bad:stdin xml:cdata datum)])
                  (xml-parser-mode maybe-leader (xml-next-literal-type datum prev-type)))))))

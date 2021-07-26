#lang typed/racket/base

(provide (all-defined-out) port-next-location)

(require css/digitama/syntax/w3s)

(require "digicore.rkt")

(require "tokenizer/delimiter.rkt")
(require "tokenizer/port.rkt")

(require typed/racket/unsafe)

(unsafe-require/typed
 racket/base ; the line is gauranteed to count, hence the explicitly requiring.
 [port-next-location (-> Port (Values Positive-Integer Natural Positive-Integer))])

(require (for-syntax racket/base))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax (xml-make-token stx)
  (syntax-case stx []
    [(_ source prev-env end make-xml:token datum ...)
     (syntax/loc stx (make-xml:token source (xml-parser-env-line prev-env) (xml-parser-env-column prev-env) (xml-parser-env-position prev-env) end datum ...))]))
  
(define-syntax (xml-make-bad-token stx)
  (syntax-case stx []
    [(_ source prev-env end xml:bad:sub datum)
     (syntax/loc stx
       (let ([bad (xml-make-token source prev-env end xml:bad:sub datum)])
         (w3s-log-exn (xml-token->string bad) 'exn:xml:read)
         bad))]))

(define-syntax (xml-datum->token stx)
  (syntax-case stx []
    [(_ source prev-env end datum)
     (syntax/loc stx
       (cond [(xml-white-space? datum)
              (cond [(xml-comment? datum) (xml-make-token source prev-env end xml:comment (xml-white-space-raw datum))]
                    [(xml-new-line? datum) (xml-make-token source prev-env end xml:newline (xml-white-space-raw datum))]
                    [else (xml-make-token source prev-env end xml:whitespace (xml-white-space-raw datum))])]
             [(char? datum)
              (cond [(eq? datum #\<) (xml-make-token source prev-env end xml:stag datum)]
                    [(eq? datum #\>) (xml-make-token source prev-env end xml:etag datum)] ; close delimiter for Decls and EndTags
                    [(eq? datum #\=) (xml-make-token source prev-env end xml:eq datum)]
                    [(or (eq? datum #\() (eq? datum #\[) (eq? datum #\{)) (xml-make-token source prev-env end xml:open datum)]
                    [(or (eq? datum #\)) (eq? datum #\]) (eq? datum #\})) (xml-make-token source prev-env end xml:close datum)]
                    [(eq? datum #\%) (xml-make-token source prev-env end xml:pe datum)]
                    [else (xml-make-token source prev-env end xml:delim datum) #| `>` for non-empty start tag |#])]
             [(symbol? datum)
              (cond [(symbol-interned? datum) (xml-make-token source prev-env end xml:name datum)]
                    [(eq? datum />) (xml-make-token source prev-env end xml:etag datum)] ; close delimiter for empty Elements
                    [(eq? datum stag>) (xml-make-token source prev-env end xml:cstag datum)] ; not an close delimiter
                    [(eq? datum </) (xml-make-token source prev-env end xml:oetag datum)] ; not an open delimiter
                    [(eq? datum <?) (xml-make-token source prev-env end xml:pi datum)]
                    [(eq? datum <!) (xml-make-token source prev-env end xml:decl datum)]
                    [(eq? datum <!&) (xml-make-token source prev-env end xml:csec datum)]
                    [(eq? datum csec&) (xml-make-token source prev-env end xml:csec$ datum)]
                    [(eq? datum <!&CDATA&) (xml-make-token source prev-env end xml:open datum)]
                    [(or (eq? datum ?>) (eq? datum $$>)) (xml-make-token source prev-env end xml:close datum)]
                    [(eq? datum /=) (xml-make-token source prev-env end xml:/eq datum)] ; for RelaxNG
                    [(eq? datum &=) (xml-make-token source prev-env end xml:&eq datum)] ; for RelaxNG
                    [else (xml-make-token source prev-env end xml:reference datum)])]
             [(string? datum) (xml-make-token source prev-env end xml:string datum)]
             [(box? datum) (xml-make-token source prev-env end xml:&string (assert (unbox datum) string?))]
             [(index? datum) (xml-make-token source prev-env end xml:char datum)]
             [(keyword? datum) (xml-make-token source prev-env end xml:pereference datum)]
             [(pair? datum) (xml-make-bad-token source prev-env end xml:bad (cons (list->string (car datum)) (cdr datum)))]
             [else eof]))]))

(struct xml-parser-env
  ([consume : XML-Token-Consumer]
   [scope : XML-Scope]
   [line : Positive-Integer]
   [column : Natural]
   [position : Positive-Integer])
  #:type-name XML-Parser-ENV
  #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define read-xml-tokens* : (->* (Input-Port (U String Symbol)) ((U False XML-Parser-ENV (Pairof XML-Token-Consumer XML-Scope))) (Listof XML-Token))
  (lambda [/dev/xmlin source [initial-env #false]]
    (let read-xml ([snekot : (Listof XML-Token) null]
                   [env : (U False XML-Parser-ENV (Pairof XML-Token-Consumer XML-Scope)) initial-env])
      (define-values (token env++) (xml-consume-token* /dev/xmlin source env))
      (cond [(eof-object? token) (reverse snekot)]
            [else (read-xml (cons token snekot) env++)]))))

(define read-xml-content-tokens* : (->* (Input-Port (U String Symbol)) (Index) (Listof XML-Token))
  (lambda [/dev/xmlin source [doc-depth 0]]
    (read-xml-tokens* /dev/xmlin source
                      (cons xml-consume-token:* doc-depth))))
  
(define read-dtd-declaration-tokens* : (-> Input-Port (U String Symbol) Symbol (Listof XML-Token))
  ; for ELEMENT and ATTLIST
  (lambda [/dev/xmlin source DECLNAME]
    (filter-not xml:whitespace?
                (read-xml-tokens* /dev/xmlin source
                                  (cons xml-consume-token:* DECLNAME)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml-consume-token* : (-> Input-Port (U String Symbol)
                                 (U False XML-Parser-ENV (Pairof XML-Token-Consumer XML-Scope))
                                 (Values (U XML-Token EOF) XML-Parser-ENV))
  (lambda [/dev/xmlin source env]
    (define prev-env : XML-Parser-ENV
      (cond [(xml-parser-env? env) env]
            [else (let-values ([(line column position) (port-next-location /dev/xmlin)])
                    (cond [(not env) (xml-parser-env xml-consume-token:* xml-initial-scope line column position)]
                          [else (xml-parser-env (car env) (cdr env) line column position)]))]))
    (define-values (datum next-consume next-scope)
      (xml-consume-token /dev/xmlin (xml-parser-env-consume prev-env) (xml-parser-env-scope prev-env)))
    (define-values (line column end) (port-next-location /dev/xmlin))
    (define env++ : XML-Parser-ENV (xml-parser-env next-consume next-scope line column end))

    (values (xml-datum->token source prev-env end datum) env++)))

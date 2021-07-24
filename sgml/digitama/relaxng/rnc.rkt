#lang typed/racket/base

;;; https://relaxng.org/compact-20021121.html

(provide (all-defined-out))

(require racket/unsafe/ops)

(require "../digicore.rkt")
(require "../tokenizer.rkt")

(require "../tokenizer/port.rkt")
(require "../tokenizer/errno.rkt")
(require "../tokenizer/characters.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define read-rnc-tokens* : (->* (Input-Port (U String Symbol)) ((U False XML-Parser-ENV (Pairof XML-Token-Consumer XML-Scope))) (Listof XML-Token))
  (lambda [/dev/rncin source [initial-env #false]]
    (let read-rnc ([snekot : (Listof XML-Token) null]
                   [env : (U False XML-Parser-ENV (Pairof XML-Token-Consumer XML-Scope)) initial-env])
      (define-values (token env++) (rnc-consume-token* /dev/rncin source env))
      (cond [(eof-object? token) (reverse snekot)]
            [else (read-rnc (cons token snekot) env++)]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define rnc-consume-token* : (-> Input-Port (U String Symbol)
                                 (U False XML-Parser-ENV (Pairof XML-Token-Consumer XML-Scope))
                                 (Values (U XML-Token EOF) XML-Parser-ENV))
  (lambda [/dev/rncin source env]
    (define prev-env : XML-Parser-ENV
      (cond [(xml-parser-env? env) env]
            [else (let-values ([(line column position) (port-next-location /dev/rncin)])
                    (cond [(not env) (xml-parser-env rnc-consume-token:* xml-initial-scope line column position)]
                          [else (xml-parser-env (car env) (cdr env) line column position)]))]))
    (define-values (datum next-consume next-scope)
      (xml-consume-token /dev/rncin (xml-parser-env-consume prev-env) (xml-parser-env-scope prev-env)))
    (define-values (line column end) (port-next-location /dev/rncin))
    (define env++ : XML-Parser-ENV (xml-parser-env next-consume next-scope line column end))

    (values (xml-datum->token source prev-env end datum) env++)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define rnc-consume-token:* : XML-Token-Consumer
  (lambda [/dev/rncin ch scope]
    (cond [(char-whitespace? ch)
           (xml-skip-whitespace /dev/rncin)
           (values xml-collapsed-whitespace rnc-consume-token:* scope)]
          [(xml-name-char? ch)
           (let ([kw (xml-consume-nmtoken /dev/rncin ch)])
             (values kw rnc-consume-token:* kw))]
          [else (case ch
                  [(#\<) (xml-consume-open-token /dev/rncin rnc-consume-token:* scope)]
                  [(#\>) (values ch rnc-consume-token:* scope)]
                  [(#\= #\( #\) #\[) (values ch rnc-consume-token:* scope)]
                  [(#\' #\") (values (rnc-consume-string /dev/rncin ch) rnc-consume-token:* scope)]
                  [(#\? #\/ #\]) (xml-consume-close-token /dev/rncin ch rnc-consume-token:* scope)]
                  [(#\& #\%) (values (xml-consume-reference-token /dev/rncin ch) rnc-consume-token:* scope)]
                  [(#\#) (values (rnc-consume-comment /dev/rncin) rnc-consume-token:* scope)]
                  [else (values ch rnc-consume-token:* scope)])])))

(define rnc-consume-string : (-> Input-Port Char (U String XML-Error))
  ;;; https://relaxng.org/compact-20021121.html#syntax
  (lambda [/dev/rncin quote]
    (define head1 : (U Char EOF) (peek-char /dev/rncin 0))
    (define head2 : (U Char EOF False) (and (eq? head1 quote) (peek-char /dev/rncin 1)))
    
    (if (eq? head2 quote)
        (xml-consume-cdata+tail /dev/rncin quote quote quote 2 #true)
        
        (let consume-single-line-string ([srahc : (Listof Char) null])
          (define ch : (U EOF Char) (read-char /dev/rncin))
          (cond [(eq? ch quote) (list->string (reverse srahc))]
                [(eof-object? ch) (cons (reverse srahc) !eof)]
                [(not (xml-newline-char? ch)) (consume-single-line-string (cons ch srahc))]
                [else (cons (xml-consume-chars-literal /dev/rncin quote (cons ch srahc)) !char)])))))

(define rnc-consume-comment : (-> Input-Port (U XML-Comment XML-Error))
  ;;; https://relaxng.org/compact-20021121.html#d0e171
  ;;; https://relaxng.org/compact-20021121.html#d0e339
  (lambda [/dev/xmlin]
    (define body : (U String EOF) (read-line /dev/xmlin))

    ; check documentations when dealing with comments
    (xml-comment (if (string? body) body ""))))

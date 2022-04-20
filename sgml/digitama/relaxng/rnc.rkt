#lang typed/racket/base

;;; https://relaxng.org/compact-20021121.html
;;; NOTE: performance is *not* the goal in this module,
;;    partially because of the pipeline of reading RNC chars

(provide (all-defined-out))

(require racket/unsafe/ops)

(require digimon/enumeration)
(require digimon/stdio)

(require "../digicore.rkt")
(require "../tokenizer.rkt")

(require "../tokenizer/port.rkt")
(require "../tokenizer/errno.rkt")
(require "../tokenizer/characters.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type RNC-Token-Consumer (-> Input-Port Char XML-Scope (Values (U XML-Datum EOF) XML-Scope)))

(define-enumeration rnc-keyword : RNC-Keyword
  [attribute default datatypes div element empty external grammar include inherit
             list mixed namespace notAllowed parent start string text token])

(define default-rnc-error-literal : (Parameterof Char) (make-parameter #\uFFFD))

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
    ; `xml-parser-env-consume` is useless for RNC
    (define prev-env : XML-Parser-ENV
      (cond [(xml-parser-env? env) env]
            [else (let-values ([(line column position) (syn-token-port-location /dev/rncin)])
                    (cond [(not env) (xml-parser-env xml-consume-token:* xml-default-scope line column position)]
                          [else (xml-parser-env (car env) (cdr env) line column position)]))]))
    (define ch : (U Char XML-Error EOF) (read-rnc-char /dev/rncin))
    (define-values (datum next-scope)
      (cond [(not (char? ch)) (values ch (xml-parser-env-scope prev-env))]
            [else (rnc-consume-token:* /dev/rncin ch (xml-parser-env-scope prev-env))]))
    (define-values (line column end) (syn-token-port-location /dev/rncin))
    (define env++ : XML-Parser-ENV
      (xml-parser-env xml-consume-token:*
                      (and next-scope xml-default-scope) ; tell the lexer that meanwhile the `scope` is useless for RNC
                      line column end))

    (values (xml-datum->token source prev-env end datum) env++)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define rnc-consume-token:* : RNC-Token-Consumer
  (lambda [/dev/rncin ch scope]
    (cond [(char-whitespace? ch)
           (xml-skip-whitespace /dev/rncin)
           (values xml-collapsed-whitespace scope)]
          [(xml-name-char? ch)
           (let-values ([(id scope++) (rnc-consume-identify-or-keyword /dev/rncin ch)])
             (values id scope++))]
          [else (case ch
                  [(#\' #\") (values (rnc-consume-string /dev/rncin ch) scope)]
                  [(#\#) (values (rnc-consume-comment /dev/rncin) scope)]
                  [(#\\) (values (rnc-consume-identify-or-keyword /dev/rncin) scope)]
                  [(#\| #\&) (values (rnc-consume-assign-method /dev/rncin ch) scope)]
                  [else (values ch scope)])])))

(define rnc-consume-identify-or-keyword : (case-> [Input-Port -> (U Symbol Char)]
                                                  [Input-Port Char -> (Values (U Symbol Keyword) Symbol)])
  ;;; https://relaxng.org/compact-20021121.html#nt-identifierOrKeyword
  (case-lambda
    [(/dev/rncin) ; for '\'identify
     (let-values ([(leader span) (peek-rnc-char /dev/rncin)])
       (cond [(or (not (char? leader)) (not (xml-name-char? leader))) #\\]
             [else (read-bytes span /dev/rncin) (string->symbol (rnc-consume-namechars /dev/rncin leader))]))]
    [(/dev/rncin ch)
     (let ([raw (rnc-consume-namechars /dev/rncin ch)])
       (define id : Symbol (string->symbol raw))

       (values (if (rnc-keyword? id) (string->keyword raw) id)
               id #| the `scope` info is actually useless for RNC |#))]))

(define rnc-consume-string : (-> Input-Port Char (U String XML-Error))
  ;;; https://relaxng.org/compact-20021121.html#syntax
  (lambda [/dev/rncin quote-char]
    (define-values (head1 head2-offset) (peek-rnc-char /dev/rncin 0))
    (define-values (head2 offset) (if (eq? head1 quote-char) (peek-rnc-char /dev/rncin head2-offset) (values #false head2-offset)))
    
    (if (eq? head2 quote-char)
        (rnc-consume-multi-line-literal /dev/rncin quote-char offset #true)
        (rnc-consume-single-line-literal /dev/rncin quote-char null))))

(define rnc-consume-comment : (-> Input-Port (U XML-Comment XML-Error))
  ;;; https://relaxng.org/compact-20021121.html#d0e171
  ;;; https://relaxng.org/compact-20021121.html#d0e339
  (lambda [/dev/xmlin]
    (define body : (U String EOF) (read-line /dev/xmlin 'any))

    ; check documentations when dealing with comments
    (xml-comment (if (string? body) body ""))))

(define rnc-consume-assign-method : (-> Input-Port Char (U Symbol Char))
  ;;; https://relaxng.org/compact-20021121.html#syntax
  (lambda [/dev/rncin assign]
    (define-values (eq size) (peek-rnc-char /dev/rncin 0))
    (define compound? : Boolean (eq? eq #\=))

    (when (and compound?)
      (drop-string /dev/rncin size))

    (cond [(not (eq? assign #\|)) assign]
          [(not compound?) assign]
          [else #\λ #| `#\|` itself is a valid char |#])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define rnc-consume-namechars : (-> Input-Port Char String)
  (lambda [/dev/rncin leader]
    (let consume-name ([srahc : (Listof Char) (list leader)]
                       [skip : Nonnegative-Fixnum 0])
      (define-values (ch skip++) (peek-rnc-char /dev/rncin skip))
      (cond [(or (eof-object? ch) (pair? ch)) (read-bytes skip /dev/rncin) (list->string (reverse srahc))]
            [(xml-name-char? ch) (consume-name (cons ch srahc) skip++)]
            [else (read-bytes skip /dev/rncin) (list->string (reverse srahc))]))))

(define rnc-consume-single-line-literal : (-> Input-Port Char (Listof Char) (U String XML-Error))
  (lambda [/dev/rncin quote-char chars]
    (let consume-literal ([srahc : (Listof Char) chars])
      (define ch : (U EOF Char XML-Error) (read-rnc-char /dev/rncin))
      (cond [(char? ch)
             (cond [(eq? ch #\newline) (cons (xml-consume-chars-literal /dev/rncin quote-char (cons ch srahc)) !char)]
                   [(not (eq? ch quote-char)) (consume-literal (cons ch srahc))]
                   [else (list->string (reverse srahc))])]
            [(pair? ch) (consume-literal (cons (default-rnc-error-literal) srahc))]
            [else (list->string (reverse srahc))]))))

(define rnc-consume-multi-line-literal : (-> Input-Port Char Nonnegative-Fixnum Boolean String)
  (lambda [/dev/rncin quote-char skip contain-tail?]
    ;; NOTE: The CDATA might be large, in which case `cons`ing every chars would be inefficient
    (define /dev/cdout : Output-Port (open-output-bytes '/dev/cdout))
    
    (let consume-cdata ([offset : Nonnegative-Fixnum skip])
      (define-values (ch offset++) (peek-rnc-char /dev/rncin offset))
      
      (cond [(char? ch)
             (cond [(not (eq? ch quote-char)) (write-char ch /dev/cdout) (consume-cdata offset++)]
                   [else (let*-values ([(ach offset++++) (peek-rnc-char /dev/rncin offset++)])
                           (cond [(not (eq? ach quote-char)) (consume-cdata offset++)]
                                 [else (let-values ([(aach offset++++++) (peek-rnc-char /dev/rncin offset++++)])
                                         (cond [(eq? aach quote-char) (read-string (+ offset (if (not contain-tail?) 0 3)) /dev/rncin)]
                                               [else (consume-cdata offset++)]))]))])]
            [(pair? ch) (write-char (default-rnc-error-literal) /dev/cdout) (consume-cdata offset++)]
            [else (read-string offset /dev/rncin)]))
    
    (get-output-string /dev/cdout)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define read-rnc-char : (-> Input-Port (U Char XML-Error EOF))
  ;;; https://relaxng.org/compact-20021121.html#d0e3920
  ;;; https://relaxng.org/compact-20021121.html#d0e3937
  (lambda [/dev/rncin]
    (define ch : (U Char EOF) (read-char /dev/rncin))

    (cond [(eof-object? ch) eof]
          [(eq? ch #\return)
           (when (eq? (peek-char /dev/rncin) #\newline)
             (read-char /dev/rncin))
           #\newline]
          [(eq? ch #\\)
           (let read-escape-sequence ([offset : Nonnegative-Fixnum 0])
             (define x : (U Char EOF) (peek-char /dev/rncin offset))
             (cond [(eq? x #\x) (read-escape-sequence (unsafe-fx+ offset 1))]
                   [(and (> offset 0) (eq? x #\{))
                    (let ([xs (read-string (unsafe-fx+ offset 1) /dev/rncin)])
                      (let read-escape-char ([srahc : (Listof Char) (list x #| #\{ |#)]
                                             [code : Fixnum 0])
                        (define digit : (U Char EOF) (read-char /dev/rncin))
                        (cond [(eof-object? digit) (cons (reverse srahc) !eof)]
                              [(char-hexdigit? digit) (read-escape-char (cons digit srahc) (unsafe-fx+ (unsafe-fx* code 16) (char->hexadecimal digit)))]
                              [(eq? digit #\})
                               (let ([rnc-char (natural->char-entity code)])
                                 (cond [(index? rnc-char) (integer->char rnc-char)]
                                       [else (cons (reverse (cons digit srahc)) !char)]))]
                              [else ; read error of the entire escape sequence
                               (let read-literal ([srahc : (Listof Char) (cons digit srahc)])
                                 (define ech : (U EOF Char) (read-char /dev/rncin))
                                 (cond [(eof-object? ech) (cons (reverse srahc) !eof)]
                                       [(eq? ech #\}) (values (cons (reverse (cons ech srahc)) !char))]
                                       [else (read-literal (cons ech srahc))]))])))]
                   [else ch]))]
          [else ch])))

(define peek-rnc-char : (->* (Input-Port) (Nonnegative-Fixnum) (Values (U Char XML-Error EOF) Nonnegative-Fixnum))
  ;;; https://relaxng.org/compact-20021121.html#d0e3920
  ;;; https://relaxng.org/compact-20021121.html#d0e3937
  (lambda [/dev/rncin [skip 0]]
    (define ch : (U Char EOF) (peek-char /dev/rncin skip))
    (define skip+1 : Nonnegative-Fixnum (unsafe-fx+ skip 1))

    (cond [(eof-object? ch) (values eof skip)]
          [(eq? ch #\return)
           (values #\newline
                   (if (eq? (peek-char /dev/rncin skip+1) #\newline)
                       (unsafe-fx+ skip+1 1) skip+1))]
          [(eq? ch #\\)
           (let ([x0-skip : Nonnegative-Fixnum (unsafe-fx+ skip 1)])
             (let peek-escape-sequence ([offset : Nonnegative-Fixnum x0-skip])
               (define x : (U Char EOF) (peek-char /dev/rncin offset))
               (cond [(eq? x #\x) (peek-escape-sequence (unsafe-fx+ offset 1))]
                     [(and (> offset x0-skip) (eq? x #\{))
                      (let read-escape-char ([srahc : (Listof Char) null]
                                             [code : Fixnum 0]
                                             [hex-skip : Nonnegative-Fixnum (unsafe-fx+ offset 1)])
                        (define digit : (U Char EOF) (peek-char /dev/rncin hex-skip))
                        (define skip++ : Nonnegative-Fixnum (unsafe-fx+ hex-skip 1))
                        (cond [(eof-object? digit) (values (cons (reverse srahc) !eof) hex-skip)]
                              [(char-hexdigit? digit) (read-escape-char (cons digit srahc) (unsafe-fx+ (unsafe-fx* code 16) (char->hexadecimal digit)) skip++)]
                              [(eq? digit #\})
                               (let ([rnc-char (natural->char-entity code)])
                                 (values (cond [(index? rnc-char) (integer->char rnc-char)]
                                               [else (cons (reverse (cons digit srahc)) !char)])
                                         skip++))]
                              [else ; peek error of the entire escape sequence
                               (let peek-literal ([srahc : (Listof Char) (cons digit srahc)]
                                                  [e-skip : Nonnegative-Fixnum skip++])
                                 (define ech : (U EOF Char) (peek-char /dev/rncin e-skip))
                                 (cond [(eof-object? ech) (values (cons (reverse srahc) !eof) e-skip)]
                                       [(eq? ech #\}) (values (cons (reverse (cons ech srahc)) !char) (unsafe-fx+ e-skip 1))]
                                       [else (peek-literal (cons ech srahc) (unsafe-fx+ e-skip 1))]))]))]
                     [else (values ch x0-skip)])))]
          [else (values ch skip+1)])))

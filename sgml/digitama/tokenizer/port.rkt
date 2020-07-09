#lang typed/racket/base

;;; https://www.w3.org/TR/xml11/

(provide (all-defined-out))

(require "characters.rkt")
(require "../delimiter.rkt")

(require racket/unsafe/ops)

;;; Performance Hint:
;; 0. See schema/digitama/exchange/csv/reader/port.rkt
;; 1. Checking empty file before reading makes it oscillate(500ms for 2.1MB xslx), weird

(define-type XML-Error (Listof Char))
(define-type XML-Datum (U Char Symbol String Index Keyword XML-White-Space XML-Error))

(define-type XML-Scope XML-Datum)

(struct xml-white-space ([raw : String]) #:type-name XML-White-Space)
(struct xml-comment xml-white-space () #:type-name XML-Comment)

(define xml-empty-attributes : (Immutable-HashTable Symbol XML-Datum) (make-immutable-hasheq))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define read-xml-tokens : (-> Input-Port (Listof XML-Datum))
  (lambda [/dev/xmlin]
    (let read-xml ([snekot : (Listof XML-Datum) null]
                   [scope : XML-Scope 'TopLevel])
      (define-values (token this-scope) (xml-consume-token /dev/xmlin scope))
      (cond [(eof-object? token) (reverse snekot)]
            [else (read-xml (cons token snekot) this-scope)]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml-consume-token : (->* (Input-Port) (XML-Scope) (Values (U XML-Datum EOF) XML-Scope))
  ;;; https://www.w3.org/TR/xml11/#sec-common-syn
  (lambda [/dev/xmlin [scope 'TopLevel]]
    (define ch : (U Char EOF) (read-char /dev/xmlin))
    (cond [(eof-object? ch) (values eof scope)]
          [(char-whitespace? ch) (values (xml-consume-whitespace /dev/xmlin ch scope) scope)]
          [(xml-name-start-char? ch) (xml-consume-nmtoken /dev/xmlin ch scope)]
          [else (case ch
                  [(#\<) (xml-consume-open-token /dev/xmlin scope)]
                  [(#\> #\= #\( #\) #\[) (values ch ch)]
                  [(#\' #\") (values (xml-consume-literal-token /dev/xmlin ch scope) scope)]
                  [(#\? #\/ #\]) (xml-consume-close-token /dev/xmlin ch scope)]
                  [(#\& #\%) (values (xml-consume-reference-token /dev/xmlin ch) scope)]
                  [else (values ch scope)])])))

#;(define xml-consume-token/skip-whitespace : (->* (Input-Port (U Char EOF)) (XML-Literal) (Values XML-Datum (U Char EOF)))
  ;;; https://www.w3.org/TR/xml11/#sec-common-syn
  (lambda [/dev/xmlin leading-char [literals 'Attribute]]
    (define-values (token maybe-char) (xml-consume-token /dev/xmlin leading-char literals))

    (cond [(xml-white-space? token) (xml-consume-token /dev/xmlin maybe-char literals)]
          [else (values token maybe-char)])))

(define xml-consume-open-token : (-> Input-Port XML-Scope (Values (U Char Symbol XML-Comment XML-Error) XML-Scope))
  (lambda [/dev/xmlin scope]
    (define maybe-ch : (U Char EOF) (peek-char /dev/xmlin 0))
    (cond [(eq? maybe-ch #\!)
           (let ([dispatcher (peek-char /dev/xmlin 1)])
             (cond [(eq? dispatcher #\[)
                    (cond [(equal? (peek-string 6 2 /dev/xmlin) "CDATA[") (read-string 9 /dev/xmlin) (values <!$CDATA$ scope)]
                          [else (read-string 2 /dev/xmlin) (values <!$ '|<![|)])]
                   [(and (eq? dispatcher #\-) (eq? (peek-char /dev/xmlin 2) #\-))
                    (read-string 3 /dev/xmlin) (values (xml-consum-comment-tail /dev/xmlin) scope)]
                   [else (read-char /dev/xmlin) (values <! <!)]))]
          [(eq? maybe-ch #\?) (read-char /dev/xmlin) (values <? <?)]
          [(eq? maybe-ch #\/) (read-char /dev/xmlin) (values </ </)]
          [else (values #\< #\<)])))

(define xml-consume-close-token : (-> Input-Port Char XML-Scope (Values (U Symbol Char XML-Error) XML-Scope))
  (lambda [/dev/xmlin leading-char scope]
    (define maybe-char : (U Char EOF) (peek-char /dev/xmlin 0))

    (if (eq? leading-char #\])
        (cond [(not (eq? maybe-char #\])) (values leading-char leading-char)]
              [else (let ([maybe-> (peek-char /dev/xmlin 1)])
                      (cond [(not (eq? maybe-> #\>)) (values leading-char leading-char)]
                            [else (read-string 2 /dev/xmlin) (values $$> $$>)]))])
        (cond [(not (eq? maybe-char #\>)) (values leading-char scope)]
              [(eq? leading-char #\?) (read-char /dev/xmlin) (values ?> ?>)]
              [else (read-char /dev/xmlin) (values /> />)]))))
  
(define xml-consume-nmtoken : (-> Input-Port Char XML-Scope (Values Symbol XML-Scope))
  ;;; https://www.w3.org/TR/xml11/#sec-common-syn
  ;;; https://www.w3.org/TR/xml11/#NT-Names
  ;;; https://www.w3.org/TR/xml11/#NT-Nmtokens
  (lambda [/dev/xmlin leader scope]
    (values (string->symbol (xml-consume-namechars /dev/xmlin leader))
            scope)))

(define xml-consume-reference-token : (-> Input-Port Char (U Symbol Index XML-Error))
  ;;; https://www.w3.org/TR/xml11/#sec-references
  (lambda [/dev/xmlin leader]
    (define entity-leader : (U Char EOF) (read-char /dev/xmlin))
    (cond [(eof-object? entity-leader) (list leader)]
          [(eq? leader #\%) (xml-consume-entity-reference /dev/xmlin leader entity-leader)]
          [(eq? entity-leader #\;) (list leader entity-leader)]
          [(not (eq? entity-leader #\#)) (xml-consume-entity-reference /dev/xmlin leader entity-leader)]
          [else (let ([char-leader (read-char /dev/xmlin)])
                  (cond [(eof-object? char-leader) (list leader entity-leader)]
                        [(eq? char-leader #\x) (xml-consume-char-reference /dev/xmlin #false)]
                        [else (xml-consume-char-reference /dev/xmlin char-leader)]))])))

(define xml-consume-literal-token : (-> Input-Port Char XML-Scope (U String XML-Error))
  ;;; https://www.w3.org/TR/xml11/#NT-SystemLiteral
  (lambda [/dev/xmlin quote type]
    (case type
      [(Attribute) (xml-consume-attribute-value /dev/xmlin quote)]
      [(Entity) (xml-consume-entity-value /dev/xmlin quote)]
      [(Public) (xml-consume-pubid-literal /dev/xmlin quote)]
      [else (xml-consume-system-literal /dev/xmlin quote)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml-consume-whitespace : (-> Input-Port Char XML-Scope XML-White-Space)
  ;;; https://www.w3.org/TR/xml11/#NT-S
  ;;; https://www.w3.org/TR/xml11/#sec-line-ends
  (lambda [/dev/xmlin leader scope]
    ;; NOTE: The clients take responsibility for normalizing the End-of-Lines if required
    (let read-whitespace ([span : Nonnegative-Fixnum 0])
      (define maybe-space : (U Char EOF) (peek-char /dev/xmlin span))
      (cond [(eof-object? maybe-space) (xml-white-space (xml-read-string /dev/xmlin span leader))]
            [(char-whitespace? maybe-space) (read-whitespace (unsafe-fx+ span 1))]
            [else (xml-white-space (xml-read-string /dev/xmlin span leader))]))))

(define xml-consum-comment-tail : (-> Input-Port (U XML-Comment XML-Error))
  ;;; https://www.w3.org/TR/xml11/#sec-comments
  (lambda [/dev/xmlin]
    (let read-comment ([srahc : (Listof Char) null]
                       [malformed? : Boolean #false])
      (define maybe-char : (U Char EOF) (read-char /dev/xmlin))
      (cond [(eof-object? maybe-char) (list* #\< #\! #\- #\- (reverse srahc))]
            [(not (eq? maybe-char #\-)) (read-comment (cons maybe-char srahc) malformed?)]
            [else (let ([maybe-- (peek-char /dev/xmlin 0)]
                        [maybe-> (peek-char /dev/xmlin 1)])
                    (define -? : Boolean (eq? maybe-- #\-))
                    (define >? : Boolean (eq? maybe-> #\>))
                    (cond [(and -? >?)
                           (read-string 2 /dev/xmlin)
                           (cond [(not malformed?) (xml-comment (list->string (reverse srahc)))]
                                 [else (list*  #\< #\! #\- #\- (reverse (list* #\> #\- #\- srahc)))])]
                          [else (read-comment (cons maybe-char srahc) (or malformed? -? (null? srahc)))]))]))))
  
(define xml-consume-namechars : (-> Input-Port Char String)
  ;;; https://www.w3.org/TR/xml11/#NT-NameChar
  (lambda [/dev/xmlin leader]
    (let consume-name ([span : Nonnegative-Fixnum 0])
      (define ch : (U EOF Char) (peek-char /dev/xmlin span))
      (cond [(eof-object? ch) (xml-read-string /dev/xmlin span leader)]
            [(xml-name-char? ch) (consume-name (unsafe-fx+ span 1))]
            [else (xml-read-string /dev/xmlin span leader)]))))

(define xml-consume-char-reference : (-> Input-Port (Option Char) (U Index XML-Error))
  ;;; https://www.w3.org/TR/xml11/#NT-CharRef
  (lambda [/dev/xmlin maybe-leader]
    (define-values (base char-digit?) (if (not maybe-leader) (values 16 char-hexdigit?) (values 10 char-numeric?)))
    
    (let read-char-reference ([ch : (U EOF Char) (or maybe-leader (read-char /dev/xmlin))]
                              [srahc : (Listof Char) (if maybe-leader (list #\# #\&) (list #\x #\# #\&))]
                              [code : Fixnum 0])
      (cond [(eof-object? ch) (reverse srahc)]
            [(char-digit? ch) (read-char-reference (read-char /dev/xmlin) (cons ch srahc) (unsafe-fx+ (unsafe-fx* code base) (char->hexadecimal ch)))]
            [(eq? ch #\;) (natural->char-entity code)]
            [else (xml-consume-error-literal /dev/xmlin #\; (cons ch srahc))]))))

(define xml-consume-entity-reference : (-> Input-Port Char Char (U Symbol XML-Error))
  ;;; https://www.w3.org/TR/xml11/#NT-EntityRef
  (lambda [/dev/xmlin type leader]
    (define %? : Boolean (eq? type #\%))
    (cond [(not (xml-name-start-char? leader)) (xml-consume-error-literal /dev/xmlin #\; (list leader type))]
          [else (let read-entity-reference ([srahc : (Listof Char) (if (not %?) (list leader) (list leader type))])
                  (define ch : (U EOF Char) (read-char /dev/xmlin))
                  (cond [(eof-object? ch) (reverse srahc)]
                        [(xml-name-char? ch) (read-entity-reference (cons ch srahc))]
                        [(eq? ch #\;) (string->unreadable-symbol (list->string (reverse srahc)))]
                        [else (xml-consume-error-literal /dev/xmlin #\; (cons ch srahc))]))])))

(define xml-consume-entity-value : (-> Input-Port Char (U String XML-Error))
  ;;; https://www.w3.org/TR/xml11/#NT-EntityValue
  (lambda [/dev/xmlin quote]
    (let consume-literal ([srahc : (Listof Char) null])
      (define ch : (U EOF Char) (read-char /dev/xmlin))
      (cond [(eq? ch quote) (list->string (reverse srahc))]
            [(eof-object? ch) (reverse srahc)]
            [else (consume-literal (cons ch srahc))]))))

(define xml-consume-attribute-value : (-> Input-Port Char (U String XML-Error))
  ;;; https://www.w3.org/TR/xml11/#NT-AttValue
  (lambda [/dev/xmlin quote]
    (let consume-literal ([srahc : (Listof Char) null])
      (define ch : (U EOF Char) (read-char /dev/xmlin))
      (cond [(eq? ch quote) (list->string (reverse srahc))]
            [(eof-object? ch) (reverse srahc)]
            [else (consume-literal (cons ch srahc))]))))

(define xml-consume-system-literal : (-> Input-Port Char (U String XML-Error))
  ;;; https://www.w3.org/TR/xml11/#NT-SystemLiteral
  (lambda [/dev/xmlin quote]
    (let consume-literal ([srahc : (Listof Char) null])
      (define ch : (U EOF Char) (read-char /dev/xmlin))
      (cond [(eq? ch quote) (list->string (reverse srahc))]
            [(eof-object? ch) (reverse srahc)]
            [else (consume-literal (cons ch srahc))]))))

(define xml-consume-pubid-literal : (-> Input-Port Char (U String XML-Error))
  ;;; https://www.w3.org/TR/xml11/#NT-PubidLiteral
  (lambda [/dev/xmlin quote]
    (let consume-literal ([srahc : (Listof Char) null])
      (define ch : (U EOF Char) (read-char /dev/xmlin))
      (cond [(eq? ch quote) (list->string (reverse srahc))]
            [(eof-object? ch) (reverse srahc)]
            [(xml-pubid-char? ch) (consume-literal (cons ch srahc))]
            [else (xml-consume-error-literal /dev/xmlin quote (cons ch srahc))]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml-consume-error-literal : (-> Input-Port Char (Listof Char) XML-Error)
  (lambda [/dev/xmlin quote chars]
    (let consume-literal ([srahc : (Listof Char) chars])
      (define ch : (U EOF Char) (read-char /dev/xmlin))
      (cond [(or (eq? ch quote) (eof-object? ch)) (reverse srahc)]
            [else (consume-literal (cons ch srahc))]))))

(define xml-read-string : (-> Input-Port Natural Char String)
  (lambda [/dev/xmlin tailsize leader]
    (define total : Nonnegative-Fixnum (unsafe-fx+ tailsize 1))
    (define s : String (make-string total leader))

    (read-string! s /dev/xmlin 1 total)
    s))

#lang typed/racket/base

;;; https://www.w3.org/TR/xml11/

(provide (all-defined-out))

(require "characters.rkt")
(require "../delimiter.rkt")

(require racket/unsafe/ops)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Performance Hint:
;; 0. See schema/digitama/exchange/csv/reader/port.rkt
;; 1. Checking empty file before reading makes it oscillate(500ms for 2.1MB xslx), weird

(define-type XML-Error (Listof Char))
(define-type XML-Datum (U Char Symbol String Index Keyword XML-White-Space XML-Error))

(define-type XML-Scope (U Symbol Fixnum))
(define-type XML-Token-Consumer (Rec λ (-> Input-Port Char XML-Scope (Values (U XML-Datum EOF) λ XML-Scope))))

(struct xml-white-space ([raw : String]) #:type-name XML-White-Space)
(struct xml-comment xml-white-space () #:type-name XML-Comment)

(define xml-empty-attributes : (Immutable-HashTable Symbol XML-Datum) (make-immutable-hasheq))

(define xml-initial-scope : XML-Scope 'initial)
(define xml-default-scope : XML-Scope 'xml)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define read-xml-tokens : (-> Input-Port (Listof XML-Datum))
  (lambda [/dev/xmlin]
    (let read-xml ([snekot : (Listof XML-Datum) null]
                   [consume : XML-Token-Consumer xml-consume-token:*]
                   [scope : XML-Scope xml-initial-scope])
      (define-values (token this-consume this-scope) (xml-consume-token /dev/xmlin consume scope))
      (cond [(eof-object? token) (reverse snekot)]
            [else (read-xml (cons token snekot) this-consume this-scope)]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml-consume-token : (-> Input-Port XML-Token-Consumer XML-Scope (Values (U XML-Datum EOF) XML-Token-Consumer XML-Scope))
  ;;; https://www.w3.org/TR/xml11/#sec-common-syn
  (lambda [/dev/xmlin consume scope]
    (define ch : (U Char EOF) (read-char /dev/xmlin))
    
    (cond [(eof-object? ch) (values eof consume scope)]
          [else (consume /dev/xmlin ch scope)])))

(define xml-consume-token:* : XML-Token-Consumer
  (lambda [/dev/xmlin ch scope]
    (cond [(char-whitespace? ch) (values (xml-consume-whitespace /dev/xmlin ch) xml-consume-token:* scope)]
          [else (if (index? scope)
                    (case ch
                      [(#\<) (xml-consume-open-token /dev/xmlin xml-consume-token:* scope)]
                      [(#\& #\%) (values (xml-consume-reference-token /dev/xmlin ch) xml-consume-token:* scope)]
                      [(#\? #\]) (xml-consume-close-token /dev/xmlin ch xml-consume-token:* scope)] ; for PI and CDATA
                      [else (values (xml-consume-contentchars /dev/xmlin ch) xml-consume-token:* scope)])
                    (cond [(xml-name-start-char? ch)
                           (let ([kw (xml-consume-nmtoken /dev/xmlin ch)])
                             (case kw
                               [(PUBLIC) (values kw xml-consume-token:* kw)]
                               [(SYSTEM) (values kw xml-consume-token:* kw)]
                               [else (values kw xml-consume-token:* scope)]))]
                          [else (case ch
                                  [(#\<) (xml-consume-open-token /dev/xmlin xml-consume-token:* scope)]
                                  [(#\>) (values ch xml-consume-token:* scope)]
                                  [(#\= #\( #\) #\[) (values ch xml-consume-token:* scope)]
                                  [(#\' #\") (xml-consume-token:start-decl-string /dev/xmlin ch scope)]
                                  [(#\? #\/ #\]) (xml-consume-close-token /dev/xmlin ch xml-consume-token:* scope)]
                                  [(#\& #\%) (values (xml-consume-reference-token /dev/xmlin ch) xml-consume-token:* scope)]
                                  [else (values ch xml-consume-token:* scope)])]))])))

(define xml-consume-token:start-decl-name : XML-Token-Consumer
  (lambda [/dev/xmlin ch scope]
    (cond [(not (xml-name-start-char? ch)) (values (xml-consume-chars-literal/within-tag /dev/xmlin (list ch)) xml-consume-token:* scope)]
          [else (values (xml-consume-nmtoken /dev/xmlin ch) xml-consume-token:* scope)])))

(define xml-consume-token:start-decl-string : XML-Token-Consumer
  ;;; https://www.w3.org/TR/xml11/#NT-SystemLiteral
  (lambda [/dev/xmlin ch scope]
    (case scope
      [(Entity) (values (xml-consume-entity-value /dev/xmlin ch) xml-consume-token:* scope)]
      [(PUBLIC) (values (xml-consume-pubid-literal /dev/xmlin ch) xml-consume-token:* 'SYSTEM)]
      [else (values (xml-consume-system-literal /dev/xmlin ch) xml-consume-token:* xml-default-scope)])))

(define xml-consume-token:start-tag-name : XML-Token-Consumer
  ;;; https://www.w3.org/TR/xml11/#NT-STag
  (lambda [/dev/xmlin ch scope]
    (values (if (xml-name-start-char? ch)
                (xml-consume-nmtoken /dev/xmlin ch)
                (xml-consume-chars-literal/within-tag /dev/xmlin (list ch)))
            xml-consume-token:tag-attr-name scope)))

(define xml-consume-token:end-tag-name : XML-Token-Consumer
  ;;; https://www.w3.org/TR/xml11/#NT-ETag
  (lambda [/dev/xmlin ch scope]
    (values (if (xml-name-start-char? ch)
                (xml-consume-nmtoken /dev/xmlin ch)
                (xml-consume-chars-literal/within-tag /dev/xmlin (list ch)))
            xml-consume-token:tag-end scope)))

(define xml-consume-token:tag-attr-name : XML-Token-Consumer
  ;;; https://www.w3.org/TR/xml11/#NT-Attribute
  (lambda [/dev/xmlin ch scope]
    (cond [(char-whitespace? ch) (xml-consume-token:tag-attr-name /dev/xmlin (xml-skip-whitespace /dev/xmlin #\!) scope)]
          [(xml-name-start-char? ch) (values (xml-consume-nmtoken /dev/xmlin ch) xml-consume-token:tag-attr-eq scope)]
          [(eq? ch #\>) (values stag> xml-consume-token:* scope)] ; non-empty (start) tag does not have a close delimiter.
          [(eq? ch #\/)
           (let ([nch (read-char /dev/xmlin)])
             (cond [(eq? nch #\>) (values /> xml-consume-token:* (xml-doc-scope-- scope))]
                   [else (values (xml-consume-chars-literal/within-tag /dev/xmlin (if (eof-object? nch) (list ch) (list nch ch)))
                                 xml-consume-token:* scope)]))]
          [(eq? ch #\?)
           (let ([nch (read-char /dev/xmlin)])
             (cond [(eq? nch #\>) (values ?> xml-consume-token:* (if (eq? scope xml-initial-scope) xml-default-scope scope))]
                   [else (values (xml-consume-chars-literal/within-tag /dev/xmlin (if (eof-object? nch) (list ch) (list nch ch)))
                                 xml-consume-token:* scope)]))]
          [else (values (xml-consume-chars-literal/within-tag /dev/xmlin (list ch)) xml-consume-token:tag-attr-name scope)])))

(define xml-consume-token:tag-attr-eq : XML-Token-Consumer
  ;;; https://www.w3.org/TR/xml11/#NT-Attribute
  (lambda [/dev/xmlin ch scope]
    (cond [(eq? ch #\=) (values ch xml-consume-token:tag-attr-value scope)]
          [else (values (xml-consume-chars-literal/within-tag /dev/xmlin (list ch)) xml-consume-token:tag-attr-name scope)])))

(define xml-consume-token:tag-attr-value : XML-Token-Consumer
  ;;; https://www.w3.org/TR/xml11/#NT-Attribute
  (lambda [/dev/xmlin ch scope]
    (values (if (or (eq? ch #\') (eq? ch #\"))
                (xml-consume-attribute-value /dev/xmlin ch)
                (xml-consume-chars-literal/within-tag /dev/xmlin (list ch)))
            xml-consume-token:tag-attr-name scope)))

(define xml-consume-token:tag-end : XML-Token-Consumer
  (lambda [/dev/xmlin ch scope]
    (cond [(char-whitespace? ch) (xml-consume-token:tag-end /dev/xmlin (xml-skip-whitespace /dev/xmlin #\!) scope)]
          [(eq? ch #\>) (values ch xml-consume-token:* (xml-doc-scope-- scope))]
          [else (values (xml-consume-chars-literal/exclusive /dev/xmlin #\> (list ch)) xml-consume-token:* scope)])))

(define xml-consume-token:pi-target : XML-Token-Consumer
  ;;; https://www.w3.org/TR/xml11/#sec-pi
  (lambda [/dev/xmlin ch scope]
    (cond [(not (xml-name-start-char? ch)) (values (xml-consume-chars-literal/exclusive /dev/xmlin #\? (list ch) #\>) xml-consume-token:* scope)]
          [else (let ([target (xml-consume-namechars /dev/xmlin ch)])
                  ;;; DrRacket's color lexer parses the XMLDecl which should be eaten by the #lang reader
                  (cond [(not (string-ci=? target "xml")) (values (string->symbol target) xml-consume-token:pi-body scope)]
                        [(eq? scope xml-initial-scope) (values (string->symbol target) xml-consume-token:tag-attr-name scope)]
                        [else (values (xml-consume-chars-literal/exclusive /dev/xmlin #\? (reverse (string->list target)) #\>) xml-consume-token:* scope)]))])))

(define xml-consume-token:pi-body : XML-Token-Consumer
  ;;; https://www.w3.org/TR/xml11/#sec-pi
  (lambda [/dev/xmlin ch scope]
    (cond [(not (char-whitespace? ch)) (values (xml-consume-chars-literal/exclusive /dev/xmlin #\? (list ch) #\>) xml-consume-token:* scope)]
          [else (values (list->string (xml-consume-chars-literal/exclusive /dev/xmlin #\? null #\>)) xml-consume-token:* scope)])))

(define xml-consume-token:cdata : XML-Token-Consumer
  ;;; https://www.w3.org/TR/xml11/#sec-cdata-sect
  (lambda [/dev/xmlin ch scope]
    (values (list->string (xml-consume-chars-literal/exclusive /dev/xmlin #\] (list ch) #\] #\>))
            xml-consume-token:*
            scope)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml-consume-open-token : (-> Input-Port XML-Token-Consumer XML-Scope (Values (U Char Symbol XML-Comment XML-Error) XML-Token-Consumer XML-Scope))
  (lambda [/dev/xmlin consume scope]
    (define maybe-ch : (U Char EOF) (peek-char /dev/xmlin 0))
    (cond [(eq? maybe-ch #\!)
           (let ([dispatcher (peek-char /dev/xmlin 1)])
             (cond [(eq? dispatcher #\[)
                    (cond [(equal? (peek-string 6 2 /dev/xmlin) "CDATA[") (read-string 8 /dev/xmlin) (values <!$CDATA$ xml-consume-token:cdata scope)]
                          [else (read-string 2 /dev/xmlin) (values <!$ xml-consume-token:* scope)])]
                   [(and (eq? dispatcher #\-) (eq? (peek-char /dev/xmlin 2) #\-))
                    (read-string 3 /dev/xmlin) (values (xml-consume-comment-tail /dev/xmlin) consume scope)]
                   [else (read-char /dev/xmlin) (values <! xml-consume-token:start-decl-name scope)]))]
          [(eq? maybe-ch #\?) (read-char /dev/xmlin) (values <? xml-consume-token:pi-target scope)]
          [(eq? maybe-ch #\/) (read-char /dev/xmlin) (values </ xml-consume-token:end-tag-name scope)]
          [else (values #\< xml-consume-token:start-tag-name (xml-doc-scope++ scope))])))

(define xml-consume-close-token : (-> Input-Port Char XML-Token-Consumer XML-Scope (Values (U Symbol Char XML-Error) XML-Token-Consumer XML-Scope))
  (lambda [/dev/xmlin leading-char consume scope]
    (define maybe-char : (U Char EOF) (peek-char /dev/xmlin 0))

    (if (eq? leading-char #\])
        (cond [(not (eq? maybe-char #\])) (values leading-char xml-consume-token:* scope)]
              [else (let ([maybe-> (peek-char /dev/xmlin 1)])
                      (cond [(not (eq? maybe-> #\>)) (values leading-char xml-consume-token:* scope)]
                            [else (read-string 2 /dev/xmlin) (values $$> xml-consume-token:* scope)]))])
        (cond [(not (eq? maybe-char #\>)) (values leading-char consume scope)]
              [(eq? leading-char #\?) (read-char /dev/xmlin) (values ?> consume scope)]
              [else (read-char /dev/xmlin) (values /> xml-consume-token:* (xml-doc-scope-- scope))]))))
  
(define xml-consume-nmtoken : (-> Input-Port Char Symbol)
  ;;; https://www.w3.org/TR/xml11/#sec-common-syn
  ;;; https://www.w3.org/TR/xml11/#NT-Names
  ;;; https://www.w3.org/TR/xml11/#NT-Nmtokens
  (lambda [/dev/xmlin leader]
    (string->symbol (xml-consume-namechars /dev/xmlin leader))))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml-consume-whitespace : (-> Input-Port Char XML-White-Space)
  ;;; https://www.w3.org/TR/xml11/#NT-S
  ;;; https://www.w3.org/TR/xml11/#sec-line-ends
  (lambda [/dev/xmlin leader]
    ;; NOTE: The clients take responsibility for normalizing the End-of-Lines if required
    (let read-whitespace ([span : Nonnegative-Fixnum 0])
      (define maybe-space : (U Char EOF) (peek-char /dev/xmlin span))
      (cond [(eof-object? maybe-space) (xml-white-space (xml-read-string /dev/xmlin span leader))]
            [(char-whitespace? maybe-space) (read-whitespace (unsafe-fx+ span 1))]
            [else (xml-white-space (xml-read-string /dev/xmlin span leader))]))))

(define xml-skip-whitespace : (-> Input-Port Char Char)
  ;;; https://www.w3.org/TR/xml11/#NT-S
  ;;; https://www.w3.org/TR/xml11/#sec-line-ends
  (lambda [/dev/xmlin fallback]
    (regexp-match-positions #px"\\s*" /dev/xmlin)
    (define maybe-space : (U Char EOF) (read-char /dev/xmlin))
    (if (eof-object? maybe-space) fallback maybe-space)))

(define xml-consume-comment-tail : (-> Input-Port (U XML-Comment XML-Error))
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
    (let consume-name ([span : Nonnegative-Fixnum 0]
                       [skip : Nonnegative-Fixnum 0])
      (define ch : (U EOF Char) (peek-char /dev/xmlin skip))
      (cond [(eof-object? ch) (xml-read-string /dev/xmlin span leader)]
            [(xml-name-char? ch) (consume-name (unsafe-fx+ span 1) (unsafe-fx+ skip (char-utf-8-length ch)))]
            [else (xml-read-string /dev/xmlin span leader)]))))

(define xml-consume-contentchars : (-> Input-Port Char (U String XML-Error))
  ;;; https://www.w3.org/TR/xml11/#sec-starttags
  (lambda [/dev/xmlin leader]
    (let consume-content ([span : Nonnegative-Fixnum 0]
                          [skip : Nonnegative-Fixnum 0])
      (define ch : (U EOF Char) (peek-char /dev/xmlin skip))
      (cond [(eof-object? ch) (xml-read-string /dev/xmlin span leader)]
            [(xml-content-char? ch) (consume-content (unsafe-fx+ span 1) (unsafe-fx+ skip (char-utf-8-length ch)))]
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
            [else (xml-consume-chars-literal /dev/xmlin #\; (cons ch srahc))]))))

(define xml-consume-entity-reference : (-> Input-Port Char Char (U Symbol XML-Error))
  ;;; https://www.w3.org/TR/xml11/#NT-EntityRef
  (lambda [/dev/xmlin type leader]
    (define %? : Boolean (eq? type #\%))
    (cond [(not (xml-name-start-char? leader)) (xml-consume-chars-literal /dev/xmlin #\; (list leader type))]
          [else (let read-entity-reference ([srahc : (Listof Char) (if (not %?) (list leader) (list leader type))])
                  (define ch : (U EOF Char) (read-char /dev/xmlin))
                  (cond [(eof-object? ch) (reverse srahc)]
                        [(xml-name-char? ch) (read-entity-reference (cons ch srahc))]
                        [(eq? ch #\;) (string->unreadable-symbol (list->string (reverse srahc)))]
                        [else (xml-consume-chars-literal /dev/xmlin #\; (cons ch srahc))]))])))

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
            [else (xml-consume-chars-literal /dev/xmlin quote (cons ch srahc))]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml-consume-chars-literal : (->* (Input-Port Char (Listof Char)) ((Option Char) (Option Char)) XML-Error)
  (lambda [/dev/xmlin boundary chars [ahead-boundary #false] [aahead-boundary #false]]
    (let consume-literal ([srahc : (Listof Char) chars])
      (define ch : (U EOF Char) (read-char /dev/xmlin))
      (cond [(eof-object? ch) (reverse srahc)]
            [(not (eq? ch boundary)) (consume-literal (cons ch srahc))]
            [(not ahead-boundary) (reverse srahc)]
            [else (let ([ach (read-char /dev/xmlin)])
                    (cond [(eof-object? ach) (reverse (cons ch srahc))]
                          [(not (eq? ach ahead-boundary)) (consume-literal (list* ach ch srahc))]
                          [(not aahead-boundary) (reverse srahc)]
                          [else (let ([aach (read-char /dev/xmlin)])
                                  (cond [(eof-object? aach) (reverse (list* ach ch srahc))]
                                        [(not (eq? aach aahead-boundary)) (consume-literal (list* aach ach ch srahc))]
                                        [else (reverse srahc)]))]))]))))

(define xml-consume-chars-literal/exclusive : (->* (Input-Port Char (Listof Char)) ((Option Char) (Option Char)) XML-Error)
  (lambda [/dev/xmlin boundary chars [ahead-boundary #false] [aahead-boundary #false]]
    (let consume-literal ([srahc : (Listof Char) chars])
      (define ch : (U EOF Char) (peek-char /dev/xmlin))
      (cond [(eof-object? ch) (reverse srahc)]
            [(not (eq? ch boundary)) (read-char /dev/xmlin) (consume-literal (cons ch srahc))]
            [(not ahead-boundary) (reverse srahc)]
            [else (let ([ach (peek-char /dev/xmlin 1)])
                    (cond [(eof-object? ach) (read-char /dev/xmlin) (reverse (cons ch srahc))]
                          [(not (eq? ach ahead-boundary)) (read-string 2 /dev/xmlin) (consume-literal (list* ach ch srahc))]
                          [(not aahead-boundary) (reverse srahc)]
                          [else (let ([aach (peek-char /dev/xmlin 2)])
                                  (cond [(eof-object? aach) (read-string 2 /dev/xmlin) (reverse (list* ach ch srahc))]
                                        [(not (eq? aach aahead-boundary)) (read-string 3 /dev/xmlin) (consume-literal (list* aach ach ch srahc))]
                                        [else (reverse srahc)]))]))]))))

(define xml-consume-chars-literal/within-tag : (-> Input-Port (Listof Char) XML-Error)
  (lambda [/dev/xmlin chars]
    (let consume-literal ([srahc : (Listof Char) chars])
      (define ch : (U EOF Char) (peek-char /dev/xmlin))
      (cond [(eof-object? ch) (reverse srahc)]
            [(eq? ch #\>) (reverse srahc)]
            [(not (eq? ch #\/)) (read-char /dev/xmlin) (consume-literal (cons ch srahc))]
            [else (let ([ach (peek-char /dev/xmlin 1)])
                    (cond [(eof-object? ach) (read-char /dev/xmlin) (reverse (cons ch srahc))]
                          [(eq? ach #\>) (reverse srahc)]
                          [else (read-string 2 /dev/xmlin) (consume-literal (list* ach ch srahc))]))]))))

(define xml-read-string : (-> Input-Port Natural Char String)
  (lambda [/dev/xmlin tailsize leader]
    (define total : Nonnegative-Fixnum (unsafe-fx+ tailsize 1))
    (define s : String (make-string total leader))

    (read-string! s /dev/xmlin 1 total)
    s))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml-doc-scope++ : (-> XML-Scope XML-Scope)
  (lambda [scope]
    (cond [(index? scope) (+ scope 1)]
          [else 0])))

(define xml-doc-scope-- : (-> XML-Scope XML-Scope)
  (lambda [scope]
    (cond [(and (index? scope) (> scope 0)) (- scope 1)]
          [else xml-default-scope])))

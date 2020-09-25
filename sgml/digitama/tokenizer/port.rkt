#lang typed/racket/base

;;; https://www.w3.org/TR/xml/

(provide (all-defined-out))

(require racket/string)

(require "characters.rkt")
(require "delimiter.rkt")
(require "errno.rkt")

(require racket/unsafe/ops)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Performance Hint:
;; 0. See schema/digitama/exchange/csv/reader/port.rkt
;; 1. Checking empty file before reading makes it oscillate(500ms for 2.1MB xslx), weird
;; 2. for long strings, `cons`ing each char should be avoided 

(define-type XML-Error (Pairof (Listof Char) Symbol))
(define-type XML-Datum (U Char Symbol String Index Keyword (Boxof String) XML-White-Space XML-Error))

(define-type XML-Scope (U Symbol Fixnum))
(define-type XML-Token-Consumer (Rec λ (-> Input-Port Char XML-Scope (Values (U XML-Datum EOF) λ XML-Scope))))

(struct xml-white-space ([raw : String]) #:type-name XML-White-Space)
(struct xml-new-line xml-white-space () #:type-name XML-New-Line)
(struct xml-comment xml-white-space () #:type-name XML-Comment)

(define xml-collapsed-whitespace : XML-White-Space (xml-white-space ""))
(define xml-initial-scope : XML-Scope 'initial)
(define xml-default-scope : XML-Scope 'dtd)

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
  ;;; https://www.w3.org/TR/xml/#sec-common-syn
  (lambda [/dev/xmlin consume scope]
    (define ch : (U Char EOF) (read-char /dev/xmlin))
    
    (cond [(eof-object? ch) (values eof consume scope)]
          [else (consume /dev/xmlin ch scope)])))

(define xml-consume-token:* : XML-Token-Consumer
  (lambda [/dev/xmlin ch scope]
    ;;; Do not skip whitespace here, or the lexer will not find the right place to start an open parenthesis.
    (if (index? scope)
        (cond [(char-whitespace? ch) (values (xml-consume-whitespace /dev/xmlin ch) xml-consume-token:* scope)]
              [(eq? ch #\<) (xml-consume-open-token /dev/xmlin xml-consume-token:* scope)]
              [(eq? ch #\&) (values (xml-consume-reference-token /dev/xmlin ch) xml-consume-token:* scope)]
              [(or (eq? ch #\?) (eq? ch #\])) (xml-consume-close-token /dev/xmlin ch xml-consume-token:* scope)] ; for PIs and CDATAs
              [else (values (xml-consume-contentchars /dev/xmlin ch) xml-consume-token:* scope)])
        (cond [(char-whitespace? ch) (xml-skip-whitespace /dev/xmlin) (values xml-collapsed-whitespace xml-consume-token:* scope)]
              [(or (xml-name-char? ch) (eq? ch #\#))
               (let ([kw (xml-consume-nmtoken /dev/xmlin ch)])
                 (case kw
                   [(PUBLIC SYSTEM) (values kw xml-consume-token:* kw)]
                   [else (values kw xml-consume-token:* scope)]))]
              [else (case ch
                      [(#\<) (xml-consume-open-token /dev/xmlin xml-consume-token:* scope)]
                      [(#\>) (values ch xml-consume-token:* scope)]
                      [(#\= #\( #\) #\[) (values ch xml-consume-token:* scope)]
                      [(#\' #\") (xml-consume-token:literals /dev/xmlin ch scope)]
                      [(#\? #\/ #\]) (xml-consume-close-token /dev/xmlin ch xml-consume-token:* scope)]
                      [(#\& #\%) (values (xml-consume-reference-token /dev/xmlin ch) xml-consume-token:* scope)]
                      [else (values ch xml-consume-token:* scope)])]))))

(define xml-consume-token:start-condition : XML-Token-Consumer
  ;;; https://www.w3.org/TR/xml/#sec-condition-sect
  (lambda [/dev/xmlin ch scope]
    (cond [(xml-name-start-char? ch) (values (xml-consume-nmtoken /dev/xmlin ch) xml-consume-token:start-condition-block scope)]
          [(eq? ch #\%) (values (xml-consume-reference-token /dev/xmlin ch) xml-consume-token:start-condition-block scope)]
          [(char-whitespace? ch) (xml-skip-whitespace /dev/xmlin) (xml-consume-token /dev/xmlin xml-consume-token:start-condition scope)]
          [else (values (cons (xml-consume-chars-literal/exclusive /dev/xmlin #\[ (list ch)) !char) xml-consume-token:* scope)])))

(define xml-consume-token:start-condition-block : XML-Token-Consumer
  ;;; https://www.w3.org/TR/xml/#sec-condition-sect
  (lambda [/dev/xmlin ch scope]
    (cond [(eq? ch #\[) (values csec& xml-consume-token:* scope)]
          [(char-whitespace? ch) (xml-skip-whitespace /dev/xmlin) (xml-consume-token /dev/xmlin xml-consume-token:start-condition-block scope)]
          [else (values (cons (xml-consume-chars-literal/exclusive /dev/xmlin #\[ (list ch)) !char) xml-consume-token:start-condition-block scope)])))

(define xml-consume-token:start-decl-name : XML-Token-Consumer
  (lambda [/dev/xmlin ch scope]
    (if (xml-name-start-char? ch)
        (let ([DECLNAME (xml-consume-nmtoken /dev/xmlin ch)])
          (values DECLNAME
                  (case DECLNAME
                    [(ENTITY) xml-consume-token:entity-name]
                    [else xml-consume-token:*])
                  DECLNAME))
        (values (cons (xml-consume-chars-literal/within-tag /dev/xmlin (list ch))
                      (xml-!char-errno ch))
                xml-consume-token:* scope))))

(define xml-consume-token:entity-name : XML-Token-Consumer
  ;;; https://www.w3.org/TR/xml/#sec-entity-decl
  (lambda [/dev/xmlin ch scope]
    (cond [(char-whitespace? ch) (xml-skip-whitespace /dev/xmlin) (xml-consume-token /dev/xmlin xml-consume-token:entity-name scope)]
          [(eq? ch #\%)
           (if (eq? scope 'ENTITY)
               (values ch xml-consume-token:entity-name 'ENTITY%)
               (values (cons (xml-consume-chars-literal/within-tag /dev/xmlin (list ch))
                             !char)
                       xml-consume-token:* xml-default-scope))]
          [(xml-name-start-char? ch)
           (let ([name (xml-consume-namechars /dev/xmlin ch)])
             (values (if (eq? scope 'ENTITY)
                         (string->unreadable-symbol name)
                         (string->keyword name))
                     xml-consume-token:* 'ENTITY))]
          [else (values (cons (xml-consume-chars-literal/within-tag /dev/xmlin (list ch)) !char) xml-consume-token:* xml-default-scope)])))

(define xml-consume-token:literals : XML-Token-Consumer
  ;;; https://www.w3.org/TR/xml/#NT-EntityValue
  (lambda [/dev/xmlin ch scope]
    (case scope
      [(ATTLIST) (values (xml-consume-attribute-value /dev/xmlin ch) xml-consume-token:* scope)]
      [(ENTITY) (values (xml-consume-entity-value /dev/xmlin ch) xml-consume-token:* xml-default-scope)]
      [(PUBLIC) (values (xml-consume-pubid-literal /dev/xmlin ch) xml-consume-token:* 'SYSTEM)]
      [else (values (xml-consume-system-literal /dev/xmlin ch) xml-consume-token:* xml-default-scope)])))

(define xml-consume-token:start-tag-name : XML-Token-Consumer
  ;;; https://www.w3.org/TR/xml/#NT-STag
  (lambda [/dev/xmlin ch scope]
    (values (if (xml-name-start-char? ch)
                (xml-consume-nmtoken /dev/xmlin ch)
                (cons (xml-consume-chars-literal/within-tag /dev/xmlin (list ch)) !char))
            xml-consume-token:tag-attr-name scope)))

(define xml-consume-token:end-tag-name : XML-Token-Consumer
  ;;; https://www.w3.org/TR/xml/#NT-ETag
  (lambda [/dev/xmlin ch scope]
    (values (if (xml-name-start-char? ch)
                (xml-consume-nmtoken /dev/xmlin ch)
                (cons (xml-consume-chars-literal/within-tag /dev/xmlin (list ch))
                      (xml-!char-errno ch)))
            xml-consume-token:tag-end scope)))

(define xml-consume-token:tag-attr-name : XML-Token-Consumer
  ;;; https://www.w3.org/TR/xml/#NT-Attribute
  (lambda [/dev/xmlin ch scope]
    (cond [(char-whitespace? ch) (xml-skip-whitespace /dev/xmlin) (xml-consume-token /dev/xmlin xml-consume-token:tag-attr-name scope)]
          [(xml-name-start-char? ch) (values (xml-consume-nmtoken /dev/xmlin ch) xml-consume-token:tag-attr-eq scope)]
          [(eq? ch #\>) (values stag> xml-consume-token:* scope)] ; non-empty (start) tag does not have a close delimiter.
          [(eq? ch #\/)
           (let ([nch (read-char /dev/xmlin)])
             (cond [(eq? nch #\>) (values /> xml-consume-token:* (xml-doc-scope-- scope))]
                   [else (values (cons (xml-consume-chars-literal/within-tag /dev/xmlin (if (eof-object? nch) (list ch) (list nch ch)) #true) !char)
                                 xml-consume-token:tag-attr-name scope)]))]
          [(eq? ch #\?)
           (let ([nch (read-char /dev/xmlin)])
             (cond [(eq? nch #\>) (values ?> xml-consume-token:* (if (eq? scope xml-initial-scope) xml-default-scope scope))]
                   [else (values (cons (xml-consume-chars-literal/within-tag /dev/xmlin (if (eof-object? nch) (list ch) (list nch ch)) #true) !char)
                                 xml-consume-token:tag-attr-name scope)]))]
          [else (values (cons (xml-consume-chars-literal/within-tag /dev/xmlin (list ch) #true) !char) xml-consume-token:tag-attr-name scope)])))

(define xml-consume-token:tag-attr-eq : XML-Token-Consumer
  ;;; https://www.w3.org/TR/xml/#NT-Attribute
  (lambda [/dev/xmlin ch scope]
    (if (eq? ch #\=)
        (values ch xml-consume-token:tag-attr-value scope)
        (values (cons (xml-consume-chars-literal/within-tag /dev/xmlin (list ch) #true)
                      (xml-!char-errno ch))
                xml-consume-token:tag-attr-name scope))))

(define xml-consume-token:tag-attr-value : XML-Token-Consumer
  ;;; https://www.w3.org/TR/xml/#NT-Attribute
  (lambda [/dev/xmlin ch scope]
    (values (if (or (eq? ch #\') (eq? ch #\"))
                (xml-consume-attribute-value /dev/xmlin ch)
                (cons (xml-consume-chars-literal/within-tag /dev/xmlin (list ch) #true)
                      (xml-!char-errno ch)))
            xml-consume-token:tag-attr-name scope)))

(define xml-consume-token:tag-end : XML-Token-Consumer
  (lambda [/dev/xmlin ch scope]
    (cond [(char-whitespace? ch) (xml-skip-whitespace /dev/xmlin) (xml-consume-token /dev/xmlin xml-consume-token:tag-end scope)]
          [(eq? ch #\>) (values ch xml-consume-token:* (xml-doc-scope-- scope))]
          [else (values (cons (xml-consume-chars-literal/exclusive /dev/xmlin #\> (list ch)) !char) xml-consume-token:* scope)])))

(define xml-consume-token:pi-target : XML-Token-Consumer
  ;;; https://www.w3.org/TR/xml/#sec-pi
  (lambda [/dev/xmlin ch scope]
    (if (xml-name-start-char? ch)
        (let ([target (xml-consume-namechars /dev/xmlin ch)])
          ; DrRacket's color lexer parses the XMLDecl which should be eaten by the #lang reader
          ; Using reversed names is just a validity error     
          (if (and (eq? scope xml-initial-scope) (string-ci=? target "xml"))
              (values (string->symbol target) xml-consume-token:tag-attr-name scope)
              (values (string->symbol target) xml-consume-token:pi-body scope)))
        (values (cons (xml-consume-chars-literal/exclusive /dev/xmlin #\? (list ch) #\>)
                      (xml-!char-errno ch))
                xml-consume-token:* scope))))

(define xml-consume-token:pi-body : XML-Token-Consumer
  ;;; https://www.w3.org/TR/xml/#sec-pi
  (lambda [/dev/xmlin ch scope]
    (values (if (char-whitespace? ch)
                (string-trim (list->string (xml-consume-chars-literal/exclusive /dev/xmlin #\? null #\>)) #:right? #false)
                (or (and (eq? ch #\?) (eq? (peek-char /dev/xmlin) #\>) (read-char /dev/xmlin) ?>)
                    (cons (xml-consume-chars-literal/exclusive /dev/xmlin #\? (list ch) #\>) !char)))
            xml-consume-token:* scope)))

(define xml-consume-token:cdata : XML-Token-Consumer
  ;;; https://www.w3.org/TR/xml/#sec-cdata-sect
  ;;; https://www.w3.org/TR/xml/#sec-line-ends
  (lambda [/dev/xmlin ch scope]
    (define cdata-chars : (Listof Char)
      (let consume-cdata ([srahc : (Listof Char) (list ch)])
        (define ch : (U EOF Char) (peek-char /dev/xmlin))
        (cond [(eof-object? ch) (reverse srahc)]
              [(eq? ch #\return)
               (let ([ach (peek-char /dev/xmlin 1)])
                 (when (eq? ch #\newline) (xml-drop-string /dev/xmlin 2))
                 (consume-cdata (cons #\newline srahc)))]
              [(not (eq? ch #\])) (read-char /dev/xmlin) (consume-cdata (cons ch srahc))]
              [else (let ([ach (peek-char /dev/xmlin 1)])
                      (cond [(eof-object? ach) (read-char /dev/xmlin) (reverse (cons ch srahc))]
                            [(not (eq? ach #\])) (read-char /dev/xmlin) (consume-cdata (cons ch srahc))]
                            [else (let ([aach (peek-char /dev/xmlin 2)])
                                    (cond [(eof-object? aach) (xml-drop-string /dev/xmlin 2) (reverse (list* ach ch srahc))]
                                          [(not (eq? aach #\>)) (xml-drop-string /dev/xmlin 2) (consume-cdata (list* ach ch srahc))]
                                          [else (reverse srahc)]))]))])))
    
    (values (list->string cdata-chars) xml-consume-token:* scope)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml-consume-open-token : (-> Input-Port XML-Token-Consumer XML-Scope (Values (U Char Symbol XML-Comment XML-Error) XML-Token-Consumer XML-Scope))
  (lambda [/dev/xmlin consume scope]
    (define maybe-ch : (U Char EOF) (peek-char /dev/xmlin 0))
    (cond [(eq? maybe-ch #\!)
           (let ([dispatcher (peek-char /dev/xmlin 1)])
             (cond [(eq? dispatcher #\[)
                    (cond [(equal? (peek-string 6 2 /dev/xmlin) "CDATA[") (xml-drop-string /dev/xmlin 8) (values <!&CDATA& xml-consume-token:cdata scope)]
                          [else (xml-drop-string /dev/xmlin 2) (values <!& xml-consume-token:start-condition scope)])]
                   [(and (eq? dispatcher #\-) (eq? (peek-char /dev/xmlin 2) #\-))
                    (xml-drop-string /dev/xmlin 3) (values (xml-consume-comment-tail /dev/xmlin) consume scope)]
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
                            [else (xml-drop-string /dev/xmlin 2) (values $$> xml-consume-token:* scope)]))])
        (cond [(not (eq? maybe-char #\>)) (values leading-char consume scope)]
              [(eq? leading-char #\?) (read-char /dev/xmlin) (values ?> consume scope)]
              [else (read-char /dev/xmlin) (values /> xml-consume-token:* (xml-doc-scope-- scope))]))))
  
(define xml-consume-nmtoken : (-> Input-Port Char Symbol)
  ;;; https://www.w3.org/TR/xml/#sec-common-syn
  ;;; https://www.w3.org/TR/xml/#NT-Names
  ;;; https://www.w3.org/TR/xml/#NT-Nmtokens
  (lambda [/dev/xmlin leader]
    (string->symbol (xml-consume-namechars /dev/xmlin leader))))

(define xml-consume-reference-token : (-> Input-Port Char (U Symbol Keyword Index XML-Error))
  ;;; https://www.w3.org/TR/xml/#sec-references
  (lambda [/dev/xmlin leader]
    (define ch : (U Char EOF) (read-char /dev/xmlin))
    (cond [(eof-object? ch) (cons (list leader) !eof)]
          [(eq? ch #\;) (cons (list leader ch) !name)]
          [(eq? leader #\%) (xml-consume-entity-reference /dev/xmlin leader ch)]
          [(not (eq? ch #\#)) (xml-consume-entity-reference /dev/xmlin leader ch)]
          [else (let ([char-leader (read-char /dev/xmlin)])
                  (cond [(eof-object? char-leader) (cons (list leader ch) !eof)]
                        [(eq? char-leader #\x) (xml-consume-char-reference /dev/xmlin #false)]
                        [else (xml-consume-char-reference /dev/xmlin char-leader)]))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml-consume-whitespace : (-> Input-Port Char XML-White-Space)
  ;;; https://www.w3.org/TR/xml/#NT-S
  ;;; https://www.w3.org/TR/xml/#sec-line-ends
  (lambda [/dev/xmlin leader]
    (let read-whitespace ([span : Nonnegative-Fixnum 0]
                          [newline? : Boolean (xml-newline-char? leader)])
      (define ?space : (U Char EOF) (peek-char /dev/xmlin span))
      (cond [(eof-object? ?space) (xml-make-whitespace /dev/xmlin span leader newline?)]
            [(eq? ?space #\space) (read-whitespace (unsafe-fx+ span 1) newline?)]
            [(char-whitespace? ?space) (read-whitespace (unsafe-fx+ span 1) (or newline? (xml-newline-char? ?space)))]
            [else (xml-make-whitespace /dev/xmlin span leader newline?)]))))

(define xml-skip-whitespace : (-> Input-Port Any)
  (lambda [/dev/xmlin]
    (regexp-match-positions #px"\\s*" /dev/xmlin)))

(define xml-consume-comment-tail : (-> Input-Port (U XML-Comment XML-Error))
  ;;; https://www.w3.org/TR/xml/#sec-comments
  (lambda [/dev/xmlin]
    (let read-comment ([srahc : (Listof Char) null]
                       [malformed? : Boolean #false])
      (define maybe-char : (U Char EOF) (read-char /dev/xmlin))
      (cond [(eof-object? maybe-char) (cons (list* #\< #\! #\- #\- (reverse srahc)) !eof)]
            [(not (eq? maybe-char #\-)) (read-comment (cons maybe-char srahc) malformed?)]
            [else (let ([maybe-- (peek-char /dev/xmlin 0)]
                        [maybe-> (peek-char /dev/xmlin 1)])
                    (define -? : Boolean (eq? maybe-- #\-))
                    (define >? : Boolean (eq? maybe-> #\>))
                    (cond [(and -? >?)
                           (xml-drop-string /dev/xmlin 2)
                           (cond [(not malformed?) (xml-comment (list->string (reverse srahc)))]
                                 [else (cons (list* #\< #\! #\- #\- (reverse (list* #\> #\- #\- srahc))) !comment)])]
                          [else (read-comment (cons maybe-char srahc) (or malformed? -? (null? srahc)))]))]))))
  
(define xml-consume-namechars : (-> Input-Port Char String)
  ;;; https://www.w3.org/TR/xml/#NT-NameChar
  (lambda [/dev/xmlin leader]
    (let consume-name ([span : Nonnegative-Fixnum 0]
                       [skip : Nonnegative-Fixnum 0])
      (define ch : (U EOF Char) (peek-char /dev/xmlin skip))
      (cond [(eof-object? ch) (xml-read-string /dev/xmlin span leader)]
            [(xml-name-char? ch) (consume-name (unsafe-fx+ span 1) (unsafe-fx+ skip (char-utf-8-length ch)))]
            [else (xml-read-string /dev/xmlin span leader)]))))

(define xml-consume-contentchars : (-> Input-Port Char (U String XML-Error))
  ;;; https://www.w3.org/TR/xml/#NT-content
  (lambda [/dev/xmlin leader]
    (let consume-content ([span : Nonnegative-Fixnum 0]
                          [skip : Nonnegative-Fixnum 0])
      (define ch : (U EOF Char) (peek-char /dev/xmlin skip))
      (cond [(eof-object? ch) (xml-read-string /dev/xmlin span leader)]
            [(xml-content-char? ch) (consume-content (unsafe-fx+ span 1) (unsafe-fx+ skip (char-utf-8-length ch)))]
            [else (xml-read-string /dev/xmlin span leader)]))))

(define xml-consume-char-reference : (-> Input-Port (Option Char) (U Index XML-Error))
  ;;; https://www.w3.org/TR/xml/#NT-CharRef
  (lambda [/dev/xmlin maybe-leader]
    (define-values (base char-digit?) (if (not maybe-leader) (values 16 char-hexdigit?) (values 10 char-numeric?)))
    
    (let read-char-reference ([ch : (U EOF Char) (or maybe-leader (read-char /dev/xmlin))]
                              [srahc : (Listof Char) (if maybe-leader (list #\# #\&) (list #\x #\# #\&))]
                              [code : Fixnum 0])
      (cond [(eof-object? ch) (cons (reverse srahc) !eof)]
            [(char-digit? ch) (read-char-reference (read-char /dev/xmlin) (cons ch srahc) (unsafe-fx+ (unsafe-fx* code base) (char->hexadecimal ch)))]
            [(eq? ch #\;) (natural->char-entity code)]
            [else (cons (xml-consume-chars-literal /dev/xmlin #\; (cons ch srahc)) !char)]))))

(define xml-consume-entity-reference : (-> Input-Port Char Char (U Symbol Keyword XML-Error))
  ;;; https://www.w3.org/TR/xml/#NT-EntityRef
  (lambda [/dev/xmlin type leader]
    (define parameter-entity? : Boolean (eq? type #\%))
    (cond [(not (xml-name-start-char? leader)) (cons (xml-consume-chars-literal /dev/xmlin #\; (list leader type)) !char)]
          [else (let read-entity-reference ([srahc : (Listof Char) (list leader)])
                  (define ch : (U EOF Char) (read-char /dev/xmlin))
                  (cond [(eof-object? ch) (cons (reverse srahc) !eof)]
                        [(xml-name-char? ch) (read-entity-reference (cons ch srahc))]
                        [(not (eq? ch #\;)) (cons (xml-consume-chars-literal /dev/xmlin #\; (cons ch srahc)) !char)]
                        [(eq? type #\%) (string->keyword (list->string (reverse srahc)))]
                        [else (string->unreadable-symbol (list->string (reverse srahc)))]))])))

(define xml-consume-entity-value : (-> Input-Port Char (U String (Boxof String) XML-Error))
  ;;; https://www.w3.org/TR/xml/#NT-EntityValue
  (lambda [/dev/xmlin quote]
    (let consume-literal ([srahc : (Listof Char) null]
                          [ref? : Boolean #false])
      (define ch : (U EOF Char) (read-char /dev/xmlin))
      (cond [(eq? ch quote) (let ([v (list->string (reverse srahc))]) (if (not ref?) v (box v)))]
            [(eof-object? ch) (cons (reverse srahc) !eof)]
            [else (consume-literal (cons ch srahc) (or ref? (eq? ch #\%) (eq? ch #\&)))]))))

(define xml-consume-attribute-value : (-> Input-Port Char (U String (Boxof String) XML-Error))
  ;;; https://www.w3.org/TR/xml/#NT-AttValue
  (lambda [/dev/xmlin quote]
    (let consume-literal ([srahc : (Listof Char) null]
                          [unnormalized? : Boolean #false])
      (define ch : (U EOF Char) (read-char /dev/xmlin))
      (cond [(eq? ch quote) (let ([v (list->string (reverse srahc))]) (if (not unnormalized?) v (box v)))]
            [(eof-object? ch) (cons (reverse srahc) !eof)]
            [(eq? ch #\<) (cons (xml-consume-chars-literal /dev/xmlin quote (cons ch srahc)) !char)]
            [else (consume-literal (cons ch srahc)
                                   (or unnormalized? (eq? ch #\&)
                                       (char-whitespace? ch)))]))))

(define xml-consume-system-literal : (-> Input-Port Char (U String XML-Error))
  ;;; https://www.w3.org/TR/xml/#NT-SystemLiteral
  (lambda [/dev/xmlin quote]
    (let consume-literal ([srahc : (Listof Char) null]
                          [space? : Boolean #false])
      (define ch : (U EOF Char) (read-char /dev/xmlin))
      (cond [(eq? ch quote) (list->string (reverse srahc))]
            [(eof-object? ch) (cons (reverse srahc) !eof)]
            [(char-whitespace? ch) (consume-literal srahc (or space? (pair? srahc)))]
            [else (consume-literal (if (not space?) (cons ch srahc) (list* ch #\space srahc)) #false)]))))

(define xml-consume-pubid-literal : (-> Input-Port Char (U String XML-Error))
  ;;; https://www.w3.org/TR/xml/#NT-PubidLiteral
  (lambda [/dev/xmlin quote]
    (let consume-literal ([srahc : (Listof Char) null]
                          [space? : Boolean #false])
      (define ch : (U EOF Char) (read-char /dev/xmlin))
      (cond [(eq? ch quote) (list->string (reverse srahc))]
            [(eof-object? ch) (cons (reverse srahc) !eof)]
            [(xml-pubid-char/no-spaces? ch) (consume-literal (if (not space?) (cons ch srahc) (list* ch #\space srahc)) #false)]
            [(char-whitespace? ch) (consume-literal srahc (or space? (pair? srahc)))]
            [else (cons (xml-consume-chars-literal /dev/xmlin quote (cons ch srahc)) !char)]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml-consume-chars-literal : (->* (Input-Port Char (Listof Char)) ((Option Char)) (Listof Char))
  (lambda [/dev/xmlin boundary chars [ahead-boundary #false]]
    (let consume-literal ([srahc : (Listof Char) chars])
      (define ch : (U EOF Char) (read-char /dev/xmlin))
      (cond [(eof-object? ch) (reverse srahc)]
            [(not (eq? ch boundary)) (consume-literal (cons ch srahc))]
            [(not ahead-boundary) (reverse srahc)]
            [else (let ([ach (read-char /dev/xmlin)])
                    (cond [(eof-object? ach) (reverse (cons ch srahc))]
                          [(not (eq? ach ahead-boundary)) (consume-literal (list* ach ch srahc))]
                          [else (reverse srahc)]))]))))

(define xml-consume-chars-literal/exclusive : (->* (Input-Port Char (Listof Char)) ((Option Char)) (Listof Char))
  (lambda [/dev/xmlin boundary chars [ahead-boundary #false]]
    (let consume-literal ([srahc : (Listof Char) chars])
      (define ch : (U EOF Char) (peek-char /dev/xmlin))
      (cond [(eof-object? ch) (reverse srahc)]
            [(not (eq? ch boundary)) (read-char /dev/xmlin) (consume-literal (cons ch srahc))]
            [(not ahead-boundary) (reverse srahc)]
            [else (let ([ach (peek-char /dev/xmlin 1)])
                    (cond [(eof-object? ach) (read-char /dev/xmlin) (reverse (cons ch srahc))]
                          [(not (eq? ach ahead-boundary)) (xml-drop-string /dev/xmlin 2) (consume-literal (list* ach ch srahc))]
                          [else (reverse srahc)]))]))))

(define xml-consume-chars-literal/within-tag : (->* (Input-Port (Pairof Char (Listof Char))) (Boolean) (Listof Char))
  (lambda [/dev/xmlin chars [stop-at-whitespace? #false]]
    (when (and stop-at-whitespace? (char-whitespace? (car chars)))
      (xml-skip-whitespace /dev/xmlin))
    
    (let consume-literal ([srahc : (Listof Char) chars])
      (define ch : (U EOF Char) (peek-char /dev/xmlin))
      (cond [(eof-object? ch) (reverse srahc)]
            [(eq? ch #\>) (reverse srahc)]
            [(and stop-at-whitespace? (char-whitespace? ch)) (xml-skip-whitespace /dev/xmlin) (reverse srahc)]
            [(not (eq? ch #\/)) (read-char /dev/xmlin) (consume-literal (cons ch srahc))]
            [else (let ([ach (peek-char /dev/xmlin 1)])
                    (cond [(eof-object? ach) (read-char /dev/xmlin) (reverse (cons ch srahc))]
                          [(eq? ach #\>) (reverse srahc)]
                          [else (xml-drop-string /dev/xmlin 2) (consume-literal (list* ach ch srahc))]))]))))

(define xml-read-string : (-> Input-Port Natural Char String)
  (lambda [/dev/xmlin tailsize leader]
    (define total : Nonnegative-Fixnum (unsafe-fx+ tailsize 1))
    (define s : String (make-string total leader))

    (read-string! s /dev/xmlin 1 total)
    s))

(define xml-drop-string : (-> Input-Port Natural Void)
  (let ([blackhole (make-string 8)])
    (lambda [/dev/xmlin n]
      (read-string! blackhole /dev/xmlin 0 n)
      (void))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml-doc-scope++ : (-> XML-Scope XML-Scope)
  (lambda [scope]
    (cond [(index? scope) (+ scope 1)]
          [else 0])))

(define xml-doc-scope-- : (-> XML-Scope XML-Scope)
  (lambda [scope]
    (cond [(and (index? scope) (> scope 0)) (- scope 1)]
          [else xml-default-scope])))

(define xml-!char-errno : (-> Char Symbol)
  (lambda [ch]
    (if (char-whitespace? ch) !space !char)))

(define xml-make-whitespace : (-> Input-Port Nonnegative-Fixnum Char Boolean XML-White-Space)
  (lambda [/dev/xmlin span leader newline?]
    (define ws : String (xml-read-string /dev/xmlin span leader))
    
    (if (not newline?)
        (xml-white-space ws)
        (xml-new-line ws))))

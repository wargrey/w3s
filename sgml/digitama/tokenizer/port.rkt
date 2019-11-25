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
(define-type XML-Datum (U Char Symbol String Keyword XML-White-Space XML-Error))
(define-type XML-Literal (U 'Entity 'Attribute 'System 'Public))

(struct xml-white-space ([raw : String]) #:type-name XML-White-Space)

(define xml-empty-attributes : (Immutable-HashTable Symbol XML-Datum) (make-immutable-hasheq))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define read-xml-tokens : (-> Input-Port (Listof XML-Datum))
  (lambda [/dev/xmlin]
    (let read-xml ([snekot : (Listof XML-Datum) null]
                   [literal-type : XML-Literal 'Attribute])
      (define token : (U XML-Datum EOF) (xml-consume-token /dev/xmlin literal-type))
      (cond [(eof-object? token) (reverse snekot)]
            [else (read-xml (cons token snekot)
                            (xml-next-literal-type token literal-type))]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml-consume-token : (->* (Input-Port) (XML-Literal) (U XML-Datum EOF))
  ;;; https://www.w3.org/TR/xml11/#sec-common-syn
  (lambda [/dev/xmlin [literals 'Attribute]]
    (define ch : (U Char EOF) (read-char /dev/xmlin))
    (cond [(eof-object? ch) eof]
          [(char-whitespace? ch) (xml-consume-whitespace /dev/xmlin ch)]
          [(xml-name-start-char? ch) (xml-consume-nmtoken /dev/xmlin ch)]
          [else (case ch
                  [(#\<) (xml-consume-open-token /dev/xmlin)]
                  [(#\? #\/) (xml-consume-close-token /dev/xmlin ch)]
                  [(#\' #\") (xml-consume-literal-token /dev/xmlin ch literals)]
                  [else ch])])))

#;(define xml-consume-token/skip-whitespace : (->* (Input-Port (U Char EOF)) (XML-Literal) (Values XML-Datum (U Char EOF)))
  ;;; https://www.w3.org/TR/xml11/#sec-common-syn
  (lambda [/dev/xmlin leading-char [literals 'Attribute]]
    (define-values (token maybe-char) (xml-consume-token /dev/xmlin leading-char literals))

    (cond [(xml-white-space? token) (xml-consume-token /dev/xmlin maybe-char literals)]
          [else (values token maybe-char)])))

(define xml-consume-open-token : (-> Input-Port Char)
  (lambda [/dev/xmlin]
    (define maybe-char : (U Char EOF) (peek-char /dev/xmlin 0))
    (case maybe-char
      [(#\?) (read-char /dev/xmlin) <?]
      [(#\!) (read-char /dev/xmlin) <!]
      [(#\/) (read-char /dev/xmlin) </]
      [else #\<])))

(define xml-consume-close-token : (-> Input-Port Char Char)
  (lambda [/dev/xmlin leading-char]
    (define maybe-char : (U Char EOF) (peek-char /dev/xmlin 0))
    (cond [(not (eq? maybe-char #\>)) leading-char]
          [(eq? leading-char #\?) (read-char /dev/xmlin) ?>]
          [else (read-char /dev/xmlin) />])))

#;(define xml-consume-cd-token : (-> XML-Srcloc Char XML-Datum)
  ;;; https://drafts.xmlwg.org/xml-syntax/#CDO-token-diagram
  ;;; https://drafts.xmlwg.org/xml-syntax/#CDC-token-diagram
  (lambda [srcloc open/close]
    (define /dev/xmlin : Input-Port (xml-srcloc-in srcloc))
    (if (char=? open/close #\<)
        (let ([cdo : (U EOF String) (peek-string 3 0 /dev/xmlin)])
          (cond [(and (string? cdo) (string=? cdo "!--")) (read-string 3 /dev/xmlin) (xml-make-token srcloc xml:cdata '<!--)]
                [else (xml-make-token srcloc xml:delim #\<)]))
        (let ([cdc : (U EOF String) (peek-string 2 0 /dev/xmlin)])
          (cond [(eof-object? cdc) (xml-make-token srcloc xml:delim #\-)]
                [(string=? cdc "->") (read-string 2 /dev/xmlin) (xml-make-token srcloc xml:cdata '-->)]
                [else (xml-consume-nmtoken srcloc #\-)])))))
  
(define xml-consume-nmtoken : (-> Input-Port Char Symbol)
  ;;; https://www.w3.org/TR/xml11/#sec-common-syn
  ;;; https://www.w3.org/TR/xml11/#NT-Names
  ;;; https://www.w3.org/TR/xml11/#NT-Nmtokens
  (lambda [/dev/xmlin leader]
    (string->symbol (xml-consume-namechars /dev/xmlin leader))))

(define xml-consume-literal-token : (-> Input-Port Char XML-Literal (U String XML-Error))
  ;;; https://www.w3.org/TR/xml11/#NT-SystemLiteral
  (lambda [/dev/xmlin quote type]
    (case type
      [(Attribute) (xml-consume-attribute-value /dev/xmlin quote)]
      [(Entity) (xml-consume-entity-value /dev/xmlin quote)]
      [(Public) (xml-consume-pubid-literal /dev/xmlin quote)]
      [else (xml-consume-system-literal /dev/xmlin quote)])))

(define xml-consume-hexadecimal : (->* (Input-Port Byte) (Fixnum #:\s?$? Boolean) (Values Fixnum Byte))
  (lambda [/dev/cssin --count [result 0] #:\s?$? [eat-last-whitespace? #false]]
    (define hex : (U EOF Char) (peek-char /dev/cssin))
    (cond [(or (eof-object? hex) (not (char-hexdigit? hex)) (zero? --count))
           (when (and eat-last-whitespace? (char? hex) (char-whitespace? hex)) (read-char /dev/cssin))
           (values result --count)]
          [else (read-char /dev/cssin) (xml-consume-hexadecimal #:\s?$? eat-last-whitespace?
                                                                /dev/cssin (unsafe-fx- --count 1)
                                                                (unsafe-fx+ (unsafe-fxlshift result 4)
                                                                            (char->hexadecimal hex)))])))

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
  
(define xml-consume-namechars : (-> Input-Port Char String)
  ;;; https://www.w3.org/TR/xml11/#NT-NameChar
  (lambda [/dev/xmlin leader]
    (let consume-name ([span : Nonnegative-Fixnum 0])
      (define ch : (U EOF Char) (peek-char /dev/xmlin span))
      (cond [(eof-object? ch) (xml-read-string /dev/xmlin span leader)]
            [(xml-name-char? ch) (consume-name (unsafe-fx+ span 1))]
            [else (xml-read-string /dev/xmlin span leader)]))))

(define xml-consume-entity-value : (-> Input-Port Char (U String XML-Error))
  ;;; https://www.w3.org/TR/xml11/#NT-EntityValue
  (lambda [/dev/xmlin quote]
    (let consume-literal ([srahc : (Listof Char) null])
      (define ch : (U EOF Char) (read-char /dev/xmlin))
      (cond [(eof-object? ch) (reverse srahc)]
            [(eq? ch quote) (list->string (reverse srahc))]
            [else (consume-literal (cons ch srahc))]))))

(define xml-consume-attribute-value : (-> Input-Port Char (U String XML-Error))
  ;;; https://www.w3.org/TR/xml11/#NT-AttValue
  (lambda [/dev/xmlin quote]
    (let consume-literal ([srahc : (Listof Char) null])
      (define ch : (U EOF Char) (read-char /dev/xmlin))
      (cond [(eof-object? ch) (reverse srahc)]
            [(eq? ch quote) (list->string (reverse srahc))]
            [else (consume-literal (cons ch srahc))]))))

(define xml-consume-system-literal : (-> Input-Port Char (U String XML-Error))
  ;;; https://www.w3.org/TR/xml11/#NT-SystemLiteral
  (lambda [/dev/xmlin quote]
    (let consume-literal ([srahc : (Listof Char) null])
      (define ch : (U EOF Char) (read-char /dev/xmlin))
      (cond [(eof-object? ch) (reverse srahc)]
            [(eq? ch quote) (list->string (reverse srahc))]
            [else (consume-literal (cons ch srahc))]))))

(define xml-consume-pubid-literal : (-> Input-Port Char (U String XML-Error))
  ;;; https://www.w3.org/TR/xml11/#NT-PubidLiteral
  (lambda [/dev/xmlin quote]
    (let consume-literal ([srahc : (Listof Char) null])
      (define ch : (U EOF Char) (read-char /dev/xmlin))
      (cond [(eof-object? ch) (reverse srahc)]
            [(eq? ch quote) (list->string (reverse srahc))]
            [(xml-pubid-char? ch) (consume-literal (cons ch srahc))]
            [else (xml-consume-error-literal /dev/xmlin quote (cons ch srahc))]))))

(define xml-consume-escaped-char : (-> Input-Port Char)
  ;;; https://drafts.xmlwg.org/xml-syntax/#consume-an-escaped-code-point
  (lambda [xml]
    (define esc : (U EOF Char) (read-char xml))
    (cond [(eof-object? esc) #\uFFFD]
          [(not (char-hexdigit? esc)) esc]
          [else (let-values ([(hex _) (xml-consume-hexadecimal xml (sub1 6) (char->hexadecimal esc) #:\s?$? #true)])
                  (cond [(> hex #x10FFFF) #\uFFFD] ; #\nul and max unicode
                        [(<= #xD800 hex #xDFFF) #\uFFFD] ; surrogate
                        [else (integer->char hex)]))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml-next-literal-type : (-> (U XML-Datum EOF) XML-Literal XML-Literal)
  (lambda [token prev-type]
    (cond [(xml-white-space? token) prev-type]
          [(or (eq? token #\=) (eq? token #\>)) 'Attribute]
          [(or (eq? token 'SYSTEM) (eq? prev-type 'Public)) 'System]
          [(eq? token 'PUBLIC) 'Public]
          [(eq? token 'ENTITY) 'Entity]
          [else prev-type])))

(define xml-consume-error-literal : (-> Input-Port Char (Listof Char) (U String XML-Error))
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

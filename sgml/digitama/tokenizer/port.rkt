#lang typed/racket/base

;;; https://drafts.xmlwg.org/xml-syntax/#tokenization

(provide (all-defined-out))

(require "characters.rkt")
(require "../misc.rkt")

(require racket/fixnum)

;;; Performance Hint:
;; 0. See schema/digitama/exchange/csv/reader/port.rkt
;; 1. Checking empty file before reading makes it oscillates(500ms for 2.1MB xslx), weird

(define-type XML-Error (Listof Char))
(define-type XML-Datum (U Char Symbol String Keyword XML-White-Space XML-Error))

(struct xml-white-space ([raw : (Listof Char)]) #:type-name XML-White-Space)

#;(define in-xml-port : (-> Input-Port (Sequenceof XML-Datum))
  (lambda [/dev/xmlin]
    (define (read-xml [hint : XML-Datum]) : XML-Datum
      (define-values (maybe-row maybe-leader) (read-csv-row /dev/csvin n maybe-char dialect strict? trim-line? maybe-progress-handler topic))
      (cond [(and maybe-leader) (if (and maybe-row) (cons maybe-leader maybe-row) (read-with maybe-leader))]
            [(not maybe-row) (csv-report-final-progress /dev/csvin maybe-progress-handler topic #true) sentinel]
            [else (cons eof maybe-row)]))

    (unless (not skip-header?) (read-line /dev/csvin))
    ((inst make-do-sequence (Pairof (U Char EOF) (Vectorof CSV-Field)) (Vectorof CSV-Field))
     (λ [] (values unsafe-cdr
                   read-csv
                   (read-csv (cons (read-char /dev/csvin) empty-row))
                   (λ [v] (not (eq? v sentinel)))
                   #false
                   #false)))))

(define read-xml/reverse : (-> Input-Port (Listof XML-Datum))
  (lambda [/dev/xmlin]
    (let read-xml ([snekot : (Listof XML-Datum) null]
                   [maybe-char : (U Char EOF) (read-char /dev/xmlin)])
      (define-values (token maybe-leader) (xml-consume-token /dev/xmlin maybe-char))
      (cond [(char? maybe-leader) (read-xml (cons token snekot) maybe-leader)]
            [(eq? token #\uFFFD) snekot]
            [else (cons token snekot)]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml-stream-valid? : (-> Input-Port Boolean)
  (lambda [/dev/xmlin]
    (and (regexp-match-peek #px"^\\s*<[?]xml\\s" /dev/xmlin)
         #true)))

(define xml-consume-declaration : (-> Input-Port (Values (Option Positive-Flonum) (Option String) Boolean))
  (lambda [/dev/xmlin]
    (regexp-match #px"^\\s*" /dev/xmlin)
    
    (let ([ch (read-char /dev/xmlin)])
      (let*-values ([(open nch) (xml-consume-token /dev/xmlin ch)]
                    [(name nch) (xml-consume-token /dev/xmlin nch)])
        (displayln (cons open name))
        (values 1.0 "UTF-8" #true)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml-consume-token : (-> Input-Port (U Char EOF) (Values XML-Datum (U Char EOF)))
  ;;; https://drafts.xmlwg.org/xml-syntax/#error-handling
  ;;; https://drafts.xmlwg.org/xml-syntax/#consume-a-token
  (lambda [/dev/xmlin leading-char]
    (cond [(eof-object? leading-char) (values #\uFFFD eof)]
          [(char-whitespace? leading-char) (xml-consume-whitespace /dev/xmlin leading-char)]
          [(xml-name-start-char? leading-char) (xml-consume-nmtoken /dev/xmlin leading-char)]
          [else (case leading-char
                  [(#\<) (xml-consume-open-token /dev/xmlin)]
                  [(#\' #\") (xml-consume-literal-token /dev/xmlin leading-char)]
                  [(#\=) (values '= (read-char /dev/xmlin))]
                  [else (values leading-char (read-char /dev/xmlin))])])))

(define xml-consume-open-token : (-> Input-Port (Values Symbol (U Char EOF)))
  (lambda [/dev/xmlin]
    (define maybe-char : (U Char EOF) (read-char /dev/xmlin))
    (case maybe-char
      [(#\?) (values '<? (read-char /dev/xmlin))]
      [(#\!) (values '<! (read-char /dev/xmlin))]
      [else (values '< maybe-char)])))

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
  
(define xml-consume-nmtoken : (-> Input-Port Char (Values Symbol (U Char EOF)))
  ;;; https://www.w3.org/TR/xml11/#sec-common-syn
  ;;; https://www.w3.org/TR/xml11/#NT-Names
  ;;; https://www.w3.org/TR/xml11/#NT-Nmtokens
  (lambda [/dev/xmlin leader]
    (define-values (name maybe-next) (xml-consume-namechars /dev/xmlin leader))
    (values (string->symbol name) maybe-next)))

(define xml-consume-literal-token : (-> Input-Port Char (Values (U String XML-Error) (U Char EOF)))
  ;;; https://www.w3.org/TR/xml11/#NT-SystemLiteral
  (lambda [/dev/xmlin quote]
    (define literal : (U String XML-Error) (xml-consume-system-literal /dev/xmlin quote))
    (values literal (read-char /dev/xmlin))))

(define xml-consume-hexadecimal : (->* (Input-Port Byte) (Fixnum #:\s?$? Boolean) (Values Fixnum Byte))
  (lambda [/dev/cssin --count [result 0] #:\s?$? [eat-last-whitespace? #false]]
    (define hex : (U EOF Char) (peek-char /dev/cssin))
    (cond [(or (eof-object? hex) (not (char-hexdigit? hex)) (zero? --count))
           (when (and eat-last-whitespace? (char? hex) (char-whitespace? hex)) (read-char /dev/cssin))
           (values result --count)]
          [else (read-char /dev/cssin) (xml-consume-hexadecimal #:\s?$? eat-last-whitespace?
                                                                /dev/cssin (fx- --count 1)
                                                                (fx+ (fxlshift result 4)
                                                                     (char->hexadecimal hex)))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml-consume-whitespace : (-> Input-Port Char (Values XML-White-Space (U Char EOF)))
  ;;; https://www.w3.org/TR/xml11/#NT-S
  ;;; https://www.w3.org/TR/xml11/#sec-line-ends
  (lambda [/dev/xmlin leader]
    ;; NOTE: The clients take responsibility for normalizing the End-of-Lines if required
    (let read-whitespace ([secaps : (Listof Char) (list leader)])
      (define maybe-space : (U Char EOF) (read-char /dev/xmlin))
      (cond [(eof-object? maybe-space) (values (xml-white-space (reverse secaps)) eof)]
            [(char-whitespace? maybe-space) (read-whitespace (cons maybe-space secaps))]
            [else (values (xml-white-space (reverse secaps)) maybe-space)]))))
  
(define xml-consume-namechars : (-> Input-Port Char (Values String (U Char EOF)))
  ;;; https://www.w3.org/TR/xml11/#NT-NameChar
  (lambda [/dev/xmlin leader]
    (let consume-name ([srahc : (Listof Char) (list leader)])
      (define ch : (U EOF Char) (read-char /dev/xmlin))
      (cond [(eof-object? ch) (values (list->string (reverse srahc)) eof)]
            [(xml-name-char? ch) (consume-name (cons ch srahc))]
            [else (values (list->string (reverse srahc)) ch)]))))

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
                  (cond [(or (fx<= hex 0) (fx> hex #x10FFFF)) #\uFFFD] ; #\nul and max unicode
                        [(<= #xD800 hex #xDFFF) #\uFFFD] ; surrogate
                        [else (integer->char hex)]))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml-consume-error-literal : (-> Input-Port Char (Listof Char) (U String XML-Error))
  (lambda [/dev/xmlin quote chars]
    (let consume-literal ([srahc : (Listof Char) chars])
      (define ch : (U EOF Char) (read-char /dev/xmlin))
      (cond [(or (eq? ch quote) (eof-object? ch)) (reverse srahc)]
            [else (consume-literal (cons ch srahc))]))))

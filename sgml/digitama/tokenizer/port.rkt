#lang typed/racket/base

;;; https://drafts.xmlwg.org/xml-syntax/#tokenization

(provide (except-out (all-defined-out) sof))

(require "characters.rkt")
(require "../misc.rkt")

(require racket/fixnum)

(define-type XML-Error (Listof Char))
(define-type XML-Datum (U Char Symbol String Keyword XML-WhiteSpace XML-Error))

(struct XML-WhiteSpace ([raw : (Listof Char)]))

(define sof : Keyword '#:||)

#;(define in-xml-port : (-> Input-Port (U String Symbol) (Sequenceof XML-Datum))
  (lambda [/dev/xmlin source]
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

(define read-xml/reverse : (-> Input-Port (U String Symbol) (Listof XML-Datum))
  (lambda [/dev/xmlin source]
    (let read-xml ([snekot : (Listof XML-Datum) null]
                   [last-token : XML-Datum sof]
                   [maybe-char : (U Char EOF) (read-char /dev/xmlin)])
      (define-values (token maybe-leader) (xml-consume-token /dev/xmlin source last-token))
      (cond [(not maybe-leader) (if (eq? token sof) snekot (cons token snekot))]
            [(XML-WhiteSpace? token) (read-xml (cons token snekot) last-token maybe-char)]
            [else (read-xml (cons token snekot) token maybe-char)]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml-consume-token : (-> Input-Port (U String Symbol) XML-Datum (Values XML-Datum (Option Char)))
  ;;; https://drafts.xmlwg.org/xml-syntax/#error-handling
  ;;; https://drafts.xmlwg.org/xml-syntax/#consume-a-token
  (lambda [/dev/xmlin source last-token]
    (define maybe-ch (read-char /dev/xmlin))
    (cond [(eof-object? maybe-ch) (values sof #false)]
          [(char-whitespace? maybe-ch) (xml-consume-whitespace /dev/xmlin maybe-ch)]
          [(xml-name-start-char? maybe-ch) (xml-consume-nmtoken /dev/xmlin maybe-ch)]
          [else (case maybe-ch
                  [(#\<) (xml-consume-open-token /dev/xmlin)]
                  [(#\' #\") (xml-consume-literal-token /dev/xmlin maybe-ch)]
                  [else (values maybe-ch #false)])])))

(define xml-consume-open-token : (-> Input-Port (Values Symbol (Option Char)))
  (lambda [/dev/xmlin]
    (define maybe-ch : (U Char EOF) (read-char /dev/xmlin))
    (cond [(eof-object? maybe-ch) (values '< #false)]
          [else (case maybe-ch
                  [(#\?) (values '<? (xml-read-char /dev/xmlin))]
                  [(#\!) (values '<! (xml-read-char /dev/xmlin))]
                  [else (values '< maybe-ch)])])))

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
  
(define xml-consume-nmtoken : (-> Input-Port Char (Values Symbol (Option Char)))
  ;;; https://www.w3.org/TR/xml11/#sec-common-syn
  ;;; https://www.w3.org/TR/xml11/#NT-Names
  ;;; https://www.w3.org/TR/xml11/#NT-Nmtokens
  (lambda [/dev/xmlin leader]
    (define-values (name maybe-next) (xml-consume-namechars /dev/xmlin leader))
    (values (string->symbol name) maybe-next)))

(define xml-consume-literal-token : (-> Input-Port Char (Values (U String XML-Error) (Option Char)))
  ;;; https://www.w3.org/TR/xml11/#NT-SystemLiteral
  (lambda [/dev/xmlin quote]
    (define literal : (U String XML-Error) (xml-consume-system-literal /dev/xmlin quote))
    (values literal (xml-read-char /dev/xmlin))))

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
(define xml-consume-whitespace : (-> Input-Port Char (Values XML-WhiteSpace (Option Char)))
  ;;; https://www.w3.org/TR/xml11/#NT-S
  ;;; https://www.w3.org/TR/xml11/#sec-line-ends
  (lambda [/dev/xmlin leader]
    ;; NOTE: The clients take responsibility for normalizing the End-of-Lines if required
    (let read-whitespace ([secaps : (Listof Char) (list leader)])
      (define maybe-space : (U Char EOF) (read-char /dev/xmlin))
      (cond [(eof-object? maybe-space) (values (XML-WhiteSpace (reverse secaps)) #false)]
            [(char-whitespace? maybe-space) (read-whitespace (cons maybe-space secaps))]
            [else (values (XML-WhiteSpace (reverse secaps)) maybe-space)]))))
  
(define xml-consume-namechars : (-> Input-Port Char (Values String (Option Char)))
  ;;; https://www.w3.org/TR/xml11/#NT-NameChar
  (lambda [/dev/xmlin leader]
    (let consume-name ([srahc : (Listof Char) (list leader)])
      (define ch : (U EOF Char) (read-char /dev/xmlin))
      (cond [(eof-object? ch) (values (list->string (reverse srahc)) #false)]
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
(define xml-read-char : (-> Input-Port (Option Char))
  (lambda [/dev/xmlin]
    (define maybe-next : (U Char EOF) (read-char /dev/xmlin))
    (and (char? maybe-next) maybe-next)))

(define xml-consume-error-literal : (-> Input-Port Char (Listof Char) (U String XML-Error))
  (lambda [/dev/xmlin quote chars]
    (let consume-literal ([srahc : (Listof Char) chars])
      (define ch : (U EOF Char) (read-char /dev/xmlin))
      (cond [(or (eq? ch quote) (eof-object? ch)) (reverse srahc)]
            [else (consume-literal (cons ch srahc))]))))

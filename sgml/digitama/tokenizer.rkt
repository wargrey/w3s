#lang typed/racket/base

;;; https://drafts.xmlwg.org/xml-syntax/#tokenization

(provide (all-defined-out))

(require "digicore.rkt")
(require "misc.rkt")

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
    [(_ src make-xml:token datum ...)
     #'(let-values ([(line column here-position) (port-next-location (xml-srcloc-in src))])
         (make-xml:token (xml-srcloc-source src) (xml-srcloc-line src) (xml-srcloc-column src)
                         (xml-srcloc-position src) here-position datum ...))]))
  
(define-syntax (xml-make-bad-token stx)
  (syntax-case stx []
    [(_ src xml:bad:sub token datum)
     #'(let ([bad (xml-make-token src xml:bad:sub (~s (cons (object-name token) datum)))])
         (xml-log-read-error (xml-token->string bad))
         bad)]))

(define xml-consume-token : (-> Input-Port (U String Symbol) (U EOF XML-Token))
  ;;; https://drafts.xmlwg.org/xml-syntax/#error-handling
  ;;; https://drafts.xmlwg.org/xml-syntax/#consume-a-token
  (lambda [/dev/xmlin source]
    (define-values (line column position) (port-next-location /dev/xmlin))
    (define srcloc (xml-srcloc /dev/xmlin source line column position))
    (define ch (read-char /dev/xmlin))
    (cond [(eof-object? ch) eof]
          [(char-whitespace? ch) (xml-consume-whitespace-token srcloc)]
          [(xml-name-start-char? ch) (xml-consume-nmtoken srcloc ch)]
          [else (case ch
                  [(#\( #\[ #\{) (xml-make-token srcloc xml:open ch)]
                  [(#\) #\] #\}) (xml-make-token srcloc xml:close ch)]
                  [(#\' #\") (xml-consume-string-token srcloc ch)]
                  [(#\< #\-) (xml-consume-cd-token srcloc ch)]
                  [(#\\) (xml-consume-escaped-ident-token srcloc)]
                  [(#\null) (xml-make-token srcloc xml:delim #\uFFFD)]
                  [(#\?) (xml-make-token srcloc xml:question #\?)]
                  [(#\!) (xml-make-token srcloc xml:exclamation #\!)]
                  [else (xml-make-token srcloc xml:delim ch)])])))

(define xml-consume-cd-token : (-> XML-Srcloc Char XML-Token)
  ;;; https://drafts.xmlwg.org/xml-syntax/#CDO-token-diagram
  ;;; https://drafts.xmlwg.org/xml-syntax/#CDC-token-diagram
  (lambda [srcloc open/close]
    (define xml : Input-Port (xml-srcloc-in srcloc))
    (if (char=? open/close #\<)
        (let ([cdo : (U EOF String) (peek-string 3 0 xml)])
          (cond [(and (string? cdo) (string=? cdo "!--")) (read-string 3 xml) (xml-make-token srcloc xml:cdo '<!--)]
                [else (xml-make-token srcloc xml:delim #\<)]))
        (let ([cdc : (U EOF String) (peek-string 2 0 xml)])
          (cond [(eof-object? cdc) (xml-make-token srcloc xml:delim #\-)]
                [(string=? cdc "->") (read-string 2 xml) (xml-make-token srcloc xml:cdc '-->)]
                [else (xml-consume-nmtoken srcloc #\-)])))))

(define xml-consume-whitespace-token : (-> XML-Srcloc XML:WhiteSpace)
  (lambda [srcloc]
    (define xml : Input-Port (xml-srcloc-in srcloc))
    (xml-consume-whitespace xml)
    (xml-make-token srcloc xml:whitespace #\space)))
  
(define xml-consume-nmtoken : (-> XML-Srcloc Char (U XML:Name XML:Bad))
  ;;; https://www.w3.org/TR/xml11/#sec-common-syn
  ;;; https://www.w3.org/TR/xml11/#NT-Names
  (lambda [srcloc id0]
    (define xml : Input-Port (xml-srcloc-in srcloc))
    (define name : String (xml-consume-name xml id0))
    (xml-make-token srcloc xml:name (string->symbol name))))

(define xml-consume-escaped-ident-token : (-> XML-Srcloc (U XML:Name XML:Delim XML:Bad))
  ;;; https://drafts.xmlwg.org/xml-syntax/#consume-token (when #\\ is found at the beginning of a non-whitespace token)
  (lambda [srcloc]
    (define xml : Input-Port (xml-srcloc-in srcloc))
    (if (xml-valid-escape? #\\ (peek-char xml 1))
        (xml-consume-nmtoken srcloc (xml-consume-escaped-char xml))
        (xml-remake-token (xml-make-bad-token srcloc xml:bad:char struct:xml:delim #\\) xml:delim #\\))))

(define xml-consume-string-token : (-> XML-Srcloc Char (U XML:String XML:Bad))
  ;;; https://drafts.xmlwg.org/xml-syntax/#consume-a-string-token
  (lambda [srcloc quotation]
    (define xml : Input-Port (xml-srcloc-in srcloc))
    (let consume-string-token : (U XML:String XML:Bad) ([chars : (Listof Char) null])
      (define ch : (U EOF Char) (read-char xml))
      (cond [(or (eof-object? ch) (char=? ch quotation))
             (when (eof-object? ch) (xml-make-bad-token srcloc xml:bad:eof struct:xml:string (list->string (reverse chars))))
             (xml-make-token srcloc xml:string (list->string (reverse chars)))]
            [(char=? ch #\newline) (xml-make-bad-token srcloc xml:bad:eol struct:xml:string (list->string (reverse chars)))]
            [(not (char=? ch #\\)) (consume-string-token (cons ch chars))]
            [else (let ([next (peek-char xml)])
                    (cond [(eof-object? next) (consume-string-token chars)]
                          [(and (char=? next #\newline) (read-char xml)) (consume-string-token (cons ch chars))]
                          [else (consume-string-token (cons (xml-consume-escaped-char xml) chars))]))]))))

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
(define xml-consume-whitespace : (-> Input-Port Void)
  (lambda [xml]
    (regexp-match #px"\\s*" xml)
    (void)))
  
(define xml-consume-name : (-> Input-Port Char String)
  ;;; https://drafts.xmlwg.org/xml-syntax/#consume-a-name
  (lambda [xml head]
    (let consume-name ([srahc : (Listof Char) (list head)])
      (define ch : (U EOF Char) (peek-char xml))
      (cond [(and (char? ch) (xml-name-char? ch) (read-char xml)) (consume-name (cons ch srahc))]
            [else (list->string (reverse srahc))]))))

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

(define xml-consume-bad-url-remnants : (-> Input-Port XML:Bad XML:Bad)
  ;;; https://drafts.xmlwg.org/xml-syntax/#consume-the-remnants-of-a-bad-url
  (lambda [xml bad-url-token]
    (define ch : (U EOF Char) (read-char xml))
    (cond [(or (eof-object? ch) (char=? ch #\))) bad-url-token]
          [(and (char=? ch #\\) (read-char xml)) (xml-consume-bad-url-remnants xml bad-url-token)]
          [else (xml-consume-bad-url-remnants xml bad-url-token)])))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define char-hexdigit? : (-> (U EOF Char) Boolean : #:+ Char)
  (lambda [ch]
    (and (char? ch)
         (or (char-numeric? ch)
             (char-ci<=? #\a ch #\f)))))

(define char->hexadecimal : (-> Char Fixnum)
  (lambda [hexch]
    (cond [(char<=? #\a hexch) (fx- (char->integer hexch) #x57)]
          [(char<=? #\A hexch) (fx- (char->integer hexch) #x37)]
          [else (fx- (char->integer hexch) #x30)])))

(define xml-char-non-printable? : (-> (U EOF Char) Boolean : #:+ Char)
  (lambda [ch]
    (and (char? ch)
         (or (char<=? #\null ch #\backspace)
             (char=? ch #\vtab)
             (char<=? #\u000E ch #\u001F)
             (char=? ch #\rubout)))))

(define xml-name-start-char? : (-> Char Boolean)
  (lambda [ch]
    (or (char-alphabetic? ch)
        (eq? ch #\:)
        (eq? ch #\_)
        (char<=? #\u00C0 ch #\u00D6)
        (char<=? #\u00D8 ch #\u00F6)
        (char<=? #\u00F8 ch #\u02FF)
        (char<=? #\u0370 ch #\u037D)
        (char<=? #\u037F ch #\u1FFF)
        (char<=? #\u200C ch #\u200D)
        (char<=? #\u2070 ch #\u218F)
        (char<=? #\u2C00 ch #\u2FEF)
        (char<=? #\u3001 ch #\uD7FF)
        (char<=? #\uF900 ch #\uFDCF)
        (char<=? #\uFDF0 ch #\uFFFD)
        (char<=? #\U10000 ch #\UEFFFF))))

(define xml-name-char? : (-> Char Boolean)
  (lambda [ch]
    (or (xml-name-start-char? ch)
        (char-numeric? ch)
        (eq? ch #\-)
        (eq? ch #\.)
        (eq? ch #xB7)
        (char<=? #\u0300 ch #\u036F)
        (char<=? #\u203F ch #\u2040))))
  
(define xml-valid-escape? : (-> (U EOF Char) (U EOF Char) Boolean : #:+ Char)
  ;;; https://drafts.xmlwg.org/xml-syntax/#escaping
  ;;; https://drafts.xmlwg.org/xml-syntax/#starts-with-a-valid-escape
  (lambda [ch1 ch2]
    (and (char? ch1)
         (char=? ch1 #\\)
         (or (eof-object? ch2)
             (not (char=? ch2 #\newline))))))

(define xml-number-prefix? : (-> (U EOF Char) (U EOF Char) (U EOF Char) Boolean : #:+ Char)
  ;;; https://drafts.xmlwg.org/xml-syntax/#starts-with-a-number
  (lambda [ch1 ch2 ch3]
    (or (and (char? ch1) (char<=? #\0 ch1 #\9))
        (and (char? ch1) (char? ch2)
             (char=? ch1 #\.) (char<=? #\0 ch2 #\9))
        (and (char? ch1) (char? ch2)
             (or (char=? ch1 #\+) (char=? ch1 #\-))
             (or (char<=? #\0 ch2 #\9)
                 (and (char=? ch2 #\.)
                      (char? ch3) (char<=? #\0 ch3 #\9)))))))

(define xml-scientific-notation? : (-> (U EOF Char) (U EOF Char) (U EOF Char) Boolean : #:+ Char)
  ;;; https://drafts.xmlwg.org/xml-syntax/#consume-a-number
  (lambda [ch1 ch2 ch3]
    (and (char? ch1) (char? ch2)
         (char-ci=? ch1 #\e)
         (or (char<=? #\0 ch2 #\9)
             (and (or (char=? ch2 #\+) (char=? ch2 #\-))
                  (char? ch3)
                  (char<=? #\0 ch3 #\9))))))

(define xml-decimal-point? : (-> (U EOF Char) (U EOF Char) Boolean : #:+ Char)
  ;;; https://drafts.xmlwg.org/xml-syntax/#consume-a-number
  (lambda [ch1 ch2]
    (and (char? ch1) (char? ch2)
         (char=? ch1 #\.)
         (char<=? #\0 ch2 #\9))))

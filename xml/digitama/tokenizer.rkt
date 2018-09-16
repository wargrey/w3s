#lang typed/racket/base

;;; https://drafts.xmlwg.org/xml-syntax/#tokenization

(provide (all-defined-out))

(require "digicore.rkt")
(require "misc.rkt")

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
  #:type-name CSS-Srcloc)

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

(define xml-consume-token : (-> Input-Port (U String Symbol) (U EOF CSS-Token))
  ;;; https://drafts.xmlwg.org/xml-syntax/#error-handling
  ;;; https://drafts.xmlwg.org/xml-syntax/#consume-a-token
  (lambda [/dev/xmlin source]
    (define-values (line column position) (port-next-location /dev/xmlin))
    (define srcloc (xml-srcloc /dev/xmlin source line column position))
    (define ch (read-char /dev/xmlin))
    (cond [(eof-object? ch) eof]
          [(char-whitespace? ch) (xml-consume-whitespace-token srcloc)]
          [(char-numeric? ch) (xml-consume-numeric-token srcloc ch)]
          [(xml-char-name-prefix? ch) (xml-consume-ident-token srcloc ch)]
          [else (case ch
                  [(#\( #\[ #\{) (xml-make-token srcloc xml:open ch)]
                  [(#\) #\] #\}) (xml-make-token srcloc xml:close ch)]
                  [(#\' #\") (xml-consume-string-token srcloc ch)]
                  [(#\+ #\.) (xml-consume-numeric-token srcloc ch)]
                  [(#\^ #\$ #\| #\~ #\*) (xml-consume-match-token srcloc ch)]
                  [(#\#) (xml-consume-hash-token srcloc)]
                  [(#\@) (xml-consume-@keyword-token srcloc)]
                  [(#\/) (xml-consume-comment-token srcloc)]
                  [(#\< #\-) (xml-consume-cd-token srcloc ch)]
                  [(#\;) (xml-make-token srcloc xml:semicolon #\;)]
                  [(#\,) (xml-make-token srcloc xml:comma #\,)]
                  [(#\:) (xml-make-token srcloc xml:colon #\:)]
                  [(#\\) (xml-consume-escaped-ident-token srcloc)]
                  [(#\null) (xml-make-token srcloc xml:delim #\uFFFD)]
                  [else (xml-make-token srcloc xml:delim ch)])])))

(define xml-consume-cd-token : (-> CSS-Srcloc Char CSS-Token)
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
                [(xml-identifier-prefix? #\- (string-ref cdc 0) (string-ref cdc 1)) (xml-consume-ident-token srcloc #\-)]
                [else (xml-consume-numeric-token srcloc #\-)])))))

(define xml-consume-comment-token : (-> CSS-Srcloc (U CSS:WhiteSpace CSS:Delim CSS:Bad))
  (lambda [srcloc]
    (define xml : Input-Port (xml-srcloc-in srcloc))
    (define ch1 : (U EOF Char) (peek-char xml 0))
    (cond [(or (eof-object? ch1) (not (char=? ch1 #\*))) (xml-make-token srcloc xml:slash #\/)]
          [(regexp-match #px".*?((\\*/)|$)" xml) => (λ [**/] (xml-make-token srcloc xml:whitespace (format "/~a" (car **/))))]
          [else (xml-make-bad-token srcloc xml:bad:eof struct:xml:whitespace "/*")])))

(define xml-consume-whitespace-token : (-> CSS-Srcloc CSS:WhiteSpace)
  (lambda [srcloc]
    (define xml : Input-Port (xml-srcloc-in srcloc))
    (xml-consume-whitespace xml)
    (xml-make-token srcloc xml:whitespace #\space)))
  
(define xml-consume-ident-token : (-> CSS-Srcloc Char (U CSS:Ident CSS:Function CSS:URL CSS:URange CSS:Bad))
  ;;; https://drafts.xmlwg.org/xml-syntax/#consume-an-ident-like-token
  ;;; https://drafts.xmlwg.org/xml-syntax/#urange-syntax
  ;;; https://drafts.xmlwg.org/xml-values/#urls
  ;;; https://drafts.xmlwg.org/xml-variables/#defining-variables
  (lambda [srcloc id0]
    (define xml : Input-Port (xml-srcloc-in srcloc))
    (define ch1 : (U EOF Char) (peek-char xml 0))
    (define ch2 : (U EOF Char) (peek-char xml 1))
    (cond [(and (char-ci=? id0 #\u) (char? ch1) (char=? ch1 #\+)
                (or (char-hexdigit? ch2) (and (char? ch2) (char=? ch2 #\?))))
           (read-char xml) (xml-consume-unicode-range-token srcloc)]
          [else (let ([name (xml-consume-name xml id0)])
                  (define ch : (U EOF Char) (peek-char xml))
                  (cond [(or (eof-object? ch) (not (char=? ch #\()))
                         (if (and (char=? id0 #\-) (eqv? id0 ch1))
                             (let ([--id (string->unreadable-symbol name)]) (xml-make-token srcloc xml:ident --id --id))
                             (xml-make-token srcloc xml:ident (string->symbol name) (string->symbol (string-downcase name))))]
                        [(and (or (not (string-ci=? name "url")) (regexp-match-peek #px"^.\\s*[\"']" xml)) (read-char xml))
                         (define fnorm : Symbol (string->symbol (string-downcase name)))
                         (xml-make-token srcloc xml:function (string->unreadable-symbol name) fnorm null #false)]
                        [else (read-char xml) (xml-consume-url-token srcloc)]))])))

(define xml-consume-escaped-ident-token : (-> CSS-Srcloc (U CSS:Ident CSS:Delim CSS:Function CSS:URL CSS:URange CSS:Bad))
  ;;; https://drafts.xmlwg.org/xml-syntax/#consume-token (when #\\ is found at the beginning of a non-whitespace token)
  (lambda [srcloc]
    (define xml : Input-Port (xml-srcloc-in srcloc))
    (if (xml-valid-escape? #\\ (peek-char xml 1))
        (xml-consume-ident-token srcloc (xml-consume-escaped-char xml))
        (xml-remake-token (xml-make-bad-token srcloc xml:bad:char struct:xml:delim #\\) xml:delim #\\))))

(define xml-consume-string-token : (-> CSS-Srcloc Char (U CSS:String CSS:Bad))
  ;;; https://drafts.xmlwg.org/xml-syntax/#consume-a-string-token
  (lambda [srcloc quotation]
    (define xml : Input-Port (xml-srcloc-in srcloc))
    (let consume-string-token : (U CSS:String CSS:Bad) ([chars : (Listof Char) null])
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

(define xml-consume-numeric-token : (-> CSS-Srcloc Char (U CSS-Numeric CSS:Delim CSS:Bad))
  ;;; https://drafts.xmlwg.org/xml-syntax/#consume-a-number
  ;;; https://drafts.xmlwg.org/xml-values/#numeric-types
  (lambda [srcloc sign/digit]
    (define xml : Input-Port (xml-srcloc-in srcloc))
    (define ch1 : (U EOF Char) (peek-char xml 0))
    (define ch2 : (U EOF Char) (peek-char xml 1))
    (cond [(not (xml-number-prefix? sign/digit ch1 ch2)) (xml-make-token srcloc xml:delim sign/digit)]
          [else (let-values ([(n representation) (xml-consume-number xml sign/digit)])
                  (let ([ch1 : (U EOF Char) (peek-char xml 0)]
                        [ch2 : (U EOF Char) (peek-char xml 1)]
                        [ch3 : (U EOF Char) (peek-char xml 2)])
                    (cond [(and (char? ch1) (char=? ch1 #\%) (read-char xml))
                           (define n% : Single-Flonum (real->single-flonum (* n 0.01)))
                           (xml-make-token srcloc xml:percentage (string-append representation "%") n%)]
                          [(flonum? n)
                           (cond [(zero? n) (xml-make-token srcloc xml:flzero representation n)]
                                 [(fl= n 1.0) (xml-make-token srcloc xml:flone representation n)]
                                 [else (xml-make-token srcloc xml:flonum representation n)])]
                          [(zero? n) (xml-make-token srcloc xml:zero representation n)]
                          [(= n 1) (xml-make-token srcloc xml:one representation n)]
                          [else (xml-make-token srcloc xml:integer representation n)])))])))

(define xml-consume-url-token : (-> CSS-Srcloc (U CSS:URL CSS:Bad))
  ;;; https://drafts.xmlwg.org/xml-syntax/#consume-a-url-token
  ;;; https://drafts.xmlwg.org/xml-values/#urls
  ;;; https://drafts.xmlwg.org/xml-values/#url-empty
  ;;; https://drafts.xmlwg.org/xml-values/#about-invalid
  (lambda [srcloc]
    (define xml : Input-Port (xml-srcloc-in srcloc))
    (xml-consume-whitespace xml)
    (let consume-url-token ([srahc : (Listof Char) null])
      (define ch : (U EOF Char) (read-char xml))
      (cond [(or (eof-object? ch) (char=? ch #\)))
             (define uri : String (list->string (reverse srahc)))
             (when (eof-object? ch) (xml-make-bad-token srcloc xml:bad:eof struct:xml:url uri))
             (xml-make-token srcloc xml:url uri null #false)]
            [(and (char-whitespace? ch) (xml-consume-whitespace xml))
             (define end : (U EOF Char) (read-char xml))
             (define uri : String (list->string (reverse srahc)))
             (cond [(or (eof-object? end) (char=? end #\))) (xml-make-token srcloc xml:url uri null #false)]
                   [else (xml-consume-bad-url-remnants xml (xml-make-bad-token srcloc xml:bad:blank struct:xml:url uri))])]
            [(xml-valid-escape? ch (peek-char xml)) (consume-url-token (cons (xml-consume-escaped-char xml) srahc))]
            [(or (memq ch '(#\\ #\" #\' #\()) (xml-char-non-printable? ch))
             (xml-consume-bad-url-remnants xml (xml-make-bad-token srcloc xml:bad:char struct:xml:url ch))]
            [else (consume-url-token (cons ch srahc))]))))

(define xml-consume-unicode-range-token : (-> CSS-Srcloc (U CSS:URange CSS:Bad))
  ;;; https://drafts.xmlwg.org/xml-syntax/#urange-syntax
  (lambda [srcloc]
    (define xml : Input-Port (xml-srcloc-in srcloc))
    (define-values (n rest) (xml-consume-hexadecimal (xml-srcloc-in srcloc) 6))
    (define-values (start0 end0)
      (let consume-? : (Values Fixnum Fixnum) ([s : Fixnum n] [e : Fixnum n] [? : Fixnum rest])
        (cond [(zero? ?) (values s e)]
              [else (let ([ch : (U EOF Char) (peek-char xml)])
                      (cond [(or (eof-object? ch) (not (char=? ch #\?))) (values s e)]
                            [else (read-char xml) (consume-? (fxlshift s 4)
                                                             (fxior (fxlshift e 4) #xF)
                                                             (fx- ? 1))]))])))
    (define-values (start end)
      (cond [(not (fx= start0 end0)) (values start0 end0)]
            [else (let ([ch1 (peek-char xml 0)]
                        [ch2 (peek-char xml 1)])
                    (cond [(and (char? ch1) (char=? ch1 #\-) (char-hexdigit? ch2) (read-char xml))
                           (define-values (end _) (xml-consume-hexadecimal (xml-srcloc-in srcloc) 6))
                           (values start0 end)]
                          [else (values start0 start0)]))]))
    (cond [(and (index? start) (index? end) (<= start end #x10FFFF)) (xml-make-token srcloc xml:urange (cons start end))]
          [(> end #x10FFFF) (xml-make-bad-token srcloc xml:bad:range struct:xml:urange end)]
          [else (xml-make-bad-token srcloc xml:bad:range struct:xml:urange (cons start end))])))

(define xml-consume-hash-token : (-> CSS-Srcloc (U CSS:Hash CSS:Delim))
  ;;; https://drafts.xmlwg.org/xml-syntax/#hash-token-diagram
  (lambda [srcloc]
    (define xml : Input-Port (xml-srcloc-in srcloc))
    (define ch1 : (U EOF Char) (peek-char xml 0))
    (define ch2 : (U EOF Char) (peek-char xml 1))
    (define ch3 : (U EOF Char) (peek-char xml 2))
    (if (or (xml-char-name? ch1) (xml-valid-escape? ch1 ch2))
        (let ([name (xml-consume-name (xml-srcloc-in srcloc) #false)])
          (xml-make-token srcloc xml:hash (string->keyword name) #|(string->keyword (string-downcase name))|#))
        (xml-make-token srcloc xml:delim #\#))))

(define xml-consume-@keyword-token : (-> CSS-Srcloc (U CSS:@Keyword CSS:Delim))
  ;;; https://drafts.xmlwg.org/xml-syntax/#at-keyword-token-diagram
  (lambda [srcloc]
    (define xml : Input-Port (xml-srcloc-in srcloc))
    (define ch1 : (U EOF Char) (peek-char xml 0))
    (define ch2 : (U EOF Char) (peek-char xml 1))
    (define ch3 : (U EOF Char) (peek-char xml 2))
    (if (xml-identifier-prefix? ch1 ch2 ch3)
        (let ([name (xml-consume-name (xml-srcloc-in srcloc) #\@)])
          (xml-make-token srcloc xml:@keyword (string->keyword name) (string->keyword (string-downcase name))))
        (xml-make-token srcloc xml:delim #\@))))

(define xml-consume-match-token : (-> CSS-Srcloc Char (U CSS:Match CSS:Delim))
  ;;; https://drafts.xmlwg.org/xml-syntax/#include-match-token-diagram
  ;;; https://drafts.xmlwg.org/xml-syntax/#column-token-diagram
  (lambda [srcloc prefix]
    (define xml : Input-Port (xml-srcloc-in srcloc))
    (define ch : (U EOF Char) (peek-char xml))
    (cond [(and (eq? prefix #\|) (eq? ch #\|) (read-char xml)) (xml-make-token srcloc xml:delim #\tab)]
          [(and (eq? ch #\=) (read-char xml)) (xml-make-token srcloc xml:match prefix)]
          [(eq? prefix #\|) (xml-make-token srcloc xml:vbar prefix)]
          [else (xml-make-token srcloc xml:delim prefix)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml-consume-whitespace : (-> Input-Port Void)
  (lambda [xml]
    (regexp-match #px"\\s*" xml)
    (void)))
  
(define xml-consume-name : (-> Input-Port (Option Char) String)
  ;;; https://drafts.xmlwg.org/xml-syntax/#consume-a-name
  (lambda [xml ?head]
    (let consume-name ([srahc : (Listof Char) (if ?head (list ?head) null)])
      (define ch : (U EOF Char) (peek-char xml))
      (cond [(and (xml-char-name? ch) (read-char xml)) (consume-name (cons ch srahc))]
            [(and (xml-valid-escape? ch (peek-char xml 1)) (read-char xml)) (consume-name (cons (xml-consume-escaped-char xml) srahc))]
            [else (list->string (reverse srahc))]))))

(define xml-consume-number : (-> Input-Port Char (Values (U Flonum Integer) String))
  ;;; https://drafts.xmlwg.org/xml-syntax/#consume-a-number
  (lambda [xml sign/digit]
    (let consume-number ([chars (list sign/digit)])
      (define ch : (U EOF Char) (peek-char xml))
      (cond [(and (char? ch)
                  (or (char-numeric? ch)
                      (char=? ch #\+) (char=? ch #\-)
                      (xml-decimal-point? ch (peek-char xml 1))
                      (xml-scientific-notation? ch (peek-char xml 1) (peek-char xml 2)))
                  (read-char xml))
             (consume-number (cons ch chars))]
            [else (let* ([representation : String (list->string (reverse chars))]
                         [?number : (Option Complex) (string->number representation)])
                    (cond [(exact-integer? ?number) (values ?number representation)]
                          [(flonum? ?number) (values ?number representation)]
                          [else (values +nan.0 representation)]))]))))

(define xml-consume-hexadecimal : (->* (Input-Port Byte) (Fixnum #:\s?$? Boolean) (Values Fixnum Byte))
  (lambda [xml --count [result 0] #:\s?$? [eat-last-whitespace? #false]]
    (define hex : (U EOF Char) (peek-char xml))
    (cond [(or (eof-object? hex) (not (char-hexdigit? hex)) (zero? --count))
           (when (and eat-last-whitespace? (char? hex) (char-whitespace? hex)) (read-char xml))
           (values result --count)]
          [else (read-char xml) (xml-consume-hexadecimal #:\s?$? eat-last-whitespace?
                                                         xml (fx- --count 1)
                                                         (fx+ (fxlshift result 4)
                                                              (char->hexadecimal hex)))])))

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

(define xml-consume-bad-url-remnants : (-> Input-Port CSS:Bad CSS:Bad)
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

(define xml-char-name-prefix? : (-> (U EOF Char) Boolean : #:+ Char)
  (lambda [ch]
    (and (char? ch)
         (or (char-lower-case? ch)
             (char-upper-case? ch)
             (char=? #\_  ch)
             (char>=? ch #\u0080)))))

(define xml-char-name? : (-> (U EOF Char) Boolean : #:+ Char)
  (lambda [ch]
    (and (char? ch)
         (or (xml-char-name-prefix? ch)
             (char-numeric? ch)
             (char=? #\- ch)))))
  
(define xml-valid-escape? : (-> (U EOF Char) (U EOF Char) Boolean : #:+ Char)
  ;;; https://drafts.xmlwg.org/xml-syntax/#escaping
  ;;; https://drafts.xmlwg.org/xml-syntax/#starts-with-a-valid-escape
  (lambda [ch1 ch2]
    (and (char? ch1)
         (char=? ch1 #\\)
         (or (eof-object? ch2)
             (not (char=? ch2 #\newline))))))

(define xml-identifier-prefix? : (-> (U EOF Char) (U EOF Char) (U EOF Char) Boolean : #:+ Char)
  ;;; https://drafts.xmlwg.org/xml-syntax/#would-start-an-identifier
  (lambda [ch1 ch2 ch3]
    (or (xml-char-name-prefix? ch1)
        (xml-valid-escape? ch1 ch2)
        (and (char? ch1) (char=? ch1 #\-)
             (or (xml-char-name-prefix? ch2)
                 (and (char? ch2) (char=? ch2 #\-))
                 (xml-valid-escape? ch2 ch3))))))

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

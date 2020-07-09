#lang typed/racket/base

;;; https://www.w3.org/TR/xml11/#charsets

(provide (all-defined-out))

(define char-hexdigit? : (-> Char Boolean)
  (lambda [ch]
    (or (char-numeric? ch)
        (char-ci<=? #\a ch #\f))))

(define char->hexadecimal : (-> Char Fixnum)
  (lambda [hexch]
    (cond [(char<=? #\a hexch) (- (char->integer hexch) #x57)]
          [(char<=? #\A hexch) (- (char->integer hexch) #x37)]
          [else (- (char->integer hexch) #x30)])))

(define natural->char-entity : (-> Fixnum Index)
  (lambda [n]
    (cond [(and (<= #x1 n) (<= n #xD7FF)) n]
          [(and (<= #xE000 n) (<= n #xFFFD)) n]
          [(and (<= #x10000 n) (<= n #x10FFFF)) n]
          [else #xFFFD])))

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


(define xml-pubid-char? : (-> Char Boolean)
  (lambda [ch]
    (or (char-alphabetic? ch)
        (char-numeric? ch)
        (and (memq ch (list #\space #\newline #\return
                            #\- #\' #\( #\) #\+ #\, #\. #\/ #\:
                            #\= #\? #\; #\! #\* #\# #\@ #\$ #\_ #\%))
             #true))))

(define xml-valid-escape? : (-> (U EOF Char) (U EOF Char) Boolean : #:+ Char)
  (lambda [ch1 ch2]
    (and (char? ch1)
         (char=? ch1 #\\)
         (or (eof-object? ch2)
             (not (char=? ch2 #\newline))))))

(define xml-number-prefix? : (-> (U EOF Char) (U EOF Char) (U EOF Char) Boolean : #:+ Char)
  (lambda [ch1 ch2 ch3]
    (or (and (char? ch1) (char<=? #\0 ch1 #\9))
        (and (char? ch1) (char? ch2)
             (char=? ch1 #\.) (char<=? #\0 ch2 #\9))
        (and (char? ch1) (char? ch2)
             (or (char=? ch1 #\+) (char=? ch1 #\-))
             (or (char<=? #\0 ch2 #\9)
                 (and (char=? ch2 #\.)
                      (char? ch3) (char<=? #\0 ch3 #\9)))))))

(define xml-decimal-point? : (-> (U EOF Char) (U EOF Char) Boolean : #:+ Char)
  (lambda [ch1 ch2]
    (and (char? ch1) (char? ch2)
         (char=? ch1 #\.)
         (char<=? #\0 ch2 #\9))))

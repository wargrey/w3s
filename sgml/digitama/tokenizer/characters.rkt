#lang typed/racket/base

;;; https://www.w3.org/TR/xml/#charsets

(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define char-hexdigit? : (-> Char Boolean)
  (lambda [ch]
    (or (char-numeric? ch)
        (char-ci<=? #\a ch #\f))))

(define char->hexadecimal : (-> Char Fixnum)
  (lambda [hexch]
    (cond [(char<=? #\a hexch) (- (char->integer hexch) #x57)]
          [(char<=? #\A hexch) (- (char->integer hexch) #x37)]
          [else (- (char->integer hexch) #x30)])))

(define natural->char-entity : (-> Fixnum (Option Index))
  (lambda [n]
    (and (or (= n #x9)
             (= n #xA)
             (= n #xD)
             (and (<= #x20 n) (<= n #xD7FF))
             (and (<= #xE000 n) (<= n #xFFFD))
             (and (<= #x10000 n) (<= n #x10FFFF)))
         n)))

(define xml-newline-char? : (-> Char Boolean)
  (lambda [ch]
    (or (eq? ch #\newline)
        (eq? ch #\return))))

(define xml-name-start-char? : (-> Char Boolean)
  (lambda [ch]
    (or (char-alphabetic? ch)
        (eq? ch #\:)
        (eq? ch #\_)
        (xml-name-start-other-char? ch))))

(define xml-name-char? : (-> Char Boolean)
  (lambda [ch]
    (or (char-alphabetic? ch)
        (eq? ch #\:)
        (eq? ch #\-)
        (eq? ch #\.)
        (eq? ch #\_)
        (char-numeric? ch)
        (eq? ch #xB7)
        (xml-name-start-other-char? ch)
        (char<=? #\u0300 ch #\u036F)
        (char<=? #\u203F ch #\u2040))))

(define xml-content-char? : (-> Char Boolean)
  (lambda [ch]
    (not (or (char-whitespace? ch)
             (eq? ch #\<)
             (eq? ch #\&)
             (eq? ch #\%)))))

(define xml-pubid-char/no-spaces? : (-> Char Boolean)
  (lambda [ch]
    (or (char<=? #\a ch #\z)
        (char<=? #\A ch #\Z)
        (char-numeric? ch)
        (and (memq ch (list #\- #\' #\( #\) #\+ #\, #\. #\/ #\:
                            #\= #\? #\; #\! #\* #\# #\@ #\$ #\_ #\%))
             #true))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml-name-start-other-char? : (-> Char Boolean)
  (lambda [ch]
    (or (char<=? #\u00C0 ch #\u00D6)
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

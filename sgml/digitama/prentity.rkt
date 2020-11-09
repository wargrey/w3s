#lang typed/racket/base

(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define &lt : Symbol (string->unreadable-symbol "lt"))
(define &gt : Symbol (string->unreadable-symbol "gt"))
(define &amp : Symbol (string->unreadable-symbol "amp"))
(define &apos : Symbol (string->unreadable-symbol "apos"))
(define &quot : Symbol (string->unreadable-symbol "quot"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml-prentity-value-ref : (-> Symbol (Option Char))
  ;;; https://www.w3.org/TR/xml/#sec-predefined-ent
  (lambda [name]
    (cond [(eq? name &lt) #\u3c]
          [(eq? name &gt) #\u3e]
          [(eq? name &amp) #\u26]
          [(eq? name &apos) #\u27]
          [(eq? name &quot) #\u22]
          [else #false])))

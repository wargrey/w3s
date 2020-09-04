#lang typed/racket/base

(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define &lt : Symbol (string->unreadable-symbol "lt"))
(define &gt : Symbol (string->unreadable-symbol "gt"))
(define &amp : Symbol (string->unreadable-symbol "amp"))
(define &apos : Symbol (string->unreadable-symbol "apos"))
(define &quot : Symbol (string->unreadable-symbol "quot"))

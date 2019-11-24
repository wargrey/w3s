#lang typed/racket/base

(provide (all-defined-out))

;;; char->integer
; #\<: #x3C
; #\>: #x3E
; #\/: #x2F
; #\!: #x21
; #\?: #x3F


(define <! : Char (integer->char #x3C21))
(define !> : Char (integer->char #x213E))
(define <? : Char (integer->char #x3C3F))
(define ?> : Char (integer->char #x3F3E))
(define </ : Char (integer->char #x2C2F))
(define /> : Char (integer->char #x2F3E))

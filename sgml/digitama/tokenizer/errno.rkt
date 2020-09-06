#lang typed/racket/base

(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define !eof : Symbol 'exn:xml:read:eof)
(define !name : Symbol 'exn:xml:read:name)
(define !space : Symbol 'exn:xml:read:space)
(define !value : Symbol 'exn:xml:read:value)
(define !char : Symbol 'exn:xml:read:badchar)
(define !comment : Symbol 'exn:xml:read:comment)

#lang typed/racket/base

(provide (all-defined-out) XML-Token)

(require "digicore.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml-tokens-extract : (-> (Listof XML-Token) (Values (Listof XML-Token) (Listof XML-Token)))
  (lambda [tokens]
    (values tokens tokens)))

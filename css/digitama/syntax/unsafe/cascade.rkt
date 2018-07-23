#lang racket/base

(provide (all-defined-out))

(define css-filter?
  (lambda [f]
    (and (procedure? f)
         (procedure-arity-includes? f 1))))


(define css-parser?
  (lambda [f]
    (and (procedure? f)
         (procedure-arity-includes? f 2))))

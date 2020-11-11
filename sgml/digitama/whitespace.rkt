#lang typed/racket/base

(provide (all-defined-out))

(require typed/racket/unsafe)

(unsafe-require/typed
 racket/unsafe/ops ; only works for Latin-1 Strings
 [unsafe-string-ref (-> String Index Char)])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-type XML-Space-Position (U 'head 'body 'tail 'span))

(define-type XML:Space-Filter
  (case-> [Symbol (Option String) Char -> (Option Char)]
          [Symbol (Option String) String (Option String) XML-Space-Position Boolean -> (Option String)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define svg:space-filter : XML:Space-Filter
  (case-lambda
    [(tag xml:lang ch) #\space]
    [(tag xml:lang raw-spaces default-replace position has-newline?)
     (cond [(not has-newline?) default-replace]
           [else (and (eq? position 'body)
                      (let ([size (string-length raw-spaces)])
                        (let svg-check-space ([idx : Nonnegative-Fixnum 0])
                          (and (< idx size)
                               (if (eq? (unsafe-string-ref raw-spaces idx) #\linefeed)
                                   (svg-check-space (+ idx 1))
                                   default-replace)))))])]))

#lang typed/racket/base

(provide (all-defined-out))
(provide xexpr? Xexpr Xexpr-AttList write-xexpr xexpr->bytes xexpr->string)

(require "digitama/document.rkt")
(require "digitama/plain/grammar.rkt")
(require "digitama/plain/xexpr.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml-root-xexpr : (-> (U XML-Document XML-Document*) (Option Xexpr))
  (lambda [xml]
    (define cs : (Listof XML-Content)
      (cond [(xml-document? xml) (xml-document-content xml)]
            [else (map xml-content*->datum (xml-document*-content xml))]))

    (findf list? cs)))

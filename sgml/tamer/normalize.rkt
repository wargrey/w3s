#lang typed/racket/base

(require sgml/dtd)
(require racket/symbol)

(require "normalize.xml")
(require "../digitama/dtd.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xml-entity-ref : (-> XML-Type Symbol (Option XML-Entity))
  (lambda [dtype name]
    (hash-ref (xml-type-entities dtype)
              (string->unreadable-symbol (symbol->immutable-string name))
              (λ [] #false))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define n:normalize.xml : XML-Document*
  (time (xml-document*-normalize normalize.xml)))

(define dtype : XML-Type (assert (xml-document*-type n:normalize.xml)))

(xml-type-entities dtype)

(xml-entity-ref dtype 'book)
(xml-entity-ref dtype 'tricky)
(xml-entity-ref dtype 'example)

(xml-document*-elements n:normalize.xml)

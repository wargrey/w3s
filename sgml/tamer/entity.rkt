#lang typed/racket/base

(require sgml/dtd)

(require "entity.xml")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define n:entity.xml : XML-Document*
  (time (xml-document*-normalize entity.xml)))

(define ?type : (Option XML-Type) (xml-document*-type n:entity.xml))

(when (xml-type? ?type)
  (hash-ref (xml-type-entities ?type)
            (string->unreadable-symbol "book")))

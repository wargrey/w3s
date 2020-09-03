#lang typed/racket/base

(require sgml/dtd)

(require "testdoc.xml")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define n:testdoc.xml : XML-Document*
  (time (xml-document*-normalize testdoc.xml)))

(define ?type : XML-Type (assert (xml-document*-type n:testdoc.xml)))


(xml-type-entities ?type)
(hash-ref (xml-type-entities ?type)
          (string->unreadable-symbol "book")
          (λ [] #false))

(xml-document*-elements n:testdoc.xml)

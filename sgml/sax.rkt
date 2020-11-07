#lang typed/racket/base

(provide (all-defined-out))
(provide xml-event-handler? make-xml-event-handler read-xml-datum)
(provide XML-Document-Handler XML-Doctype-Handler XML-PI-Handler)
(provide XML-Element-Handler XML-Attribute-Handler XML-AttrList-Handler)
(provide XML-PCData-Handler XML-Comment-Handler)

(require "digitama/plain/sax.rkt")

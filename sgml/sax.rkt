#lang typed/racket/base

(provide (all-defined-out) read-xml-datum sax-stop-with)
(provide XML-Event-Handlerof xml-event-handler? make-xml-event-handler)
(provide XML-Prolog-Handler XML-Doctype-Handler XML-PI-Handler)
(provide XML-Element-Handler SAX-Attribute-Value)
(provide XML-PCData-Handler XML-Space-Handler XML-GEReference-Handler)
(provide XML-Comment-Handler XML-Event-Handler)

(require "digitama/plain/sax.rkt")
(require "digitama/plain/prompt.rkt")

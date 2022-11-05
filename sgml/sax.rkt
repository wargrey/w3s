#lang typed/racket/base

(provide (all-from-out "digitama/namespace.rkt" "digitama/whitespace.rkt"))
(provide (all-defined-out) load-xml-datum read-xml-datum sax-stop-with)
(provide XML-Event-Handlerof xml-event-handler? make-xml-event-handler)
(provide XML-Prolog-Handler XML-Doctype-Handler XML-PI-Handler)
(provide XML-Element-Handler SAX-Attribute-Value)
(provide XML-PCData-Handler XML-GEReference-Handler)
(provide XML-Comment-Handler XML-Event-Handler)
(provide sax-handler/xml-writer)

(require "digitama/plain/sax.rkt")
(require "digitama/plain/prompt.rkt")
(require "digitama/namespace.rkt")
(require "digitama/whitespace.rkt")

(require "digitama/plain/sax/handler/writer.rkt")

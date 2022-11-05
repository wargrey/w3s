#lang typed/racket/base

(require sgml/sax)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ main
  (require "normalize.txml")

  (load-xml-datum (assert (xml-doc-location normalize.xml) string?) sax-handler/xml-writer))
